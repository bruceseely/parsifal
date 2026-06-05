;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/reference/glang-cl/denotations.lisp
;;;
;;; Marcus's denotations for the grammar language. This file currently
;;; defines enough to handle the vertical-slice test rule
;;;
;;;   {RULE FOO IN BAR [t] --> Activate cpool.}
;;;
;;; meaning: the RULE prefix (and INFIX for AS/NR variants), the
;;; pattern-bracket `[' prefix, the action-sequence `.' infix, the
;;; ACTIVATE prefix, and the necessary delimiters. Helpers shared by
;;; several denotations (get-var-list, parselist-unless, the buildfun-
;;; equivalent inline code) live here too.
;;;
;;; The rest of Marcus's vocabulary (attach, drop, label, transfer,
;;; case-rules, ...) is deliberately left for later slices.

(in-package #:glang-cl)


;;; -------------------------------------------------------------------
;;; Specials carried across denotations
;;; -------------------------------------------------------------------

(defvar *nodevarlist* nil
  "Stack of (var index sublist) triples, one per pattern position.
   Bound by BUILD-RULE; popped by BUILD-PATTERN-ELEMENT each time it
   processes a `[...]'.")

(defvar *nodevars* nil
  "The triple BUILD-PATTERN-ELEMENT is currently working with.")

(defvar *indexf* nil
  "Index feature chosen by the pattern element for this rule.")

(defvar *pfeats* nil
  "Pattern feature accumulator for the whole rule.")

(defvar *semi-flag* nil
  "When `inpat', the `;' infix behaves as `and' instead of `progn'.")

(defvar *psych-sw* nil
  "Marcus's experimental flag (glang.l line 312); off by default.")


;;; -------------------------------------------------------------------
;;; Helpers used inside denotation bodies
;;; -------------------------------------------------------------------

(defun get-var-list ()
  "Read a comma-separated sequence of tokens, stopping at `;' or any
   other separator. Mirrors glang.l line 225."
  (unless (eq *token* '|;|)
    (cons (prog1 *token* (advance))
          (when (eq *token* '|,|)
            (advance)
            (get-var-list)))))


(defun parselist-unless (rbp continuation stop)
  "Parse a CONTINUATION-separated list of expressions (each at RBP)
   until the STOP token. Returns the parsed list. Mirrors glang.l
   line 410."
  (cond
    ((eq *token* stop) nil)
    (t (cons (pratt-parse rbp)
             (when (is-token continuation)
               (parselist-unless rbp continuation stop))))))


;;; -------------------------------------------------------------------
;;; Delimiters
;;; -------------------------------------------------------------------

(delim |]|)
(delim |-->|)
(delim |--->|)
(delim |,|)
;; `}' is already set to lbp = -1 in pratt.lisp.

;; `;' is an infixm: inside a pattern body it builds `(and ...)', elsewhere
;; `(progn ...)'. See glang.l line 402. Bp 3 is intentional: higher than
;; `or' / `and' / `is' so it groups statements; lower than `,' so a
;; comma-list inside one clause doesn't get torn apart.
(infixm |;| 3
  (cond ((and (boundp '*semi-flag*) (eq *semi-flag* 'inpat)) 'and)
        (t 'progn)))


;;; -------------------------------------------------------------------
;;; Pattern element: `[ ... ]'
;;;
;;; Port of glang.l lines 310-336. Pops the next entry off
;;; *NODEVARLIST*, builds an optional `(or var (set* index))' wrapper
;;; (except for first position, which Marcus treats specially), parses
;;; the pattern body, then sublis-substitutes `*' for the position's
;;; var in the result.
;;; -------------------------------------------------------------------

(prefix |[| 0 (build-pattern-element))


(defun build-pattern-element ()
  (let ((*semi-flag* 'inpat)
        (starsubst nil)
        (set* nil)
        (patbody nil))
    (cond
      ((is-token '**)
       (setq starsubst (list (cons '* (eat-token)))
             *nodevars* nil))
      (t
       (setq *nodevars* (pop *nodevarlist*))
       (unless *nodevars*
         (warn "pattern of ~s has unidentified element" *rulename*))
       (let ((position-name (car *nodevars*)))
         (unless (or *psych-sw*
                     (member position-name '(NTH |1ST|) :test #'eq))
           (setq set* (list 'or position-name
                            (list 'set* (cadr *nodevars*))))))
       (setq starsubst (caddr *nodevars*))))
    (is-token '|;|)
    (setq patbody (pratt-parse 0))
    (check '|]|)
    (sublis starsubst
            (cond (set* (list 'and set* patbody))
                  (t patbody)))))


;;; -------------------------------------------------------------------
;;; The RULE denotation
;;;
;;; Plain `{RULE ...}' enters via the prefix; `{AS RULE ...}' and
;;; `{NR RULE ...}' enter via the infix, where AS / NR is the left
;;; operand and RULE is the operator. (See glang.l lines 274-275.)
;;; -------------------------------------------------------------------

(prefix rule 1 (build-rule 'normal))
(infix  rule 1 (build-rule *left*))


(defun build-rule (type)
  (let ((*rulename* (eat-token)))
    (unless (member type '(nr as normal) :test #'eq)
      (warn "~s is not a legal rule type (got rule ~s)" type *rulename*))
    (let* ((priority
             (cond
               ((is-token '|PRIORITY:|)
                (let ((p (eat-token)))
                  (is-token '|.|)               ; tolerate trailing `.'
                  p))
               ((numberp *token*)
                (let ((p (eat-token)))
                  (is-token '|.|)
                  p))
               (t 10)))
           (packets
             (progn (check 'in) (get-var-list)))
           (pat nil)
           (index-info
             (let ((*nodevarlist*
                     (if (eq type 'normal)
                         '((|1ST| 0 ((* . |1ST|)))
                           (|2ND| 1 ((* . |2ND|)))
                           (|3RD| 2 ((* . |3RD|))))
                         '((NTH *INT-INDEX* ((* . NTH))))))
                   (*nodevars* nil)
                   (*indexf* nil)
                   (*pfeats* nil)
                   (result '()))
               (loop until (and (not-token '|[|) (not-token 't))
                     do (push (pratt-parse 0) result))
               (unless result
                 (warn "no pattern for ~s?" *rulename*))
               (setq pat
                     (cond ((null (cdr result)) (car result))
                           (t (cons 'and (nreverse result)))))
               (cons (or *indexf* 'noindexf) *pfeats*)))
           (action
             (progn (check '(|-->| |--->|))
                    (pratt-parse 0))))
      (list 'rule
            *rulename*
            (list type priority packets index-info)
            pat
            action))))


;;; -------------------------------------------------------------------
;;; The action-sequence `.' infix
;;;
;;; `A. B. C.' parses to (PROGN A B C). Stops at `}' or end of input.
;;; (glang.l line 406.)
;;; -------------------------------------------------------------------

(infix |.| 1
  (cons 'progn
        (cons *left*
              (parselist-unless *drbp* '|.| '|}|))))


;;; -------------------------------------------------------------------
;;; The ACTIVATE prefix
;;;
;;; `Activate cpool, ss-start' parses to (ACTIVATE '(CPOOL SS-START)).
;;; Marcus uses `(denfun 'activate (kwote (getvarlist)))' at glang.l
;;; line 439; we write the equivalent inline. Each emitted action call
;;; is a list because we're producing data (the compiled-Lisp form),
;;; not executing it.
;;; -------------------------------------------------------------------

(prefix activate 10
  (list 'activate (list 'quote (get-var-list))))


;;; -------------------------------------------------------------------
;;; Other simple parser-control verbs (glang.l lines 438-442)
;;; -------------------------------------------------------------------

(prefix deactivate 10
  (list 'deactivate (list 'quote (get-var-list))))

(prefix restore 10
  (progn (check 'buffer) (list 'bufrestore)))

(prefix run 10
  (let ((rule-name (eat-token)))
    (check 'next)
    (list 'setq :nextrule (list 'quote rule-name))))

;; `Parse is finished.' --> (setq :parsecomplete t)
(prefix parse 10
  (progn (check 'is) (check 'finished)
         (list 'setq :parsecomplete t)))


;;; -------------------------------------------------------------------
;;; Nilfix atoms (glang.l lines 450, 536, 545, 546)
;;; -------------------------------------------------------------------

(nilfix last (list :last))                 ; (:last) function call form
(nilfix it :it)                            ; self-evaluating keyword
(nilfix wh-comp (list 'wh-comp))           ; (wh-comp) call form

;; `Current s' (with `s' a literal delimiter, not an operand).
(nilfix current
  (progn (check 's) (list 'current-s)))


;;; -------------------------------------------------------------------
;;; Pattern feature-match `=' (glang.l line 350)
;;;
;;; Inside a pattern element (i.e. when *nodevars* is bound), `=foo,bar'
;;; compiles to `(fast-is INDEX (foo bar))', where INDEX is the position
;;; (0, 1, 2) of the surrounding pattern element. Outside a pattern
;;; element it falls back to `(is * (quote (foo bar)))'.
;;; -------------------------------------------------------------------

(defvar *gram-stats-indexfs* nil
  "Features that have been used as :indexf in some compiled rule. Used by
   PICK-INDEX to prefer never-used features (Marcus's heuristic:
   later/more-obscure features are better discriminators).
   Mirrors Marcus's (get :gram-stats 'indexfs).")

(defun pick-index (flist)
  "Pick a feature from FLIST to be the indexf for this rule's pattern.
   Marcus's heuristic: prefer the last feature not already used as an
   :indexf, fall back to the last feature. See glang.l line 368."
  (let (old new)
    (dolist (f flist)
      (cond ((member f *gram-stats-indexfs* :test #'eq) (setq old f))
            (t (setq new f))))
    (let ((choice (or new old)))
      (when choice (pushnew choice *gram-stats-indexfs* :test #'eq))
      choice)))

(defun build-= ()
  "Body of the `=' pattern-element prefix. See glang.l line 352."
  (cond
    (*nodevars*
     (let ((vars (get-var-list)))
       (setq *pfeats* (union (reverse vars) *pfeats*))
       (when (member (car *nodevars*) '(|1ST| nth) :test #'eq)
         (setq *indexf* (pick-index vars)))
       (list 'fast-is (cadr *nodevars*) vars)))
    (t (list 'is '* (list 'quote (get-var-list))))))

(prefix |=| 10 (build-=))


;;; -------------------------------------------------------------------
;;; Relational infixes (glang.l lines 469-478)
;;;
;;;   `X is feat-list'        -> (is X '(feat-list))
;;;   `X is not feat-list'    -> (is-not-all-of X '(feat-list))
;;;   `X is any of feat-list' -> (is-any-of X '(feat-list))
;;;   `X is none of feat-list'-> (is-none-of X '(feat-list))
;;;   `X is not any of feats' -> (is-none-of X '(feats))
;;;   `X is not all of feats' -> (is-not-all-of X '(feats))
;;;
;;; `is' yields *left* unchanged when followed by one of NONE / NOT /
;;; ANY / GREATER / LESS / EQUAL, because those words start their own
;;; infix sequence to *left*.
;;; -------------------------------------------------------------------

(infix is 10
  (cond
    ((member *token* '(none not any greater less equal) :test #'eq)
     *left*)
    (t (is-token 'labelled)               ; optional `labelled' filler
       (list 'is *left* (list 'quote (get-var-list))))))

(infix not 10
  (let ((fn (cond
              ((is-token 'any) 'is-none-of)
              (t (is-token 'all) 'is-not-all-of))))
    (is-token 'of)                         ; optional `of'
    (list fn *left* (list 'quote (get-var-list)))))

(infix none 10
  (progn (check 'of)
         (list 'is-none-of *left* (list 'quote (get-var-list)))))

(infix any 10
  (progn (check 'of)
         (list 'is-any-of *left* (list 'quote (get-var-list)))))


;;; -------------------------------------------------------------------
;;; Top-level entry: parse a single `{ ... }' rule from STRING.
;;; -------------------------------------------------------------------

(defun parse-rule (string)
  "Tokenize STRING, consume `{', Pratt-parse the rule body, consume `}',
   and return the intermediate `(rule NAME (TYPE PRI PACKETS INDEX) PAT
   ACTION)' form. Use COMPILE-RULE to get Marcus's final emitted form."
  (let ((*token-stream* (make-token-stream string)))
    (advance)
    (check '|{|)
    (let ((intermediate (pratt-parse -1)))
      (check '|}|)
      intermediate)))


;;; -------------------------------------------------------------------
;;; Header-only parse
;;;
;;; Reads only the rule frame -- kind, name, priority, packets (and the
;;; UPPER/OVER/LOWER bits for case rules) -- and stops at the start of
;;; the pattern list. Bypasses the Pratt parser entirely, since the
;;; frame is regular enough to parse with a hand-coded walk. Used for
;;; cross-validation against the existing rule-frame parser while we
;;; still have limited action-verb coverage in the denotations.
;;; -------------------------------------------------------------------

(defun parse-rule-header (string)
  "Read the rule frame from STRING and return a plist:
     :kind     :rule | :as-rule | :nr-rule | :attachment-crule | :creation-crule
     :name     symbol
     :priority integer (defaults to 10) -- ordinary rules only
     :packets  list of symbols           -- ordinary rules only
     :upper    symbol                    -- attachment crules only
     :lower    symbol                    -- attachment crules only
     :node-type symbol                   -- creation crules only"
  (let ((*token-stream* (make-token-stream string)))
    (advance)
    ;; Skip any tokens that aren't `{' (top-level comment leftovers,
    ;; commented-out rules of the form `;{...;}', etc.). The rule-frame
    ;; parser's wrapper does the same thing.
    (loop until (or (eq *token* '|{|) (eq *token* *eof*))
          do (advance))
    (check '|{|)
    (let ((kind
            (cond
              ((is-token 'as)         (check 'rule) :as-rule)
              ((is-token 'nr)         (check 'rule) :nr-rule)
              ((is-token 'attachment) (check 'crule) :attachment-crule)
              ((is-token 'creation)   (check 'crule) :creation-crule)
              ((is-token 'rule)                       :rule)
              (t (error 'glang-parse-error
                        :token *token* :left nil :rulename nil
                        :message (format nil "unknown rule kind: ~s"
                                         *token*))))))
      (case kind
        ((:rule :as-rule :nr-rule)
         (let* ((name (eat-token))
                (priority
                  (cond
                    ((is-token '|PRIORITY:|)
                     (let ((p (eat-token))) (is-token '|.|) p))
                    ((numberp *token*)
                     (let ((p (eat-token))) (is-token '|.|) p))
                    (t 10))))
           (check 'in)
           (let ((packets (get-var-list)))
             (list :kind kind :name name
                   :priority priority :packets packets))))
        (:attachment-crule
         (let* ((name (eat-token))
                (upper (eat-token)))
           (check 'over)
           (let ((lower (eat-token)))
             (list :kind kind :name name :upper upper :lower lower))))
        (:creation-crule
         (let* ((name (eat-token))
                (node-type (eat-token)))
           (list :kind kind :name name :node-type node-type)))))))
