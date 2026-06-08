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

;; `Run X next.' --> (setq *nextrule* 'X).
;; Marcus's source emits the MacLISP keyword-style special `:nextrule';
;; our runtime renames it `*nextrule*' (see system/core/runtime/declr.lisp
;; and notes/cl-adaptation.md's :keyword -> *earmuff* rule).
(prefix run 10
  (let ((rule-name (eat-token)))
    (check 'next)
    (list 'setq '*nextrule* (list 'quote rule-name))))

;; `Parse is finished.' --> (setq *parsecomplete* t).
;; Same :keyword -> *earmuff* rename as above.
(prefix parse 10
  (progn (check 'is) (check 'finished)
         (list 'setq '*parsecomplete* t)))


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
;;; Helpers used by the node-op and tree-access denotations below
;;; -------------------------------------------------------------------

(defun kwote-if-atom (x)
  "If X is an atom, wrap it in `(quote X)'; otherwise return X.
   Mirrors Marcus's KWOTE-IF-ATOM at glang.l line 160."
  (if (atom x) (list 'quote x) x))


(defun name-to-index (sym)
  "Map an ordinal symbol (1ST/2ND/3RD/NTH) to its buffer position 0/1/2.
   Used by DROP and INSERT for `before NTH' phrases. Mirrors Marcus's
   NAME-TO-INDEX macro at glang.l line 159; we default to 0 if SYM is
   not one of the position names."
  (case sym
    (|1ST| 0) (|2ND| 1) (|3RD| 2)
    (NTH 0)
    (t 0)))


;;; -------------------------------------------------------------------
;;; Node-op verbs (glang.l lines 451-495)
;;; -------------------------------------------------------------------

;; `Attach 1st to c as np'   --> (attach c 1st 'np)
;; `Attach c as np'          --> (attach1 c c 'np)  -- Marcus reverses
;;                                                     args via attach1
;;                                                     when first arg is c
(prefix attach 10
  (let* ((dn (right))
         (fn (cond ((is-token 'to) (right))
                   (t 'c)))
         (funct (progn (check 'as) (list 'quote (eat-token)))))
    (cond ((eq dn 'c) (list 'attach1 dn fn funct))
          (t (list 'attach fn dn funct)))))


;; `Drop c [into the buffer] [before NTH]'  --> (drop INDEX)
(prefix drop 10
  (progn
    (check 'c)
    (when (is-token 'into) (check 'buffer))
    (let ((index (if (is-token 'before) (name-to-index (eat-token)) 0)))
      (list 'drop index))))


;; `Insert NODE [into the buffer] [before NTH]'  --> (insert-node NODE INDEX)
(prefix insert 10
  (let ((node (right)))
    (when (is-token 'into) (check 'buffer))
    (let ((index (if (is-token 'before) (name-to-index (eat-token)) 0)))
      (list 'insert-node node index))))


;; `Label NODE [with] feat1, feat2, ...'  --> (addf1 NODE '(feat1 feat2 ...))
(prefix label 10
  (let ((target (right)))
    (is-token 'with)                          ; optional filler
    (list 'addf1 target (list 'quote (get-var-list)))))


;; `Remove [features|feature] f1, f2, ... from NODE'  --> (remf1 '(f1 f2) NODE)
(prefix remove 10
  (progn
    (or (is-token 'features) (is-token 'feature))
    (let ((feats (get-var-list)))
      (check 'from)
      (list 'remf1 (list 'quote feats) (right)))))


;; `Transfer [feature|features] f1, f2 from SRC to DST'
;;                                   --> (transfer '(f1 f2) SRC DST)
(prefix transfer 10
  (progn
    (or (is-token 'feature) (is-token 'features))
    (let ((feats (get-var-list)))
      (check 'from)
      (let ((src (right)))
        (check 'to)
        (list 'transfer (list 'quote feats) src (right))))))


;; `Features [of] NODE'  --> (fe NODE)
(prefix features 10
  (progn
    (is-token 'of)
    (list 'fe (right))))


;; `Set the FOO [register] of NODE to VAL'  --> (setr 'FOO VAL NODE)
;; (We don't implement Marcus's `:storeform' specialization — the
;; default SETR form suffices for everything in gram4.l.)
(prefix set 9
  (let ((prop (eat-token)))
    (is-token 'register)                      ; optional filler
    (check 'of)
    (let ((node (pratt-parse 1)))
      (check 'to)
      (list 'setr (list 'quote prop) (right) node))))


;;; -------------------------------------------------------------------
;;; Tree-access infixes and prefixes (glang.l lines 541-548)
;;; -------------------------------------------------------------------

;; `X of Y'                  --> (find-node 'X Y)        right-associative
(infixr of 20
  (list 'find-node (kwote-if-atom *left*) (right)))


;; `X above Y'               --> (node-above 'X Y)       right-associative
(infixr above 20
  (list 'node-above (kwote-if-atom *left*) (right)))


;; `Node above X'            --> (father-node X)
(prefix node 19
  (progn (check 'above)
         (list 'father-node (right))))


;; `Binding of X'            --> (binding X)
(prefix binding 18
  (progn (check 'of)
         (list 'binding (right))))


;; `X register of Y'         --> (getr 'X Y)
(infix register 18
  (progn (check 'of)
         (list 'getr (kwote-if-atom *left*) (right))))


;;; -------------------------------------------------------------------
;;; Logical infixes (glang.l lines 445-446)
;;; -------------------------------------------------------------------

(infixm and 5 'and)
(infixm or  4 'or)


;;; -------------------------------------------------------------------
;;; Grouping and function-call `(' / `)'  (glang.l lines 380-388)
;;;
;;; `(X)' (prefix)          --> X
;;; `F(A, B, C)' (infixd)   --> (F A B C)
;;; -------------------------------------------------------------------

(delim |)|)

(prefix |(| 0
  (prog1 (right) (check '|)|)))

(infixd |(| 30 0
  (let ((args (cond ((eq *token* '|)|) nil)
                    (t (parse-list 0 '|,|)))))
    (check '|)|)
    (cons *left* args)))


;;; -------------------------------------------------------------------
;;; If/then/else/andthen (glang.l lines 422-435)
;;;
;;;   If COND then ACTION                       --> (cond (COND ACTION))
;;;   If COND then ACTION else OTHER            --> (cond (COND ACTION) (t OTHER))
;;;   If COND then ACTION else (if A then B)    --> (cond (COND ACTION) (A B))
;;;     (i.e. an `else (cond ...)' is spliced rather than nested)
;;;   ... andthen FOLLOWUP                      --> (prog2 IFCLAUSE FOLLOWUP)
;;; -------------------------------------------------------------------

(prefix if 2
  (let* ((test       (right))
         (then-arm   (progn (check 'then) (right)))
         (ifclause   (list 'cond (list test then-arm)))
         (else-arm   (when (is-token 'else) (right))))
    (when else-arm
      (setq ifclause
            (append ifclause
                    (cond ((and (consp else-arm)
                                (eq (car else-arm) 'cond))
                           ;; splice the nested cond's clauses in
                           (cdr else-arm))
                          (t (list (list else-arm)))))))
    (cond ((is-token 'andthen)
           (list 'prog2 ifclause (right)))
          (t ifclause))))


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
