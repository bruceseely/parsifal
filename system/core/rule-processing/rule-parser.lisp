;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-

(in-package :parsifal)

;;; Rule-frame parser for the PIDGIN grammar language used by PARSIFAL.
;;;
;;; This parser recognizes only the *frame* of a rule -- the kind
;;; (RULE / AS RULE / NR RULE / ATTACHMENT CRULE / CREATION CRULE), the
;;; name, the priority, the packet list, and the pattern slots -- and
;;; collects the action body as an opaque list of (type value) token
;;; pairs for the action parser to deal with later. Pattern internals
;;; are likewise collected as opaque raw token lists; a small
;;; classifier labels each pattern by its leading marker.
;;;
;;; Two layers:
;;;   1. RULE-STREAM-LEXER wraps PIDGIN-RULE-LEXER so the yacc grammar
;;;      sees a single :PATTERN-BODY token between [...] and a single
;;;      :ACTION-BODY token between --> and the matching }.
;;;   2. *PIDGIN-RULE-PARSER* is the cl-yacc grammar over that token
;;;      stream.
;;;
;;; Entry points: PARSE-RULE-TEXT, PARSE-RULE-FILE.


;;; -------------------------------------------------------------------
;;; Wrapper lexer
;;; -------------------------------------------------------------------

(defun rule-stream-lexer (string)
  "Return a thunk suitable for YACC:PARSE-WITH-LEXER.
   Each call returns (values TYPE VALUE) or (values NIL NIL) at EOF.

   The thunk wraps PIDGIN-RULE-LEXER so that:
   - Tokens lying between :LBRACKET and the matching :RBRACKET are
     collected into a single :PATTERN-BODY token whose value is the
     list of (type value) pairs found inside.
   - Tokens lying between :ARROW and the matching :RBRACE are
     collected into a single :ACTION-BODY token in the same way.
   - At top level (outside any `{...}' rule), any token other than
     :LBRACE is silently skipped. This absorbs single-`;' comment
     lines and other inter-rule noise that the parser would otherwise
     reject -- gram4.l from Marcus uses single-`;' lines between rules.
   - In normal mode (not collecting a pattern or action body), :SEMI
     tokens are always skipped, since `;' is only a legitimate token
     inside pattern bodies and action bodies (both collected
     separately). This also lets us absorb commented-out rules of the
     `;{RULE...;}' form that Marcus uses in gram4.l.
   - Top-level `(...)' forms are treated as opaque (Lisp) data. The
     wrapper tracks paren depth and refuses to enter rule mode while
     depth > 0, so an embedded `{...}' Pidgin expression inside a
     `(defun ...)' is not mistaken for a top-level rule. (Marcus
     embeds Pidgin syntax inside Lisp; see find-WH-comp in the 1977
     corpus.)

   The :LBRACKET, :RBRACKET, :ARROW, and :RBRACE tokens themselves
   are still delivered to the parser at the correct positions."
  (let ((inner       (pidgin-rule-lexer string))
        (pending     nil)
        (mode        :normal)
        (inside-rule nil)
        (paren-depth 0))
    (labels
        ((update-state (type)
           (case type
             (:lbrace (setf inside-rule t))
             (:rbrace (setf inside-rule nil))))
         (collect (opener terminator emitted-type)
           ;; Collect raw (type value) pairs from INNER until the matching
           ;; TERMINATOR is seen at depth 0. Nested OPENER..TERMINATOR
           ;; pairs are kept inside the body. This matters for action
           ;; bodies that contain embedded `{...}' Pidgin expressions
           ;; (Marcus embeds rule-language inside Lisp inside actions).
           (let ((depth 0)
                 (acc '()))
             (loop
               (multiple-value-bind (type value) (funcall inner)
                 (cond
                   ((null type)
                    (setf mode :normal)
                    (return (values emitted-type (nreverse acc))))
                   ((eq type opener)
                    (incf depth)
                    (push (list type value) acc))
                   ((and (eq type terminator) (plusp depth))
                    (decf depth)
                    (push (list type value) acc))
                   ((eq type terminator)
                    (setf mode    :normal
                          pending (cons type value))
                    (return (values emitted-type (nreverse acc))))
                   (t
                    (push (list type value) acc)))))))
         (next ()
           (cond
             (pending
              (let ((tok pending))
                (setf pending nil)
                (update-state (car tok))
                (values (car tok) (cdr tok))))
             ((eq mode :collect-pattern)
              (collect :lbracket :rbracket :pattern-body))
             ((eq mode :collect-action)
              (collect :lbrace :rbrace :action-body))
             (t
              ;; Inner loop so we can skip top-level noise tokens.
              (loop
                (multiple-value-bind (type value) (funcall inner)
                  (cond
                    ((null type)
                     (return (values nil nil)))
                    ;; Top-level paren tracking: count `(' and `)' so we
                    ;; can recognise that any `{...}' inside them is
                    ;; embedded Lisp, not a rule.
                    ((and (not inside-rule) (eq type :lparen))
                     (incf paren-depth))
                    ((and (not inside-rule) (eq type :rparen))
                     (when (plusp paren-depth) (decf paren-depth)))
                    ((eq type :lbrace)
                     (cond
                       ((or inside-rule (zerop paren-depth))
                        (update-state type)
                        (return (values type value)))
                       (t)))  ; inside top-level parens -- skip
                    ((eq type :rbrace)
                     (cond
                       (inside-rule
                        (update-state type)
                        (return (values type value)))
                       (t)))  ; stray or inside-parens -- skip
                    ((not inside-rule)
                     ;; Stray top-level token -- skip and try again.
                     )
                    ((eq type :semi)
                     ;; `;' is only meaningful inside pattern or action
                     ;; bodies, both collected elsewhere. Skip here so
                     ;; commented-out rules and stray separators don't
                     ;; surface to the parser.
                     )
                    ((eq type :lbracket)
                     (setf mode :collect-pattern)
                     (return (values type value)))
                    ((eq type :arrow)
                     (setf mode :collect-action)
                     (return (values type value)))
                    (t
                     (return (values type value))))))))))
      #'next)))


;;; -------------------------------------------------------------------
;;; Pattern classification
;;; -------------------------------------------------------------------

(defun classify-pattern (raw-tokens)
  "Wrap RAW-TOKENS (the raw token list from a :PATTERN-BODY) in a
   pattern plist, labelling its class by leading-token marker."
  (let ((class
         (cond
           ((null raw-tokens) :empty)
           ((and (eq (first (first raw-tokens)) :identifier)
                 (string-equal (second (first raw-tokens)) "t"))
            :wildcard)
           ((eq (first (first raw-tokens)) :equals)   :feature-match)
           ((eq (first (first raw-tokens)) :starstar) :context)
           ((eq (first (first raw-tokens)) :star)     :test)
           (t :unknown))))
    (list :class class :raw raw-tokens)))


;;; -------------------------------------------------------------------
;;; Grammar
;;; -------------------------------------------------------------------

(yacc:define-parser *pidgin-rule-parser*
  (:start-symbol ruleset)
  (:terminals (:lbrace :rbrace :lbracket :rbracket
               :lparen :rparen
               :arrow :colon :period :comma :semi
               :equals :star :starstar :bang
               :rule :crule :as :nr
               :attachment :creation :in :over :priority
               :identifier :number :literal :position
               :pattern-body :action-body))

  ;; A whole file: zero or more rules.
  (ruleset
   nil
   (rule ruleset
         (lambda (r rest) (cons r rest))))

  ;; A rule: {...}.
  (rule
   (:lbrace rule-content :rbrace
            (lambda (lb c rb) (declare (ignore lb rb)) c)))

  (rule-content
   ordinary-rule
   attachment-crule
   creation-crule)


  ;; --- ordinary rules: RULE / AS RULE / NR RULE ----------------------

  (ordinary-rule
   (rule-kind :identifier opt-priority :in packet-list
              opt-patterns :arrow :action-body
              (lambda (kind name pri in-tok packs pats arrow-tok body)
                (declare (ignore in-tok arrow-tok))
                (list :kind     kind
                      :name     name
                      :priority pri
                      :packets  packs
                      :patterns pats
                      :action   body))))

  (rule-kind
   (:rule        (lambda (x) (declare (ignore x)) :rule))
   (:as :rule    (lambda (a r) (declare (ignore a r)) :as-rule))
   (:nr :rule    (lambda (a r) (declare (ignore a r)) :nr-rule)))

  (opt-priority
   nil
   (:priority :colon :number opt-period
              (lambda (p c n pd) (declare (ignore p c pd)) n)))

  (opt-period
   nil
   (:period (lambda (p) (declare (ignore p)) nil)))

  (packet-list
   (:identifier
    (lambda (id) (list id)))
   (:identifier :comma packet-list
                (lambda (id c rest) (declare (ignore c)) (cons id rest))))

  (opt-patterns
   nil
   pattern-list
   bare-wildcard)

  ;; Marcus uses a shorthand: a rule whose only pattern is the wildcard
  ;; can write `t -->' (or `T -->') in place of `[t] -->'. We accept any
  ;; bare :identifier here; the classifier will label it :wildcard when
  ;; the value is "t", :unknown otherwise (useful for spotting OCR damage).
  (bare-wildcard
   (:identifier
    (lambda (v)
      (list (classify-pattern (list (list :identifier v)))))))

  (pattern-list
   (pattern (lambda (p) (list p)))
   (pattern pattern-list
            (lambda (p rest) (cons p rest))))

  (pattern
   (:lbracket :pattern-body :rbracket
              (lambda (lb body rb)
                (declare (ignore lb rb))
                (classify-pattern body))))


  ;; --- case rules ----------------------------------------------------

  (attachment-crule
   (:attachment :crule :identifier :identifier :over :identifier crule-body
                (lambda (a cr name upper ov lower body)
                  (declare (ignore a cr ov))
                  (list :kind   :attachment-crule
                        :name   name
                        :upper  upper
                        :lower  lower
                        :action body))))

  (creation-crule
   (:creation :crule crule-name crule-node-type crule-body
              (lambda (cre cr name nt body)
                (declare (ignore cre cr))
                (list :kind      :creation-crule
                      :name      name
                      :node-type nt
                      :action    body))))

  ;; A creation crule's name may be a bare identifier (the common case)
  ;; or a `*-something' form using Marcus's `*' node-type prefix
  ;; (e.g. `*-DUMMY'). The hyphen is dropped by the lexer, so we
  ;; reconstitute it here.
  (crule-name
   :identifier
   (:star :identifier
          (lambda (s id) (declare (ignore s))
            (concatenate 'string "*-" id))))

  ;; The node-type slot may likewise be a bare identifier or a bare
  ;; `*' (Marcus's anonymous-node type).
  (crule-node-type
   :identifier
   (:star (lambda (s) (declare (ignore s)) "*")))

  ;; --- crule action body: collect raw tokens up to (not including) :rbrace.
  ;; Unlike ordinary rules, crules have no :arrow to trigger the wrapper's
  ;; action collection, so we do it here in the grammar. The shape of the
  ;; result is identical to that of an ordinary rule's :action -- a list of
  ;; (type value) pairs.
  (crule-body
   ()
   (crule-body-token crule-body
                     (lambda (tok rest) (cons tok rest))))

  (crule-body-token
   (:identifier   (lambda (v) (list :identifier   v)))
   (:number       (lambda (v) (list :number       v)))
   (:literal      (lambda (v) (list :literal      v)))
   (:position     (lambda (v) (list :position     v)))
   (:lbrace       (lambda (v) (list :lbrace       v)))
   (:lbracket     (lambda (v) (list :lbracket     v)))
   (:rbracket     (lambda (v) (list :rbracket     v)))
   (:lparen       (lambda (v) (list :lparen       v)))
   (:rparen       (lambda (v) (list :rparen       v)))
   (:arrow        (lambda (v) (list :arrow        v)))
   (:colon        (lambda (v) (list :colon        v)))
   (:period       (lambda (v) (list :period       v)))
   (:comma        (lambda (v) (list :comma        v)))
   (:semi         (lambda (v) (list :semi         v)))
   (:equals       (lambda (v) (list :equals       v)))
   (:star         (lambda (v) (list :star         v)))
   (:starstar     (lambda (v) (list :starstar     v)))
   (:bang         (lambda (v) (list :bang         v)))
   (:rule         (lambda (v) (list :rule         v)))
   (:crule        (lambda (v) (list :crule        v)))
   (:as           (lambda (v) (list :as           v)))
   (:nr           (lambda (v) (list :nr           v)))
   (:attachment   (lambda (v) (list :attachment   v)))
   (:creation     (lambda (v) (list :creation     v)))
   (:in           (lambda (v) (list :in           v)))
   (:over         (lambda (v) (list :over         v)))
   (:priority     (lambda (v) (list :priority     v)))
   (:pattern-body (lambda (v) (list :pattern-body v)))
   (:action-body  (lambda (v) (list :action-body  v)))))


;;; -------------------------------------------------------------------
;;; Entry points
;;; -------------------------------------------------------------------

(defun parse-rule-text (string)
  "Parse STRING (one or more pidgin grammar rules) and return a list
   of rule plists. Signals YACC:YACC-PARSE-ERROR on malformed input."
  (yacc:parse-with-lexer (rule-stream-lexer string) *pidgin-rule-parser*))

(defun parse-rule-file (path)
  "Parse the rules in PATH and return a list of rule plists."
  (with-open-file (in path :direction :input)
    (let ((buf (with-output-to-string (out)
                 (loop for line = (read-line in nil nil)
                       while line do (write-line line out)))))
      (parse-rule-text buf))))


;;; -------------------------------------------------------------------
;;; Inspection helpers
;;; -------------------------------------------------------------------

(defun rule-summary (rule)
  "Return a short plist summary of RULE, omitting the raw action body."
  (ecase (getf rule :kind)
    ((:rule :as-rule :nr-rule)
     (list :kind     (getf rule :kind)
           :name     (getf rule :name)
           :priority (getf rule :priority)
           :packets  (getf rule :packets)
           :patterns (mapcar (lambda (p) (getf p :class))
                             (getf rule :patterns))))
    (:attachment-crule
     (list :kind  :attachment-crule
           :name  (getf rule :name)
           :upper (getf rule :upper)
           :lower (getf rule :lower)))
    (:creation-crule
     (list :kind      :creation-crule
           :name      (getf rule :name)
           :node-type (getf rule :node-type)))))
