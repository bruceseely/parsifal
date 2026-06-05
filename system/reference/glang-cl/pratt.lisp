;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/reference/glang-cl/pratt.lisp
;;;
;;; The Pratt-TDOP core, ported from glang.l lines 14-56. The big
;;; deviations from Marcus's MacLISP original are:
;;;
;;;   - keyword-named specials (`:token', `:left', `:drbp', `:rulename')
;;;     became regular specials with earmuffs (`*token*' etc.) since
;;;     keywords in CL are constants;
;;;   - errors signal a GLANG-PARSE-ERROR condition instead of throwing
;;;     to `lederr-bug';
;;;   - ASSOCIATE has an extra clause that stops on *EOF* so a parse
;;;     terminates naturally when input runs out (Marcus's parser
;;;     relied on the rule's closing `}' for that).

(in-package #:glang-cl)


;;; -------------------------------------------------------------------
;;; Specials
;;; -------------------------------------------------------------------

(defvar *token-stream* nil
  "Thunk: () -> next token. PARSE-STRING binds this for the duration
   of a parse.")

(defvar *token* nil
  "Current lookahead token (what ADVANCE last produced).")

(defvar *left* nil
  "The `left' value inside ASSOCIATE, visible to a led denotation.")

(defvar *drbp* nil
  "Default right binding power, bound by the fixity macros for the
   duration of a nud or led body. `(RIGHT)' uses this as its rbp.")

(defvar *rulename* nil
  "Name of the rule currently being parsed; surfaces in error reports.")


;;; -------------------------------------------------------------------
;;; Error condition
;;; -------------------------------------------------------------------

(define-condition glang-parse-error (error)
  ((token    :initarg :token    :reader pe-token)
   (left     :initarg :left     :reader pe-left)
   (rulename :initarg :rulename :reader pe-rulename)
   (message  :initarg :message  :reader pe-message))
  (:report
   (lambda (c stream)
     (format stream "~&glang parse error~@[ in rule ~a~]: ~a~%   token: ~s~%   left:  ~s"
             (pe-rulename c) (pe-message c) (pe-token c) (pe-left c)))))


(defun lederr ()
  (error 'glang-parse-error
         :token    *token*
         :left     *left*
         :rulename *rulename*
         :message  (format nil "token ~s does not take a left argument"
                           *token*)))


;;; -------------------------------------------------------------------
;;; Core parser
;;; -------------------------------------------------------------------

(defun advance ()
  "Read the next token into *TOKEN*. Marcus's ADVANCE silently flushes
   the articles `the', `an', `a' (glang.l line 24)."
  (loop
    (setf *token* (funcall *token-stream*))
    (cond
      ((eq *token* *eof*) (return *token*))
      ((member *token* '(the an a) :test #'eq))
      (t (return *token*)))))


(defun verify (den)
  "If DEN is non-NIL, advance one token and return DEN; otherwise NIL.
   Used by ASSOCIATE to step past an operator before invoking its led."
  (when den (advance) den))


(defun associate (rbp left)
  "Pratt's `associate' loop. Recursively consumes operators whose lbp
   exceeds RBP, with LEFT acting as the running left-hand value."
  (let ((*left* left))
    (cond
      ((eq *token* *eof*) left)
      ((< rbp (or (and (symbolp *token*) (get *token* :lbp)) 0))
       (associate rbp
                  (funcall (or (verify (get *token* :led))
                               (lederr)))))
      (t *left*))))


(defun pratt-parse (rbp)
  "Parse a single expression with right binding power RBP. The standard
   Pratt entry point."
  (associate rbp
             (cond
               ((and (symbolp *token*) (get *token* :nud))
                (funcall (prog1 (get *token* :nud) (advance))))
               (t (prog1 *token* (advance))))))


;;; -------------------------------------------------------------------
;;; Primitives used inside denotation bodies
;;; -------------------------------------------------------------------

(defun right ()
  "Parse a right operand at *DRBP*. Inside a nud or led body, *DRBP*
   has been bound to the operator's right binding power."
  (pratt-parse *drbp*))


(defmacro eat-token ()
  "Capture the current token and advance. Returns the captured value."
  `(prog1 *token* (advance)))


(defun check (delimiter)
  "If the current token is DELIMITER (a symbol, or a list of symbols
   any of which matches), advance past it. Otherwise signal a
   GLANG-PARSE-ERROR.

   Marcus's `check' (glang.l line 214) instead WARNs and pretends the
   delimiter was there; we prefer to fail loudly for now."
  (cond
    ((or (and (symbolp delimiter) (eq *token* delimiter))
         (and (consp delimiter) (member *token* delimiter :test #'eq)))
     (advance))
    (t (error 'glang-parse-error
              :token *token* :left *left* :rulename *rulename*
              :message (format nil "expected ~s; got ~s"
                               delimiter *token*)))))


(defun is-token (tok)
  "If the current token is TOK, advance past it and return T. Otherwise
   leave the stream alone and return NIL. (Marcus's ISTOKEN, glang.l
   line 154, with corrected semantics: MacLISP's IF has multi-clause
   then-progn, so his `(if* X (advance) t)' returned t only on match.)"
  (when (eq *token* tok) (advance) t))


(defun not-token (tok)
  (not (eq *token* tok)))


(defun parse-list (rbp continuation)
  "Parse a CONTINUATION-separated list, each item at RBP. Returns the
   list of parsed items. Mirrors glang.l line 221."
  (cons (pratt-parse rbp)
        (when (eq *token* continuation)
          (advance)
          (parse-list rbp continuation))))


;;; -------------------------------------------------------------------
;;; Top-level driver
;;; -------------------------------------------------------------------

(defun parse-string (string)
  "Tokenize STRING and run the Pratt parser at rbp = -1 (the value
   Marcus uses for outer-most parsing in GLANG-READ)."
  (let ((*token-stream* (make-token-stream string)))
    (advance)
    (pratt-parse -1)))


;;; -------------------------------------------------------------------
;;; The closing brace stops a rule. Done in pratt.lisp because it is
;;; foundational to how Marcus's parser terminates (glang.l line 58).
;;; -------------------------------------------------------------------

(setf (get (intern "}" :glang-cl) :lbp) -1)
