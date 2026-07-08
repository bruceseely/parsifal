;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-control-test.lisp
;;;
;;; Delta-subject (control) complement, parsed end to end and entirely from
;;; the LOADED defs.l dictionary: "the boy wants to go ."
;;;
;;; `want' is a subj-less-inf-obj verb, so its infinitive complement has NO
;;; explicit subject. Marcus's analysis: the embedded subject is a DELTA -- a
;;; base-generated trace not bound by syntax but by later (semantic)
;;; processing -- which the VP-NP crule then binds to the matrix subject, i.e.
;;; equi-NP / control:
;;;
;;;     the boy_i wants [ delta_i to go ]
;;;
;;; Derivation path: MAIN-VERB(wants) activates inf-comp + subj-less-inf-comp;
;;; CREATE-DELTA-SUBJ-1 drops a trace np into the buffer to fill the missing
;;; embedded subject; INF-S-START1 takes it as the embedded subject; the
;;; embedded clause builds (to go); SUBJECT-IS-DELTA-DIAG labels the trace
;;; `delta'; DELTA-SUBJ-S-DONE drops the embedded S *without* finalizing its
;;; frame; COMP-TO-NP wraps it as a comp-np; OBJECTS attaches it to the matrix
;;; VP; and the full VP-NP crule binds the embedded delta to the matrix
;;; subject, fills the embedded subj slot, and finalizes the embedded frame.
;;;
;;; This composes the committed complement-attachment layer
;;; (*inf-complement-rules*) with the delta-control layer
;;; (*delta-complement-rules*) and the COMPLETE VP-NP crule
;;; (*vp-np-full-rule*, with delta binding) -- not the simpler *vp-np-rule*.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-control-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-control-test (&optional verbose)
  (let ((results t))
    (flet ((check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%"
                           expected actual)))
               (setf results (and pass results))))
           (truthy (test-name actual)
             (let ((pass (and actual t)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass (format t "    expected non-NIL, got NIL~%")))
               (setf results (and pass results))))
           (word-of (np)
             (and np (getr 'word (daughter 'noun (daughter 'nbar np))))))

      ;; Dictionary-driven: want/go/boy/the all come from the loaded defs.l.
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *inf-complement-rules*
                        *delta-complement-rules* *vp-np-full-rule*)

      ;; Sanity: `want' really is a subj-less-inf-obj verb in the dictionary.
      (truthy "dict `want' is subj-less-inf-obj"
              (member 'subj-less-inf-obj (get 'want 'feats)))

      (let ((ok (parse-sentence "the boy wants to go ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"the boy wants to go .\"" ok t)
        (truthy "matrix S is a major declarative" (subsetp '(decl major s) (fe c)))
        (check "matrix subject is \"boy\"" (word-of (daughter 'np c)) 'boy)

        (let* ((vp     (daughter 'vp c))
               (compnp (and vp (daughter 'np vp))))
          (truthy "a VP attached to S" vp)
          (check "matrix verb is \"want\""
                 (and vp (getr 'word (daughter 'verb vp))) 'wants)
          (truthy "the matrix object is a comp-np"
                  (and compnp (member 'comp-np (fe compnp))))
          (check "the comp-np is marked inf-comp"
                 (and compnp (getr 'markers compnp)) '(inf-comp))

          (let* ((embs  (and compnp (daughter 's compnp)))
                 (esubj (and embs (daughter 'np embs)))
                 (evp   (and embs (daughter 'vp embs))))
            (truthy "the comp-np dominates an embedded S" embs)
            (truthy "the embedded S is a comp-s / inf-s"
                    (and embs (subsetp '(sec comp-s inf-s s) (fe embs))))
            (check "the embedded verb is \"go\""
                   (and evp (getr 'word (daughter 'verb evp))) 'go)

            ;; The control: the embedded subject is a delta trace, with NO
            ;; explicit lexical subject, bound to the matrix subject.
            (truthy "the embedded subject is a trace" (and esubj (member 'trace (fe esubj))))
            (truthy "the embedded subject is labelled delta"
                    (and esubj (member 'delta (fe esubj))))
            (check "the embedded delta is bound to the matrix subject NP (control)"
                   (and esubj (getr 'binding esubj)) (daughter 'np c)))

          (truthy "final punctuation attached to S" (daughter 'finalpunc c))

          ;; Matrix case frame: subject -> AGENT, embedded clause -> NEUTRAL.
          (let ((cf (and vp (getr 'caseframe vp))))
            (truthy "matrix VP has a case frame" cf)
            (check "matrix predicate is want" (and cf (get cf 'pred)) 'want)
            (closeframe openframe)
            (let ((filled (cadr (first (get cf 'hypo-slots)))))
              (truthy "the matrix frame has filled cases" filled)
              (let ((agt (assoc 'agt filled)) (neut (assoc 'neut filled)))
                (truthy "an agent case was filled" agt)
                (check "the agent is the matrix subject NP" (cadr agt) (daughter 'np c))
                (truthy "a neutral case was filled" neut)
                (check "the neutral case is the comp-np (the embedded clause)"
                       (cadr neut) compnp)
                ;; the controller and the agent are the SAME NP
                (check "the embedded delta's controller is the matrix agent"
                       (getr 'binding (daughter 'np (daughter 's compnp)))
                       (cadr agt))))))))

    (format t "~&gram1-control-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-control-test t)
