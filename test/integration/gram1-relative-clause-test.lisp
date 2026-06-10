;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-relative-clause-test.lisp
;;;
;;; A subject relative clause, parsed end to end and entirely from the LOADED
;;; defs.l dictionary: "the boy who runs ." (an NP utterance whose NP is
;;; modified by a relative clause).
;;;
;;;     [NP the boy_i  [S-rel  who_i  [ t_i runs ] ]]
;;;
;;; Relative clauses ARE wh-movement, so this reuses the whole wh-comp
;;; apparatus. When the NP `the boy' completes and a relative pronoun (`who',
;;; built into a relpron-np) follows, WH-RELATIVE-CLAUSE (in NP-COMPLETE):
;;;   - labels the NP `modified' and attaches a sec/relative S to it;
;;;   - makes `who' the relative clause's whcomp, BOUND to the head NP;
;;;   - since a verb (`runs') follows, drops a trace subject bound to the
;;;     wh-comp.
;;; The relative S then parses as an ordinary (embedded) clause. So the
;;; binding chain is  trace -> who -> the boy: "the boy" is understood as the
;;; subject of `runs'.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-relative-clause-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-relative-clause-test (&optional verbose)
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

      ;; Fully dictionary-driven: the/boy/who/runs (run jlike go)/. from defs.l.
      (reset-rule-table)
      (register-grammar *np-rules* *np-utterance-rule* *pronoun-rule*
                        *clause-rules* *inf-complement-rules* *wh-question-rules*
                        *relative-clause-rules*)

      (let ((ok (parse-sentence "the boy who runs ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"the boy who runs .\"" ok t)
        (truthy "the utterance is an NP utterance" (subsetp '(np-utterance s) (fe c)))

        (let ((np (daughter 'np c)))
          (truthy "the utterance NP is present" np)
          (check "the head noun is \"boy\"" (word-of np) 'boy)
          (truthy "the head NP is labelled `modified'"
                  (and np (member 'modified (fe np))))

          ;; The relative clause hangs off the head NP.
          (let* ((rel (and np (daughter 's np)))
                 (whc (and rel (getr :wh-comp rel)))
                 (relsubj (and rel (daughter 'np rel)))
                 (relvp (and rel (daughter 'vp rel))))
            (truthy "a relative S is attached to the head NP" rel)
            (truthy "the relative S is sec/relative"
                    (and rel (subsetp '(sec relative s) (fe rel))))

            ;; The relativizer `who' is the clause's wh-comp, bound to the head.
            (truthy "the relative clause has a :wh-comp (the relativizer)" whc)
            (truthy "the wh-comp is the `who' relpron-np"
                    (and whc (subsetp '(relpron-np wh) (fe whc))))
            (check "the relativizer is bound to the head NP (the boy)"
                   (and whc (getr 'binding whc)) np)

            ;; The relative subject is a trace bound to the relativizer -> so
            ;; the chain is  trace -> who -> the boy.
            (truthy "the relative subject is a trace" (and relsubj (member 'trace (fe relsubj))))
            (check "the relative subject trace is bound to the wh-comp (who)"
                   (and relsubj (getr 'binding relsubj)) whc)
            (truthy "the wh-comp is marked utilized (the gap is filled)"
                    (and whc (member 'utilized (fe whc))))

            (truthy "the relative clause has a VP" relvp)
            (check "the relative verb is \"run\""
                   (and relvp (getr 'word (daughter 'verb relvp))) 'runs)))

        (truthy "final punctuation attached to the utterance"
                (daughter 'finalpunc c))))

    (format t "~&gram1-relative-clause-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-relative-clause-test t)
