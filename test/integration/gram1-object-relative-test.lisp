;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-object-relative-test.lisp
;;;
;;; An OBJECT relative clause, parsed end to end and entirely from the loaded
;;; defs.l dictionary: "the boy who you met ." (an NP utterance whose NP is
;;; modified by a relative clause in which the head is the relative verb's
;;; OBJECT, not its subject).
;;;
;;;     [NP the boy_i  [S-rel  who_i  [ you  met  t_i ] ]]
;;;
;;; Contrast with the subject relative (gram1-relative-clause-test): there the
;;; gap was the relative subject. Here WH-RELATIVE-CLAUSE's "if 2nd is a verb"
;;; subject-trace branch does NOT fire (2nd is `you', a pronoun), so `you'
;;; becomes the overt relative subject and the wh-comp `who' is the OBJECT
;;; gap: MAIN-VERB(met) activates wh-vp, WH-WITH-END-NEXT runs CREATE-WH-TRACE
;;; to drop an object trace bound to `who', and OBJ-IN-EMBEDDED-S attaches it.
;;; So "you met the boy": the head `the boy' is the object of `met' (via the
;;; trace chain  trace -> who -> the boy), and `you' is the relative subject.
;;;
;;; Pure composition -- no new rules over the subject-relative test (it adds
;;; *inversion-rules* *object-wh-rules*, whose WH-WITH-END-NEXT / WH-RESOLVED handle the object
;;; gap inside the relative clause).
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-object-relative-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-object-relative-test (&optional verbose)
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
           (noun-of (np) (and np (getr 'word (daughter 'noun (daughter 'nbar np)))))
           (pron-of (np) (and np (getr 'word (daughter 'pronoun np)))))

      ;; Fully dictionary-driven: the/boy/who/you/met (meet)/. from defs.l.
      (reset-rule-table)
      (register-grammar *np-rules* *np-utterance-rule* *pronoun-rule*
                        *clause-rules* *vp-np-rule* *inf-complement-rules*
                        *wh-question-rules* *inversion-rules* *object-wh-rules*
                        *relative-clause-rules*)

      (let ((ok (parse-sentence "the boy who you met ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"the boy who you met .\"" ok t)
        (truthy "the utterance is an NP utterance" (subsetp '(np-utterance s) (fe c)))

        (let ((np (daughter 'np c)))
          (truthy "the utterance NP is present" np)
          (check "the head noun is \"boy\"" (noun-of np) 'boy)
          (truthy "the head NP is `modified'" (and np (member 'modified (fe np))))

          (let* ((rel     (and np (daughter 's np)))
                 (whc     (and rel (getr :wh-comp rel)))
                 (relsubj (and rel (daughter 'np rel)))
                 (relvp   (and rel (daughter 'vp rel)))
                 (relobj  (and relvp (daughter 'np relvp))))
            (truthy "a relative S is attached to the head NP" rel)
            (truthy "the relative S is sec/relative"
                    (and rel (subsetp '(sec relative s) (fe rel))))
            (truthy "the relativizer `who' is the wh-comp"
                    (and whc (subsetp '(relpron-np wh) (fe whc))))
            (check "the relativizer is bound to the head NP (the boy)"
                   (and whc (getr 'binding whc)) np)

            ;; KEY: the relative SUBJECT is overt (`you'), NOT a trace ...
            (truthy "the relative subject is present" relsubj)
            (truthy "the relative subject is overt (not a trace)"
                    (and relsubj (not (member 'trace (fe relsubj)))))
            (check "the relative subject is \"you\"" (pron-of relsubj) 'you)

            (check "the relative verb is \"meet\""
                   (and relvp (getr 'word (daughter 'verb relvp))) 'met)

            ;; ... and the relative OBJECT is the gap: a trace bound to `who'.
            (truthy "the relative object is a trace" (and relobj (member 'trace (fe relobj))))
            (check "the relative object trace is bound to `who'"
                   (and relobj (getr 'binding relobj)) whc)
            (truthy "the wh-comp is marked utilized (the object gap is filled)"
                    (and whc (member 'utilized (fe whc))))

            ;; Relative case frame: `you' is AGENT, the object trace is NEUT.
            (let ((rcf (and relvp (getr 'caseframe relvp))))
              (truthy "the relative VP has a case frame" rcf)
              (check "the relative predicate is meet" (and rcf (get rcf 'pred)) 'meet)
              (closeframe openframe)
              (let ((filled (cadr (first (get rcf 'hypo-slots)))))
                (truthy "the relative frame has filled cases" filled)
                (let ((agt (assoc 'agt filled)) (neut (assoc 'neut filled)))
                  (truthy "an agent case was filled" agt)
                  (check "the relative agent is the subject `you'" (cadr agt) relsubj)
                  (truthy "a neutral case was filled" neut)
                  (check "the relative neutral is the object trace (the head)"
                         (cadr neut) relobj))))))

        (truthy "final punctuation attached" (daughter 'finalpunc c))))

    (format t "~&gram1-object-relative-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-object-relative-test t)
