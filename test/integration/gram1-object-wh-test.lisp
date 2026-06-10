;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-object-wh-test.lisp
;;;
;;; An OBJECT wh-question with subject-aux inversion, parsed end to end and
;;; entirely from the LOADED defs.l dictionary: "who did the boy see ?"
;;; (Marcus's canonical example is "Who did John see?"; we use `the boy' for
;;; the subject so the sentence is fully dictionary-driven -- proper nouns
;;; need the separate names subsystem, whose AS trigger `name' is not in the
;;; ported *as-types*.)
;;;
;;; Here the wh-element's gap is in OBJECT position, and the auxiliary is
;;; inverted to the front:
;;;
;;;     who_i  did  the boy  see  t_i      ->  agent = the boy,  neutral = who
;;;
;;; Mechanism (building on the subject-wh layer): `who' fronts into the
;;; whcomp/:wh-comp (WH-QUEST). In parse-subj, SUBJ-QUEST? sees an auxverb
;;; (`did') with a following NP and a verb after it, so it routes to
;;; AUX-INVERSION, which attaches the post-aux NP (`the boy') as the subject;
;;; DO-SUPPORT consumes the inverted `did'. The subject is now overt, so the
;;; wh-comp stays unutilized into the VP: MAIN-VERB activates wh-vp, the buffer
;;; is exhausted at the object gap so WH-WITH-END-NEXT runs CREATE-WH-TRACE
;;; (dropping an object trace bound to the wh-comp), WH-RESOLVED returns to
;;; ss-vp, and OBJECTS attaches the trace -- filling the verb's NEUTRAL case.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-object-wh-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-object-wh-test (&optional verbose)
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

      ;; Fully dictionary-driven: who/did(do)/the/boy/see/? all from defs.l.
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*
                        *pronoun-rule* *wh-question-rules* *inversion-rules* *object-wh-rules*)

      (let ((ok (parse-sentence "who did the boy see ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"who did the boy see ?\"" ok t)
        (truthy "S is a major wh-question" (subsetp '(major quest wh-quest s) (fe c)))
        (truthy "S is np-quest" (member 'np-quest (fe c)))

        (let ((whc (getr :wh-comp c)))
          (truthy "the S has a :wh-comp register" whc)
          (truthy "the wh-element is the `who' pron-np"
                  (and whc (subsetp '(pron-np wh) (fe whc))))
          (truthy "the wh-comp is marked utilized"
                  (and whc (member 'utilized (fe whc))))

          ;; Subject is the inverted NP `the boy' -- overt, NOT a trace.
          (let ((subj (daughter 'np c)))
            (truthy "the subject NP is present" subj)
            (truthy "the subject is NOT a trace (it is overt)"
                    (and subj (not (member 'trace (fe subj)))))
            (check "the subject is \"boy\"" (word-of subj) 'boy))

          (let* ((vp  (daughter 'vp c))
                 (obj (and vp (daughter 'np vp))))
            (truthy "a VP attached to S" vp)
            ;; the inverted `did' became the aux's do-support
            (truthy "the aux carries do-support (`did')"
                    (let ((aux (daughter 'aux c)))
                      (and aux (getr 'word (daughter 'do aux)))))
            (check "the verb is \"see\""
                   (and vp (getr 'word (daughter 'verb vp))) 'see)

            ;; The object is the wh-gap: a trace bound to the wh-element.
            (truthy "the object is a trace" (and obj (member 'trace (fe obj))))
            (check "the object trace is bound to the wh-element (who)"
                   (and obj (getr 'binding obj)) whc)
            (truthy "final (question) punctuation attached" (daughter 'finalpunc c))

            ;; Case frame: the boy is AGENT, the wh-gap (object trace) is NEUT.
            (let ((cf (and vp (getr 'caseframe vp))))
              (truthy "VP has a case frame" cf)
              (check "predicate is see" (and cf (get cf 'pred)) 'see)
              (closeframe openframe)
              (let ((filled (cadr (first (get cf 'hypo-slots)))))
                (truthy "the frame has filled cases" filled)
                (let ((agt (assoc 'agt filled)) (neut (assoc 'neut filled)))
                  (truthy "an agent case was filled" agt)
                  (check "the agent is the subject NP (the boy)" (cadr agt) (daughter 'np c))
                  (truthy "a neutral case was filled" neut)
                  (check "the neutral case is the object trace (the wh-gap)"
                         (cadr neut) obj)
                  (check "the neutral was filled via the obj function"
                         (caddr neut) 'obj))))))))

    (format t "~&gram1-object-wh-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-object-wh-test t)
