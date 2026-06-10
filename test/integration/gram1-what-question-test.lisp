;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-what-question-test.lisp
;;;
;;; A wh-DETERMINER object question, parsed end to end and entirely from the
;;; LOADED defs.l dictionary: "what did the boy break ?"
;;;
;;;     what_i  did  the boy  break  t_i      ->  agent = the boy, neutral = what
;;;
;;; This is the object wh-question (subject-aux inversion, object gap) but
;;; fronted by the wh-determiner `what' rather than the pronoun `who'. `what'
;;; is det\relpron-ambig in the lexicon and carries only ngstart +
;;; det\relpron-ambig -- neither an as-type -- so it could not trigger the
;;; STARTNP attention-shift to reach its diagnosis rule (the gap noted in the
;;; proper-noun commit). Adding det\relpron-ambig to *as-types* (defs.lisp)
;;; lets STARTNP fire; WHAT-DIAG (in npool) then diagnoses `what' as a wh
;;; pronoun (the next word `did' is not a noun-group start), and from there it
;;; behaves exactly like `who': WH-QUEST fronts it, SUBJ-QUEST?/AUX-INVERSION
;;; make `the boy' the overt subject, and the object gap fills with a trace
;;; bound to `what'.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-what-question-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-what-question-test (&optional verbose)
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

      ;; Fully dictionary-driven: what/did(do)/the/boy/break/? all from defs.l.
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*
                        *pronoun-rule* *qp1-done-rule* *wh-question-rules*
                        *inversion-rules* *object-wh-rules* *wh-determiner-rules*)

      ;; Sanity: `what' really is the det\relpron-ambig word in the dictionary.
      (truthy "dict `what' carries det\\relpron-ambig"
              (member (intern "DET\\RELPRON-AMBIG" :parsifal) (get 'what 'feats)))

      (let ((ok (parse-sentence "what did the boy break ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"what did the boy break ?\"" ok t)
        (truthy "S is a major wh-question" (subsetp '(major quest wh-quest s) (fe c)))

        (let ((whc (getr :wh-comp c)))
          (truthy "the S has a :wh-comp register" whc)
          ;; `what' was diagnosed into a wh pron-np (relpron-np), like `who'.
          (truthy "the wh-element is a wh pron-np (the diagnosed `what')"
                  (and whc (subsetp '(pron-np relpron-np wh) (fe whc))))
          (truthy "the wh-comp is marked utilized"
                  (and whc (member 'utilized (fe whc))))

          ;; Subject is the inverted, overt `the boy'.
          (let ((subj (daughter 'np c)))
            (truthy "the subject is overt (not a trace)"
                    (and subj (not (member 'trace (fe subj)))))
            (check "the subject is \"boy\"" (word-of subj) 'boy))

          (let* ((vp  (daughter 'vp c))
                 (obj (and vp (daughter 'np vp))))
            (truthy "a VP attached to S" vp)
            (truthy "the aux carries do-support (`did')"
                    (let ((aux (daughter 'aux c)))
                      (and aux (getr 'word (daughter 'do aux)))))
            (check "the verb is \"break\""
                   (and vp (getr 'word (daughter 'verb vp))) 'break)

            ;; The object is the wh-gap: a trace bound to `what'.
            (truthy "the object is a trace" (and obj (member 'trace (fe obj))))
            (check "the object trace is bound to the wh-element (what)"
                   (and obj (getr 'binding obj)) whc)
            (truthy "final (question) punctuation attached" (daughter 'finalpunc c))

            ;; Case frame: the boy is AGENT, the wh-gap (object trace) is NEUT.
            (let ((cf (and vp (getr 'caseframe vp))))
              (truthy "VP has a case frame" cf)
              (check "predicate is break" (and cf (get cf 'pred)) 'break)
              (closeframe openframe)
              (let ((filled (cadr (first (get cf 'hypo-slots)))))
                (truthy "the frame has filled cases" filled)
                (let ((agt (assoc 'agt filled)) (neut (assoc 'neut filled)))
                  (truthy "an agent case was filled" agt)
                  (check "the agent is the subject NP (the boy)" (cadr agt) (daughter 'np c))
                  (truthy "a neutral case was filled" neut)
                  (check "the neutral case is the object trace (the wh-gap)"
                         (cadr neut) obj))))))))

    (format t "~&gram1-what-question-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-what-question-test t)
