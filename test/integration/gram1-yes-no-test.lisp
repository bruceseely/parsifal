;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-yes-no-test.lisp
;;;
;;; A yes-no question, parsed end to end and entirely from the loaded defs.l
;;; dictionary: "did the boy meet you ?"
;;;
;;;     [S quest/ynquest/major  did  the boy  meet  you ]
;;;
;;; A yes-no question opens with an inverted auxiliary (no fronted wh-element):
;;; YES-NO-Q (in ss-start) sees the clause-initial auxiliary `did' before an NP
;;; and labels the clause quest/ynquest/major; then AUX-INVERSION attaches the
;;; post-aux NP (`the boy') as the subject, DO-SUPPORT folds the inverted `did'
;;; into the aux, and the rest is an ordinary transitive clause -- `meet' with
;;; object `you'. Unlike a wh-question there is no :wh-comp and no trace; the
;;; subject and object are both overt.
;;;
;;; Case frame: AGENT = the boy, NEUTRAL = you.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-yes-no-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-yes-no-test (&optional verbose)
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

      ;; Fully dictionary-driven: did(do)/the/boy/meet/you/? all from defs.l.
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule* *pronoun-rule*
                        *inversion-rules* *yes-no-rules*)

      (let ((ok (parse-sentence "did the boy meet you ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"did the boy meet you ?\"" ok t)

        ;; A yes-no (major) interrogative, NOT a wh-question.
        (truthy "the clause is a yes-no question (quest/ynquest/major)"
                (subsetp '(quest ynquest major s) (fe c)))
        (truthy "the clause is NOT a wh-question" (not (member 'wh-quest (fe c))))
        (truthy "there is no fronted wh-element (:wh-comp is empty)"
                (null (getr :wh-comp c)))

        (let ((subj (daughter 'np c))
              (aux  (daughter 'aux c)))
          ;; Subject is the inverted, overt `the boy' (not a trace).
          (truthy "the subject NP is present" subj)
          (truthy "the subject is overt (not a trace)"
                  (and subj (not (member 'trace (fe subj)))))
          (check "the subject is \"boy\"" (noun-of subj) 'boy)
          ;; The inverted `did' folded into the aux as do-support.
          (truthy "an aux attached to S" aux)
          (check "the aux carries do-support (`did')"
                 (and aux (getr 'word (daughter 'do aux))) 'did))

        (let* ((vp  (daughter 'vp c))
               (obj (and vp (daughter 'np vp))))
          (truthy "a VP attached to S" vp)
          (check "the verb is \"meet\""
                 (and vp (getr 'word (daughter 'verb vp))) 'meet)
          (check "the object is \"you\"" (pron-of obj) 'you)
          (truthy "final (question) punctuation attached" (daughter 'finalpunc c))

          ;; Case frame: the boy is AGENT, you is NEUTRAL -- both overt.
          (let ((cf (and vp (getr 'caseframe vp))))
            (truthy "VP has a case frame" cf)
            (check "predicate is meet" (and cf (get cf 'pred)) 'meet)
            (closeframe openframe)
            (let ((filled (cadr (first (get cf 'hypo-slots)))))
              (truthy "the frame has filled cases" filled)
              (let ((agt (assoc 'agt filled)) (neut (assoc 'neut filled)))
                (truthy "an agent case was filled" agt)
                (check "the agent is the subject NP (the boy)" (cadr agt) (daughter 'np c))
                (truthy "a neutral case was filled" neut)
                (check "the neutral case is the object (you)" (cadr neut) obj)))))))

    (format t "~&gram1-yes-no-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-yes-no-test t)
