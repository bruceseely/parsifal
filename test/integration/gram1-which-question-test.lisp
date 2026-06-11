;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-which-question-test.lisp
;;;
;;; "Which boy broke the jar?" parsed end to end -- a SUBJECT wh-question whose
;;; wh-element is a QUANTIFIER PHRASE ("which boy"), not a bare pronoun. This
;;; exercises the gram3 quantifier-phrase construction (a quant fills an NP's
;;; qp slot) on top of the already-working subject-wh machinery.
;;;
;;;     [which boy]_i [ t_i broke the jar ]   -- agent = which boy, neut = jar
;;;
;;; Flow: `which' carries only *which (its quantity is a register), so it is
;;; invisible to AS until WHICH-DIAGN (cpool) diagnoses it -- with no NP above,
;;; it becomes quant + ngstart + wh. STARTNP then opens an NP (no det ->
;;; parse-qp-1); QUANT (parse-qp-1) attaches `which' as the qp and propagates
;;; `wh' onto the NP; `boy' completes the nbar, so "which boy" is a wh-NP.
;;; WH-QUEST ([=wh][=verb]) fronts that whole NP into the clause whcomp /
;;; :wh-comp register and labels the clause np-quest; the empty subject is
;;; filled by CREATE-WH-TRACE (a trace bound to the wh-comp); the trace's
;;; binding (the "which boy" NP) fills the verb's AGENT case.
;;;
;;; The only new rules vs. "Who broke the jar?" are the quantifier-phrase
;;; layer (*quantifier-rules*: QUANT + DET-QUANT) and *which-rules*
;;; (WHICH-DIAGN); everything else composes.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-which-question-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-which-question-test (&optional verbose)
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

      ;; Mostly dict-driven; only `jar' is missing from defs.l.
      (reset-rule-table)
      (df jar feats (noun ns n3p) markers (physob inanim))
      (expandsim 'jar)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*
                        *qp1-done-rule* *quantifier-rules* *which-rules*
                        *wh-question-rules*)

      (let ((ok (parse-sentence "which boy broke the jar ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"which boy broke the jar ?\"" ok t)

        ;; The clause is a wh-question (np-quest); the fronted wh-element is the
        ;; quantifier-phrase NP "which boy".
        (truthy "S is a major wh-question" (subsetp '(major quest wh-quest s) (fe c)))
        (truthy "S is np-quest" (member 'np-quest (fe c)))
        (let ((whc (getr :wh-comp c)))
          (truthy "the S has a :wh-comp register (the fronted wh-element)" whc)
          (truthy "the wh-element is an NP" (and whc (member 'np (fe whc))))
          (truthy "the wh-element carries the `wh' feature" (and whc (member 'wh (fe whc))))
          (check "the wh-element's noun is `boy'" (word-of whc) 'boy)
          (truthy "the wh-comp is marked utilized" (and whc (member 'utilized (fe whc))))

          ;; The wh-NP is a quantifier phrase: a qp whose quant is `which'.
          (let* ((qp (and whc (daughter 'qp whc))))
            (truthy "the wh-element has a qp daughter" qp)
            (check "the qp's quant is `which'"
                   (and qp (getr 'word (daughter 'quant qp))) 'which))

          ;; The subject is a trace bound to the wh-element (which boy).
          (let ((subj (daughter 'np c)))
            (truthy "the subject is a trace" (and subj (member 'trace (fe subj))))
            (check "the subject trace is bound to the wh-element"
                   (and subj (getr 'binding subj)) whc)

            (let* ((vp  (daughter 'vp c))
                   (obj (and vp (daughter 'np vp))))
              (truthy "a VP attached to S" vp)
              (check "the verb is \"break\""
                     (and vp (getr 'word (daughter 'verb vp))) 'broke)
              (check "the object is \"jar\"" (word-of obj) 'jar)
              (truthy "final (question) punctuation attached" (daughter 'finalpunc c))

              ;; Case frame: the wh-element (which boy) is the AGENT, jar NEUT.
              (let ((cf (and vp (getr 'caseframe vp))))
                (truthy "the VP has a case frame" cf)
                (check "the predicate is break" (and cf (get cf 'pred)) 'break)
                (closeframe openframe)
                (let ((filled (cadr (first (get cf 'hypo-slots)))))
                  (truthy "the frame has filled cases" filled)
                  (let ((agt (assoc 'agt filled)) (neut (assoc 'neut filled)))
                    (truthy "an agent case was filled" agt)
                    (check "the agent is the wh-element `which boy'" (cadr agt) whc)
                    (check "the agent was filled via the subj function" (caddr agt) 'subj)
                    (truthy "a neutral case was filled" neut)
                    (check "the neutral case is `jar'" (word-of (cadr neut)) 'jar)))))))))

    (format t "~&gram1-which-question-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-which-question-test t)
