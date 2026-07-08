;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-ditransitive-wh-test.lisp
;;;
;;; "What did Bob give Sue?" parsed end to end, FULLY DICT-DRIVEN -- the hardest
;;; wh-question case: a wh-gap competing for one of a DITRANSITIVE verb's two
;;; object slots, resolved by the grammar's semantic-preference machinery.
;;;
;;;     what_i did Bob give Sue t_i      agt = Bob, dat = Sue, neut = what
;;;
;;; `give' has two object slots (a recipient DAT and a theme NEUT). After
;;; WH-QUEST fronts `what' and subject-aux inversion makes `Bob' the overt
;;; subject, MAIN-VERB(give) leaves the wh-comp unutilized -> wh-vp. The next
;;; token is `Sue', so WH-WITH-NP-NEXT must decide which object slot the wh-comp
;;; spends on. It compares, via smqval over give's object markersets, how well
;;; `Sue' (animate) vs the wh-comp (`what') fits an object slot: `Sue' fits at
;;; least as well, so it is attached (run objects -> fills the DAT/recipient
;;; slot) and the wh-comp is kept; WH-WITH-END-NEXT then spends it on the
;;; remaining NEUT/theme slot via CREATE-WH-TRACE. Result: Sue is the recipient,
;;; the wh-gap (what) is the theme.
;;;
;;; New rules vs. the single-object object-wh case: *ditransitive-wh-rules*
;;; (WH-WITH-NP-NEXT + TOO-MANY-NPS) and the runtime port of the preference-
;;; degree thresholds much/somewhat/no (case.l 442). Everything else composes.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-ditransitive-wh-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-ditransitive-wh-test (&optional verbose)
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

      ;; Fully dict-driven: what/did/bob/give/sue/? all resolve through morpho +
      ;; defs.l (give -> a 2-object frame neut+dat; bob, sue -> hanim propnouns).
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*
                        *pronoun-rule* *qp1-done-rule* *wh-question-rules*
                        *inversion-rules* *object-wh-rules*
                        *ditransitive-wh-rules* *wh-determiner-rules*)

      (let ((ok (parse-sentence "what did bob give sue ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"what did bob give sue ?\"" ok t)
        (truthy "S is a major wh-question" (subsetp '(major quest wh-quest s) (fe c)))
        (truthy "S is np-quest" (member 'np-quest (fe c)))

        (let ((whc (getr :wh-comp c)))
          (truthy "the S has a :wh-comp register (the fronted `what')" whc)
          (truthy "the wh-element is a wh pron-np" (and whc (subsetp '(pron-np wh) (fe whc))))
          (truthy "the wh-comp is marked utilized" (and whc (member 'utilized (fe whc))))

          ;; Subject is the inverted, overt `Bob'.
          (let ((subj (daughter 'np c)))
            (truthy "the subject is overt (not a trace)"
                    (and subj (not (member 'trace (fe subj)))))
            (check "the subject is `bob'" (word-of subj) 'bob)

            (let* ((vp   (daughter 'vp c))
                   (nps  (and vp (daughters 'np vp)))
                   (trace (find-if (lambda (n) (member 'trace (fe n))) nps))
                   (sue   (find-if (lambda (n) (eq (word-of n) 'sue)) nps)))
              (truthy "a VP attached to S" vp)
              (truthy "the aux carries do-support (`did')"
                      (let ((aux (daughter 'aux c)))
                        (and aux (getr 'word (daughter 'do aux)))))
              (check "the verb is `give'" (and vp (getr 'word (daughter 'verb vp))) 'give)

              ;; Two objects: the overt `Sue' and the wh-gap trace.
              (truthy "the VP has two object NPs" (and nps (= (length nps) 2)))
              (truthy "one VP object is the overt `Sue'" sue)
              (truthy "one VP object is a trace (the wh-gap)" trace)
              (check "the wh-gap trace is bound to the wh-element (what)"
                     (and trace (getr 'binding trace)) whc)
              (truthy "final (question) punctuation attached" (daughter 'finalpunc c))

              ;; Case frame: agt = Bob (subj), dat = Sue (recipient, obj),
              ;; neut = the wh-gap (theme, obj).
              (let ((cf (and vp (getr 'caseframe vp))))
                (truthy "the VP has a case frame" cf)
                (check "the predicate is give" (and cf (get cf 'pred)) 'give)
                (closeframe openframe)
                (let ((filled (cadr (first (get cf 'hypo-slots)))))
                  (truthy "the frame has filled cases" filled)
                  (let ((agt (assoc 'agt filled))
                        (dat (assoc 'dat filled))
                        (neut (assoc 'neut filled)))
                    (truthy "an agent case was filled" agt)
                    (check "the agent is the subject `bob'" (cadr agt) subj)
                    (check "the agent was filled via the subj function" (caddr agt) 'subj)
                    (truthy "a dative (recipient) case was filled" dat)
                    (check "the recipient is `sue'" (cadr dat) sue)
                    (truthy "a neutral (theme) case was filled" neut)
                    (check "the theme is the wh-gap trace" (cadr neut) trace)))))))))

    (format t "~&gram1-ditransitive-wh-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-ditransitive-wh-test t)
