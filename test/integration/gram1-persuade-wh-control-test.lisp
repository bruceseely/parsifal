;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-persuade-wh-control-test.lisp
;;;
;;; "Who did you persuade to do it?" parsed end to end, FULLY DICT-DRIVEN -- a
;;; verbatim Marcus example sentence (notes/pidgin-grammar-intro.text). It folds
;;; together three mechanisms the earlier tests built separately:
;;;
;;;   * object wh-extraction  -- `who' is questioned out of persuade's OBJECT;
;;;   * object control        -- persuade (obj-binds-delta) binds the delta
;;;                              subject of its infinitive complement to its
;;;                              object;
;;;   * and the twist that unifies them: the controller IS the wh-gap.
;;;
;;;     who_i did you persuade t_i [ delta_i to do it ]
;;;       agt = you, dat = who (the persuadee / wh-gap), neut = the inf clause;
;;;       delta_i = who  (object control: the persuadee does the doing)
;;;       it = object of `do'
;;;
;;; Derivation: WH-QUEST fronts `who'; inversion makes `you' the subject;
;;; MAIN-VERB(persuade) -- obj-binds-delta, hence 2-obj-inf-obj -> inf-obj --
;;; activates 2-obj-inf-comp AND, with the wh-comp pending, wh-vp. There is no
;;; overt object NP (the buffer is [to][do][it]), so WH-WITH-END-NEXT runs
;;; CREATE-WH-TRACE: a trace bound to the wh-comp becomes persuade's object;
;;; WH-RESOLVED returns to ss-vp and OBJECTS attaches it. Then CREATE-DELTA-SUBJ
;;; drops the delta subject and INF-S-START1 builds `to do it'; finally the
;;; COMPLETE VP-NP crule's obj-binds-delta arm binds that delta to persuade's
;;; INDIRECT object -- which is the wh-trace. So `who' is at once persuade's
;;; object and the embedded subject's controller.
;;;
;;; NO new rules -- pure composition of *object-wh-rules* (wh-vp gap filling) +
;;; *two-object-inf-rules* (CREATE-DELTA-SUBJ) + *delta-complement-rules* +
;;; *vp-np-full-rule* (the obj-binds-delta binding) + *inf-complement-rules* +
;;; the wh-question / inversion front end.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-persuade-wh-control-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-persuade-wh-control-test (&optional verbose)
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
           (pron-word (np)
             (and np (or (getr 'word (daughter 'pronoun np))
                         (getr 'word (daughter 'noun (daughter 'nbar np)))))))

      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-full-rule*
                        *pronoun-rule* *qp1-done-rule* *wh-question-rules*
                        *inversion-rules* *object-wh-rules* *inf-complement-rules*
                        *two-object-inf-rules* *delta-complement-rules*)

      ;; Sanity: persuade is obj-binds-delta (object control) in the dictionary.
      (expandsim 'persuade)
      (truthy "dict `persuade' is obj-binds-delta"
              (member 'obj-binds-delta (get 'persuade 'features)))

      (let ((ok (parse-sentence "who did you persuade to do it ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"who did you persuade to do it ?\"" ok t)
        (truthy "S is a major wh-question" (subsetp '(major quest wh-quest s) (fe c)))
        (truthy "S is np-quest" (member 'np-quest (fe c)))

        (let ((whc (getr :wh-comp c)))
          (truthy "the S has a :wh-comp register (`who')" whc)
          (truthy "the wh-comp is marked utilized" (and whc (member 'utilized (fe whc))))

          (check "the subject is the inverted overt `you'" (pron-word (daughter 'np c)) 'you)

          (let* ((vp     (daughter 'vp c))
                 (nps    (and vp (daughters 'np vp)))
                 (trace  (find-if (lambda (n) (member 'trace (fe n))) nps))
                 (compnp (find-if (lambda (n) (member 'comp-np (fe n))) nps)))
            (truthy "a VP attached" vp)
            (check "the verb is `persuade'"
                   (and vp (getr 'word (daughter 'verb vp))) 'persuade)

            ;; Two objects: the wh-gap trace (the persuadee) and the inf clause.
            (truthy "the VP has two object NPs" (and nps (= (length nps) 2)))
            (truthy "one VP object is a trace (the wh-gap, the persuadee)" trace)
            (check "the wh-gap trace is bound to the wh-element (who)"
                   (and trace (getr 'binding trace)) whc)
            (truthy "the other VP object is a comp-np" compnp)
            (check "the comp-np is marked inf-comp"
                   (and compnp (getr 'markers compnp)) '(inf-comp))

            ;; Embedded clause: delta subject CONTROLLED BY THE WH-GAP, `do it'.
            (let* ((embs  (and compnp (daughter 's compnp)))
                   (esubj (and embs (daughter 'np embs)))
                   (evp   (and embs (daughter 'vp embs))))
              (truthy "the comp-np dominates an embedded inf-S"
                      (and embs (subsetp '(sec comp-s inf-s s) (fe embs))))
              (check "the embedded verb is `do'"
                     (and evp (getr 'word (daughter 'verb evp))) 'do)
              (truthy "the embedded subject is a delta" (and esubj (member 'delta (fe esubj))))
              (check "the embedded delta is controlled by the wh-gap (object control)"
                     (and esubj (getr 'binding esubj)) trace)
              (check "the embedded object is `it'"
                     (pron-word (daughter 'np evp)) 'it)

              (truthy "final (question) punctuation attached" (daughter 'finalpunc c))

              ;; Matrix case frame: agt = you, dat = who (wh-gap), neut = clause;
              ;; the delta's controller is exactly the DAT.
              (let ((cf (and vp (getr 'caseframe vp))))
                (truthy "the VP has a case frame" cf)
                (check "the predicate is persuade" (and cf (get cf 'pred)) 'persuade)
                (closeframe openframe)
                (let ((filled (cadr (first (get cf 'hypo-slots)))))
                  (truthy "the frame has filled cases" filled)
                  (let ((agt  (assoc 'agt filled))
                        (dat  (assoc 'dat filled))
                        (neut (assoc 'neut filled)))
                    (truthy "an agent case was filled" agt)
                    (check "the agent is the subject `you'" (cadr agt) (daughter 'np c))
                    (truthy "a dative case was filled" dat)
                    (check "the dative is the wh-gap trace (the persuadee)" (cadr dat) trace)
                    (truthy "a neutral case was filled" neut)
                    (check "the neutral is the inf clause (comp-np)" (cadr neut) compnp)
                    ;; The unifying fact: the embedded subject's controller is the
                    ;; matrix dative, which is the questioned `who'.
                    (check "the embedded delta's controller is the matrix dative"
                           (getr 'binding esubj) (cadr dat))))))))))

    (format t "~&gram1-persuade-wh-control-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-persuade-wh-control-test t)
