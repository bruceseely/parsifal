;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-ask-wh-control-test.lisp
;;;
;;; "Who did you ask to schedule the meeting?" parsed end to end, FULLY
;;; DICT-DRIVEN -- a verbatim Marcus example. It is the `ask' sibling of the
;;; persuade test (gram1-persuade-wh-control-test): `ask' is `jlike persuade',
;;; hence obj-binds-delta, so it shows the same wh-extraction-meets-object-control
;;; interaction with a different verb and a richer infinitive complement:
;;;
;;;     who_i did you ask t_i [ delta_i to schedule the meeting ]
;;;       agt = you, dat = who (the askee / wh-gap), neut = the inf clause;
;;;       delta_i = who   (object control: the askee does the scheduling);
;;;       the meeting = object (neut) of the embedded `schedule'.
;;;
;;; As with persuade, `who' is at once ask's object and the embedded subject's
;;; controller: ask (obj-binds-delta -> 2-obj-inf-obj) activates 2-obj-inf-comp
;;; and, with the wh-comp pending, wh-vp; with no overt object NP (the buffer is
;;; [to][schedule]...), WH-WITH-END-NEXT runs CREATE-WH-TRACE so a trace bound to
;;; the wh-comp becomes ask's object; CREATE-DELTA-SUBJ + INF-S-START1 build `to
;;; schedule the meeting'; and the COMPLETE VP-NP's obj-binds-delta arm binds the
;;; delta to ask's indirect object -- the wh-trace. (The `to' infinitive blocks
;;; the competing indirect-question reading that ask's indirect-q-obj feature
;;; would otherwise allow -- "ask who scheduled..." is tensed, this is not.)
;;;
;;; NO new rules -- same composition as the persuade test (*object-wh-rules* +
;;; *two-object-inf-rules* + *delta-complement-rules* + *vp-np-full-rule* +
;;; *inf-complement-rules* + the wh / inversion front end).
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-ask-wh-control-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-ask-wh-control-test (&optional verbose)
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
           (np-word (np)
             (and np (or (getr 'word (daughter 'pronoun np))
                         (getr 'word (daughter 'noun (daughter 'nbar np)))))))

      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-full-rule*
                        *pronoun-rule* *qp1-done-rule* *wh-question-rules*
                        *inversion-rules* *object-wh-rules* *inf-complement-rules*
                        *two-object-inf-rules* *delta-complement-rules*)

      ;; Sanity: ask is obj-binds-delta (object control), inherited from persuade.
      (expandsim 'ask)
      (truthy "dict `ask' is obj-binds-delta"
              (member 'obj-binds-delta (get 'ask 'features)))

      (let ((ok (parse-sentence "who did you ask to schedule the meeting ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"who did you ask to schedule the meeting ?\"" ok t)
        (truthy "S is a major wh-question" (subsetp '(major quest wh-quest s) (fe c)))
        (truthy "S is np-quest" (member 'np-quest (fe c)))

        (let ((whc (getr :wh-comp c)))
          (truthy "the S has a :wh-comp register (`who')" whc)
          (truthy "the wh-comp is marked utilized" (and whc (member 'utilized (fe whc))))

          (check "the subject is the inverted overt `you'" (np-word (daughter 'np c)) 'you)

          (let* ((vp     (daughter 'vp c))
                 (nps    (and vp (daughters 'np vp)))
                 (trace  (find-if (lambda (n) (member 'trace (fe n))) nps))
                 (compnp (find-if (lambda (n) (member 'comp-np (fe n))) nps)))
            (truthy "a VP attached" vp)
            (check "the verb is `ask'" (and vp (getr 'word (daughter 'verb vp))) 'ask)

            ;; Two objects: the wh-gap trace (the askee) and the inf clause.
            (truthy "the VP has two object NPs" (and nps (= (length nps) 2)))
            (truthy "one VP object is a trace (the wh-gap, the askee)" trace)
            (check "the wh-gap trace is bound to the wh-element (who)"
                   (and trace (getr 'binding trace)) whc)
            (truthy "the other VP object is a comp-np" compnp)
            (check "the comp-np is marked inf-comp"
                   (and compnp (getr 'markers compnp)) '(inf-comp))

            ;; Embedded clause: delta CONTROLLED BY THE WH-GAP, `schedule the meeting'.
            (let* ((embs  (and compnp (daughter 's compnp)))
                   (esubj (and embs (daughter 'np embs)))
                   (evp   (and embs (daughter 'vp embs))))
              (truthy "the comp-np dominates an embedded inf-S"
                      (and embs (subsetp '(sec comp-s inf-s s) (fe embs))))
              (check "the embedded verb is `schedule'"
                     (and evp (getr 'word (daughter 'verb evp))) 'schedule)
              (truthy "the embedded subject is a delta" (and esubj (member 'delta (fe esubj))))
              (check "the embedded delta is controlled by the wh-gap (object control)"
                     (and esubj (getr 'binding esubj)) trace)
              (check "the embedded object is `the meeting'"
                     (np-word (daughter 'np evp)) 'meeting)

              (truthy "final (question) punctuation attached" (daughter 'finalpunc c))

              ;; Matrix case frame: agt = you, dat = who (wh-gap), neut = clause;
              ;; the delta's controller is exactly the DAT.
              (let ((cf (and vp (getr 'caseframe vp))))
                (truthy "the VP has a case frame" cf)
                (check "the predicate is ask" (and cf (get cf 'pred)) 'ask)
                (closeframe openframe)
                (let ((filled (cadr (first (get cf 'hypo-slots)))))
                  (truthy "the frame has filled cases" filled)
                  (let ((agt  (assoc 'agt filled))
                        (dat  (assoc 'dat filled))
                        (neut (assoc 'neut filled)))
                    (truthy "an agent case was filled" agt)
                    (check "the agent is the subject `you'" (cadr agt) (daughter 'np c))
                    (truthy "a dative case was filled" dat)
                    (check "the dative is the wh-gap trace (the askee)" (cadr dat) trace)
                    (truthy "a neutral case was filled" neut)
                    (check "the neutral is the inf clause (comp-np)" (cadr neut) compnp)
                    (check "the embedded delta's controller is the matrix dative"
                           (getr 'binding esubj) (cadr dat))))))))))

    (format t "~&gram1-ask-wh-control-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-ask-wh-control-test t)
