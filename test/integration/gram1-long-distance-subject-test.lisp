;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-long-distance-subject-test.lisp
;;;
;;; "Who did you say scheduled the meeting?" parsed end to end, FULLY
;;; DICT-DRIVEN -- a verbatim Marcus example (pidgin-grammar-intro.text) and the
;;; SUBJECT-gap mirror of the long-distance OBJECT test ("Who did you say that
;;; Bill told?"). Here `who' is questioned out of the embedded clause's SUBJECT:
;;;
;;;     who_i did you say [ t_i scheduled the meeting ]
;;;       matrix:   agt = you,  neut = the embedded clause   (say: that-obj)
;;;       embedded: agt = who (the wh-gap, the scheduler), neut = the meeting
;;;
;;; Two things distinguish it from the object-gap case, and BOTH fall out of the
;;; same wh-comp-inheritance machinery with NO new rules:
;;;
;;;   * The gap is the embedded SUBJECT. S-CREATE copies the matrix wh-comp down
;;;     into the embedded clause; with the subject position empty, the embedded
;;;     wh-pool's CREATE-WH-TRACE drops a trace bound to that inherited wh-comp
;;;     into the subject slot -- exactly as a root subject wh-question ("who
;;;     broke the jar?") fills its own subject, but one clause down.
;;;   * There is NO `that'. English bars a complementiser immediately before an
;;;     extracted subject (the *that-trace effect): "*Who did you say THAT
;;;     scheduled the meeting?". The grammar opens the complement via the
;;;     dropped-`that' path, so the embedded S has no comp daughter -- yet it is
;;;     still analysed as a that-comp clause.
;;;
;;; The rule groups are IDENTICAL to the object-gap test; only the sentence (and
;;; hence which slot the inherited wh-comp lands in) differs. Composition of
;;; *long-distance-wh-rules* (S-CREATE) + *wh-question-rules* (CREATE-WH-TRACE /
;;; wh-pool) + *that-complement-rules* + *inf-complement-rules* + the wh /
;;; inversion front end.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-long-distance-subject-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-long-distance-subject-test (&optional verbose)
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
             (and np (or (getr 'word (daughter 'noun (daughter 'nbar np)))
                         (getr 'word (daughter 'pronoun np))))))

      ;; Same rule groups as the long-distance OBJECT test -- the difference is
      ;; entirely in the sentence (subject gap vs object gap).
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*
                        *pronoun-rule* *qp1-done-rule* *wh-question-rules*
                        *inversion-rules* *object-wh-rules*
                        *that-complement-rules* *inf-complement-rules*
                        *long-distance-wh-rules*)

      (let ((ok (parse-sentence "who did you say scheduled the meeting ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"who did you say scheduled the meeting ?\"" ok t)
        (truthy "matrix S is a major wh-question" (subsetp '(major quest wh-quest s) (fe c)))
        (truthy "matrix S is np-quest" (member 'np-quest (fe c)))

        (let ((whc (getr :wh-comp c)))
          (truthy "the matrix S has a :wh-comp register (`who')" whc)
          (truthy "the wh-element is a wh pron-np" (and whc (subsetp '(pron-np wh) (fe whc))))
          (truthy "the wh-comp is marked utilized" (and whc (member 'utilized (fe whc))))

          ;; --- Matrix clause: you say [embedded clause] ---
          (check "the matrix subject is `you'" (np-word (daughter 'np c)) 'you)
          (let* ((vp     (daughter 'vp c))
                 (compnp (and vp (daughter 'np vp)))
                 (embs   (and compnp (daughter 's compnp))))
            (check "the matrix verb is `say'" (and vp (getr 'word (daughter 'verb vp))) 'say)
            (truthy "the matrix object is a comp-np" (and compnp (member 'comp-np (fe compnp))))
            (check "the comp-np is marked that-comp"
                   (and compnp (getr 'markers compnp)) '(that-comp))

            ;; --- Embedded clause: [wh-gap] scheduled the meeting, NO `that' ---
            (truthy "the comp-np dominates an embedded comp-s / that-s"
                    (and embs (subsetp '(sec comp-s that-s s) (fe embs))))
            (truthy "the embedded clause has NO overt complementiser (that-trace effect)"
                    (and embs (null (daughter 'comp embs))))

            (let* ((esubj (and embs (daughter 'np embs)))
                   (evp   (and embs (daughter 'vp embs))))
              (check "the embedded verb is `schedule' (scheduled)"
                     (and evp (getr 'word (daughter 'verb evp))) 'scheduled)

              ;; The long-distance SUBJECT link: the embedded SUBJECT is a trace
              ;; bound to the matrix wh-comp.
              (truthy "the embedded subject is a trace" (and esubj (member 'trace (fe esubj))))
              (check "the embedded subject trace is bound to the matrix wh-comp (who)"
                     (and esubj (getr 'binding esubj)) whc)

              (check "the embedded object is `the meeting'"
                     (np-word (daughter 'np evp)) 'meeting)
              (truthy "final (question) punctuation attached" (daughter 'finalpunc c))

              ;; --- Case frames: matrix say(agt=you, neut=clause);
              ;;     embedded schedule(agt=who-gap-subject, neut=the meeting). ---
              (let ((mcf (and vp (getr 'caseframe vp)))
                    (ecf (and evp (getr 'caseframe evp))))
                (check "matrix predicate is say" (and mcf (get mcf 'pred)) 'say)
                (check "embedded predicate is schedule" (and ecf (get ecf 'pred)) 'schedule)
                (closeframe openframe)
                (let ((mfilled (cadr (first (get mcf 'hypo-slots))))
                      (efilled (cadr (first (get ecf 'hypo-slots)))))
                  (let ((magt (assoc 'agt mfilled)) (mneut (assoc 'neut mfilled)))
                    (truthy "matrix agent filled" magt)
                    (check "the matrix agent is `you'" (cadr magt) (daughter 'np c))
                    (truthy "matrix neutral filled" mneut)
                    (check "the matrix neutral is the embedded clause (comp-np)"
                           (cadr mneut) compnp))
                  (let ((eagt (assoc 'agt efilled)) (eneut (assoc 'neut efilled)))
                    (truthy "embedded agent (the scheduler) filled" eagt)
                    ;; The unifying fact: the embedded AGENT is `who'. The subject
                    ;; slot resolves the trace to its referent (via VP-VERB), so
                    ;; the frame holds the wh-element itself -- which is exactly
                    ;; what the embedded subject trace is bound to.
                    (check "the embedded agent is the wh-element `who'" (cadr eagt) whc)
                    (check "...and `who' is what the embedded subject trace is bound to"
                           (cadr eagt) (getr 'binding esubj))
                    (check "the embedded agent was filled via the subj function" (caddr eagt) 'subj)
                    (truthy "embedded neutral (the theme) filled" eneut)
                    (check "the embedded theme is `the meeting'"
                           (np-word (cadr eneut)) 'meeting)))))))))

    (format t "~&gram1-long-distance-subject-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-long-distance-subject-test t)
