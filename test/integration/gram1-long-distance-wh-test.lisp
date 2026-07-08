;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-long-distance-wh-test.lisp
;;;
;;; "Who did you say that Bill told?" parsed end to end, FULLY DICT-DRIVEN -- a
;;; verbatim Marcus example sentence (notes/pidgin-grammar-intro.text) and one of
;;; PARSIFAL's showcase results: an UNBOUNDED (long-distance) wh-dependency,
;;; handled deterministically. `who' is questioned out of the OBJECT position of
;;; the embedded `told', across the `say that' clause boundary:
;;;
;;;     who_i did you say [ that Bill told t_i ]
;;;       matrix:   agt = you,  neut = the that-clause   (say is a that-obj verb)
;;;       embedded: agt = Bill, dat = who (the wh-gap)    (told's object)
;;;
;;; The mechanism is wh-comp INHERITANCE. WH-QUEST fronts `who' onto the MATRIX
;;; S's :wh-comp register; subject-aux inversion makes `you' the overt subject;
;;; MAIN-VERB(say) -- a that-obj verb -- opens the embedded that-clause. The new
;;; piece is the S-CREATE creation crule (*long-distance-wh-rules*): the instant
;;; the embedded that-S is created it copies the nearest enclosing S's wh-comp
;;; down into itself (FIND-WH-COMP, stopping at any intervening NP -- the
;;; complex-NP island constraint). So the embedded clause inherits `who'; its
;;; MAIN-VERB(told) sees a pending wh-comp and activates wh-vp; with the buffer
;;; exhausted at the object gap, WH-WITH-END-NEXT / CREATE-WH-TRACE drop a trace
;;; bound to that same wh-comp into `told's object slot and mark it utilized.
;;; Because matrix and embedded share ONE wh-comp node, consuming it in the
;;; embedded clause resolves the whole sentence.
;;;
;;; New vs. everything before: *long-distance-wh-rules* (S-CREATE) + the
;;; FIND-WH-COMP helper. Everything else composes: *that-complement-rules*
;;; (the embedded that-clause), *inf-complement-rules* (COMP-TO-NP / NP-S
;;; attachment of the clause to `say'), *wh-question-rules* / *inversion-rules*
;;; / *object-wh-rules* (the wh-question front end + wh-vp gap-filling).
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-long-distance-wh-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-long-distance-wh-test (&optional verbose)
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
           ;; Subjects here are a pronoun (`you') and a common-noun-path proper
           ;; name (`Bill'); accept either realization.
           (np-word (np)
             (and np (or (getr 'word (daughter 'noun (daughter 'nbar np)))
                         (getr 'word (daughter 'pronoun np))
                         (getr 'word (daughter 'name np))))))

      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*
                        *pronoun-rule* *qp1-done-rule* *wh-question-rules*
                        *inversion-rules* *object-wh-rules*
                        *that-complement-rules* *inf-complement-rules*
                        *long-distance-wh-rules*)

      ;; Sanity: `say' is a that-obj verb (jlike tell) -- it can open the
      ;; embedded that-clause the wh has to cross.
      (expandsim 'say)
      (truthy "dict `say' is a that-obj verb" (member 'that-obj (get 'say 'features)))

      (let ((ok (parse-sentence "who did you say that bill told ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"who did you say that bill told ?\"" ok t)
        (truthy "matrix S is a major wh-question" (subsetp '(major quest wh-quest s) (fe c)))
        (truthy "matrix S is np-quest" (member 'np-quest (fe c)))

        (let ((whc (getr :wh-comp c)))
          (truthy "the matrix S has a :wh-comp register (`who')" whc)
          (truthy "the wh-element is a wh pron-np" (and whc (subsetp '(pron-np wh) (fe whc))))
          (truthy "the wh-comp is marked utilized" (and whc (member 'utilized (fe whc))))

          ;; --- Matrix clause: you say [that-clause] ---
          (check "the matrix subject is `you'" (np-word (daughter 'np c)) 'you)
          (let* ((vp     (daughter 'vp c))
                 (compnp (and vp (daughter 'np vp)))
                 (embs   (and compnp (daughter 's compnp))))
            (truthy "a matrix VP attached" vp)
            (check "the matrix verb is `say'" (and vp (getr 'word (daughter 'verb vp))) 'say)
            (truthy "the matrix object is a comp-np" (and compnp (member 'comp-np (fe compnp))))
            (check "the comp-np is marked that-comp"
                   (and compnp (getr 'markers compnp)) '(that-comp))

            ;; --- Embedded clause: that Bill told t ---
            (truthy "the comp-np dominates an embedded S" embs)
            (truthy "the embedded S is a comp-s / that-s"
                    (and embs (subsetp '(sec comp-s that-s s) (fe embs))))
            (check "the embedded subject is `bill'" (np-word (daughter 'np embs)) 'bill)

            (let* ((evp    (and embs (daughter 'vp embs)))
                   (enps   (and evp (daughters 'np evp)))
                   (etrace (find-if (lambda (n) (member 'trace (fe n))) enps)))
              (check "the embedded verb is `tell' (told)"
                     (and evp (getr 'word (daughter 'verb evp))) 'told)
              (truthy "the embedded object is a trace (the wh-gap)" etrace)
              ;; The long-distance link: the embedded gap is bound to the SAME
              ;; wh-comp the matrix S fronted.
              (check "the embedded gap is bound to the matrix wh-comp (who)"
                     (and etrace (getr 'binding etrace)) whc)

              (truthy "final (question) punctuation attached" (daughter 'finalpunc c))

              ;; --- Case frames: matrix say(agt=you, neut=clause);
              ;;     embedded tell(agt=Bill, dat=who-gap). ---
              (let ((mcf (and vp (getr 'caseframe vp)))
                    (ecf (and evp (getr 'caseframe evp))))
                (truthy "matrix VP has a case frame" mcf)
                (check "matrix predicate is say" (and mcf (get mcf 'pred)) 'say)
                (truthy "embedded VP has a case frame" ecf)
                (check "embedded predicate is tell" (and ecf (get ecf 'pred)) 'tell)
                (closeframe openframe)
                (let ((mfilled (cadr (first (get mcf 'hypo-slots))))
                      (efilled (cadr (first (get ecf 'hypo-slots)))))
                  (let ((magt (assoc 'agt mfilled)) (mneut (assoc 'neut mfilled)))
                    (truthy "matrix agent filled" magt)
                    (check "the matrix agent is `you'" (cadr magt) (daughter 'np c))
                    (truthy "matrix neutral filled" mneut)
                    (check "the matrix neutral is the embedded clause (comp-np)"
                           (cadr mneut) compnp))
                  (let ((eagt (assoc 'agt efilled)) (edat (assoc 'dat efilled)))
                    (truthy "embedded agent filled" eagt)
                    (check "the embedded agent is `Bill'" (cadr eagt) (daughter 'np embs))
                    (truthy "embedded dative (the told-ee) filled" edat)
                    (check "the embedded dative is the wh-gap trace" (cadr edat) etrace)))))))))

    (format t "~&gram1-long-distance-wh-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-long-distance-wh-test t)
