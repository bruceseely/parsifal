;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-promise-wh-control-test.lisp
;;;
;;; "Who did you promise to give the book to?" parsed end to end, FULLY
;;; DICT-DRIVEN -- a verbatim Marcus example sentence (pidgin-grammar-intro.text)
;;; and the most layered construction in the suite. It composes FOUR mechanisms
;;; at once, with no new rules:
;;;
;;;   * SUBJECT control       -- promise (subj-binds-delta) binds the embedded
;;;                              infinitive's delta subject to the matrix SUBJECT;
;;;   * long-distance wh      -- the wh-comp is inherited (S-CREATE) down into
;;;                              the infinitive complement;
;;;   * preposition stranding -- `who' is the object of the clause-final `to';
;;;   * ditransitive `give'   -- `the book' is the theme, `to who' the recipient.
;;;
;;;     who_i did you promise [ delta_j to give the book [to t_i] ]
;;;       matrix:   agt = you,  neut = the infinitive clause
;;;       delta_j = you   (SUBJECT control: the promiser is the giver)
;;;       embedded: agt = you (delta), neut = the book, dat = who (recipient, via
;;;                 the stranded `to') -- and who is the matrix wh-gap.
;;;
;;; So one trace (the embedded subject) reaches UP to the matrix subject by
;;; control, while another (the stranded `to's object) reaches UP to the matrix
;;; wh-comp by long-distance movement -- in the same embedded clause. promise
;;; is subj-binds-delta, so MAIN-VERB activates 2-obj-inf-comp; CREATE-DELTA-SUBJ
;;; drops the delta and INF-S-START1 builds the infinitive; S-CREATE seeds that
;;; new inf-S with the matrix wh-comp; inside it, give -> wh-vp, `the book' is
;;; the theme, and at the stranded `to' WH-PP-BUILD binds a trace to the inherited
;;; wh-comp filling DAT; the COMPLETE VP-NP's subj-binds-delta arm then binds the
;;; delta to the matrix subject.
;;;
;;; NO new rules -- composition of *two-object-inf-rules* + *delta-complement-rules*
;;; + *vp-np-full-rule* (subject control) + *long-distance-wh-rules* (S-CREATE
;;; inheritance, here into an INFINITIVE, not a that-clause) + *wh-pp-rules*
;;; (stranding) + *ditransitive-wh-rules* + *inf-complement-rules* + *pp-rules*
;;; + the wh / inversion front end.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-promise-wh-control-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-promise-wh-control-test (&optional verbose)
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
                        *two-object-inf-rules* *delta-complement-rules*
                        *long-distance-wh-rules* *ditransitive-wh-rules*
                        *wh-pp-rules* *pp-rules*)

      ;; Sanity: promise is subj-binds-delta (SUBJECT control) in the dictionary.
      (expandsim 'promise)
      (truthy "dict `promise' is subj-binds-delta"
              (member 'subj-binds-delta (get 'promise 'features)))
      (truthy "dict `promise' is NOT obj-binds-delta"
              (not (member 'obj-binds-delta (get 'promise 'features))))

      (let ((ok (parse-sentence "who did you promise to give the book to ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"who did you promise to give the book to ?\"" ok t)
        (truthy "matrix S is a major wh-question" (subsetp '(major quest wh-quest s) (fe c)))

        (let ((whc (getr :wh-comp c)))
          (truthy "the matrix S has a :wh-comp register (`who')" whc)
          (truthy "the wh-comp is marked utilized" (and whc (member 'utilized (fe whc))))

          ;; --- Matrix clause: you promise [inf clause] ---
          (check "the matrix subject is `you'" (np-word (daughter 'np c)) 'you)
          (let* ((vp     (daughter 'vp c))
                 (compnp (and vp (daughter 'np vp)))
                 (embs   (and compnp (daughter 's compnp))))
            (check "the matrix verb is `promise'"
                   (and vp (getr 'word (daughter 'verb vp))) 'promise)
            (truthy "the matrix object is a comp-np" (and compnp (member 'comp-np (fe compnp))))
            (check "the comp-np is marked inf-comp"
                   (and compnp (getr 'markers compnp)) '(inf-comp))

            ;; --- Embedded infinitive: delta(=you) give the book to t_who ---
            (truthy "the comp-np dominates an embedded inf-S"
                    (and embs (subsetp '(sec comp-s inf-s s) (fe embs))))
            (let* ((esubj (and embs (daughter 'np embs)))
                   (evp   (and embs (daughter 'vp embs)))
                   (enps  (and evp (daughters 'np evp)))
                   (book  (find-if (lambda (n) (eq (np-word n) 'book)) enps))
                   (epps  (and evp (daughters 'pp evp)))
                   (pp    (car epps))
                   (ppobj (and pp (daughter 'np pp))))
              (check "the embedded verb is `give'"
                     (and evp (getr 'word (daughter 'verb evp))) 'give)

              ;; Subject control: the embedded delta subject is bound UP to `you'.
              (truthy "the embedded subject is a delta" (and esubj (member 'delta (fe esubj))))
              (check "the embedded delta is controlled by the matrix subject `you'"
                     (and esubj (getr 'binding esubj)) (daughter 'np c))

              ;; The theme is `the book'.
              (truthy "the embedded theme is `the book'" book)

              ;; Long-distance stranding: the stranded `to's object is bound UP to
              ;; the matrix wh-comp.
              (truthy "the embedded VP has exactly one PP (the stranded `to')"
                      (and epps (= (length epps) 1)))
              (check "the stranded preposition is `to'"
                     (and pp (getr 'word (daughter 'prep pp))) 'to)
              (truthy "the stranded PP's object is a trace" (and ppobj (member 'trace (fe ppobj))))
              (check "the stranded PP's trace is bound to the matrix wh-comp (who)"
                     (and ppobj (getr 'binding ppobj)) whc)

              (truthy "final (question) punctuation attached" (daughter 'finalpunc c))

              ;; --- Case frames ---
              (let ((mcf (and vp (getr 'caseframe vp)))
                    (ecf (and evp (getr 'caseframe evp))))
                (check "matrix predicate is promise" (and mcf (get mcf 'pred)) 'promise)
                (check "embedded predicate is give" (and ecf (get ecf 'pred)) 'give)
                (closeframe openframe)
                (let ((mfilled (cadr (first (get mcf 'hypo-slots))))
                      (efilled (cadr (first (get ecf 'hypo-slots)))))
                  (let ((magt (assoc 'agt mfilled)) (mneut (assoc 'neut mfilled)))
                    (truthy "matrix agent filled" magt)
                    (check "the matrix agent is `you'" (cadr magt) (daughter 'np c))
                    (truthy "matrix neutral filled" mneut)
                    (check "the matrix neutral is the inf clause (comp-np)" (cadr mneut) compnp))
                  (let ((eagt  (assoc 'agt efilled))
                        (eneut (assoc 'neut efilled))
                        (edat  (assoc 'dat efilled)))
                    (truthy "embedded agent (the giver) filled" eagt)
                    (check "the embedded agent is the delta subject (= you, by control)"
                           (cadr eagt) esubj)
                    (truthy "embedded neutral (the theme) filled" eneut)
                    (check "the embedded theme is `the book'" (cadr eneut) book)
                    (truthy "embedded dative (the recipient) filled" edat)
                    (check "the recipient is the stranded PP's wh-gap trace" (cadr edat) ppobj)
                    (check "the recipient was filled via the preposition `to'"
                           (caddr edat) 'to)))))))))

    (format t "~&gram1-promise-wh-control-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-promise-wh-control-test t)
