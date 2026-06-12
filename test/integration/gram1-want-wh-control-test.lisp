;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-want-wh-control-test.lisp
;;;
;;; "Who do you want to give a book to?" parsed end to end, FULLY DICT-DRIVEN.
;;; This is the bare-adjunct-free core of Marcus's example "Who do you want to
;;; give a book to tomorrow?" -- the temporal adjunct `tomorrow' needs the gram4
;;; time-NP machinery, which is a separate, not-yet-ported construction (it also
;;; blocks "What did you give Sue yesterday?"); everything else parses cleanly.
;;;
;;; It is the SUBJECT-CONTROL-via-SUBJ-LESS sibling of the promise test: where
;;; `promise' is subj-binds-delta (a 2-object control verb), `want' is
;;; subj-less-inf-obj -- it simply has no embedded subject, and the delta is
;;; supplied and bound to the matrix subject. Combined here with long-distance
;;; preposition stranding:
;;;
;;;     who_i do you want [ delta_j to give a book [to t_i] ]
;;;       matrix:   agt = you,  neut = the infinitive clause
;;;       delta_j = you   (subject control, the subj-less-inf-obj path)
;;;       embedded: agt = you (delta), neut = a book, dat = who (recipient, via
;;;                 the stranded `to') -- and who is the matrix wh-gap.
;;;
;;; So, as in the promise case, two traces in one embedded clause reach UP by
;;; different routes -- the delta subject to the matrix subject by control, the
;;; stranded `to's object to the matrix wh-comp by long-distance movement -- but
;;; the control here runs through CREATE-DELTA-SUBJ-1 (subj-less-inf-comp) and
;;; the subj-less-inf-obj arm of the COMPLETE VP-NP crule, not the 2-object
;;; obj/subj-binds-delta machinery.
;;;
;;; NO new rules -- composition of *delta-complement-rules* (subj-less control)
;;; + *vp-np-full-rule* + *long-distance-wh-rules* (S-CREATE into the infinitive)
;;; + *wh-pp-rules* (stranding) + *ditransitive-wh-rules* + *inf-complement-rules*
;;; + *pp-rules* + the wh / inversion front end.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-want-wh-control-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-want-wh-control-test (&optional verbose)
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
                        *delta-complement-rules* *long-distance-wh-rules*
                        *ditransitive-wh-rules* *wh-pp-rules* *pp-rules*)

      ;; Sanity: want is subj-less-inf-obj (the subject-control path that has no
      ;; embedded subject), NOT subj-binds-delta like promise.
      (expandsim 'want)
      (truthy "dict `want' is subj-less-inf-obj"
              (member 'subj-less-inf-obj (get 'want 'features)))

      (let ((ok (parse-sentence "who do you want to give a book to ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"who do you want to give a book to ?\"" ok t)
        (truthy "matrix S is a major wh-question" (subsetp '(major quest wh-quest s) (fe c)))

        (let ((whc (getr :wh-comp c)))
          (truthy "the matrix S has a :wh-comp register (`who')" whc)
          (truthy "the wh-comp is marked utilized" (and whc (member 'utilized (fe whc))))

          ;; --- Matrix clause: you want [inf clause] ---
          (check "the matrix subject is `you'" (np-word (daughter 'np c)) 'you)
          (let* ((vp     (daughter 'vp c))
                 (compnp (and vp (daughter 'np vp)))
                 (embs   (and compnp (daughter 's compnp))))
            (check "the matrix verb is `want'"
                   (and vp (getr 'word (daughter 'verb vp))) 'want)
            (truthy "the matrix object is a comp-np" (and compnp (member 'comp-np (fe compnp))))
            (check "the comp-np is marked inf-comp"
                   (and compnp (getr 'markers compnp)) '(inf-comp))

            ;; --- Embedded infinitive: delta(=you) give a book to t_who ---
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

              ;; Subject control via the subj-less path: delta bound UP to `you'.
              (truthy "the embedded subject is a delta" (and esubj (member 'delta (fe esubj))))
              (check "the embedded delta is controlled by the matrix subject `you'"
                     (and esubj (getr 'binding esubj)) (daughter 'np c))

              (truthy "the embedded theme is `a book'" book)

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
                (check "matrix predicate is want" (and mcf (get mcf 'pred)) 'want)
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
                    (check "the embedded theme is `a book'" (cadr eneut) book)
                    (truthy "embedded dative (the recipient) filled" edat)
                    (check "the recipient is the stranded PP's wh-gap trace" (cadr edat) ppobj)
                    (check "the recipient was filled via the preposition `to'"
                           (caddr edat) 'to)))))))))

    (format t "~&gram1-want-wh-control-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-want-wh-control-test t)
