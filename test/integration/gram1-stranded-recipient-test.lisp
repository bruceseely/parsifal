;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-stranded-recipient-test.lisp
;;;
;;; "Who did Bob give the book to?" parsed end to end, FULLY DICT-DRIVEN -- a
;;; verbatim Marcus example sentence (notes/pidgin-grammar-intro.text). This is
;;; how PARSIFAL handles a questioned recipient: by STRANDING the preposition,
;;; not by pied-piping it (Marcus's grammar has no "To whom did Bob give it?"
;;; -- that fronted form is deliberately absent; the stranded form is canonical).
;;;
;;;     who_i did Bob give the book [to t_i]
;;;       agt = Bob, neut = the book (theme), dat = who (recipient, via `to')
;;;
;;; It is the ditransitive, intervening-object generalization of the simpler
;;; stranding case ("who did the boy talk to ?"): the stranded preposition is
;;; separated from the verb by an overt object (`the book'), and it fills the
;;; RECIPIENT (DAT) rather than the verb's only object. After WH-QUEST fronts
;;; `who' (np-quest) and subject-aux inversion makes `Bob' the overt subject,
;;; MAIN-VERB(give) leaves the wh-comp pending in wh-vp. The committed wh-vp
;;; placement layer (*ditransitive-wh-rules*) keeps the wh-comp for the recipient
;;; slot while `the book' is taken as the THEME (OBJECTS -> NEUT, give's first
;;; object slot); at the clause-final stranded `to' the buffer is [to][?] (prep
;;; + non-np) with the wh-comp still pending, so WH-PP-BUILD fires: it builds the
;;; PP, drops a trace as `to's object, binds the trace to the wh-comp (who), and
;;; marks the wh-comp utilized; PP-UNDER-VP-1 attaches the PP and the VP-PP
;;; case-fill routes `to' -> DAT. So the recipient is recoverable both
;;; structurally (the PP-object trace's binding) and thematically (the DAT case).
;;;
;;; No new rules -- pure composition of the wh-question / inversion / object-wh /
;;; ditransitive-wh / wh-stranding / pp layers. who/did/bob/give/the/book/to/?
;;; are all dictionary + morpho.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-stranded-recipient-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-stranded-recipient-test (&optional verbose)
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

      ;; Fully dict-driven. *ditransitive-wh-rules* is needed even though the
      ;; stranding itself is done by WH-PP-BUILD: it is what correctly slots the
      ;; overt `the book' into the THEME (NEUT) and keeps the wh-comp for the
      ;; recipient -- drop it and `the book' wrongly takes the DAT object slot
      ;; and the parse fails.
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*
                        *pronoun-rule* *qp1-done-rule* *wh-question-rules*
                        *inversion-rules* *object-wh-rules*
                        *ditransitive-wh-rules* *wh-pp-rules* *pp-rules*)

      (let ((ok (parse-sentence "who did bob give the book to ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"who did bob give the book to ?\"" ok t)
        (truthy "WH-PP-BUILD fired (the stranded-preposition rule)"
                (member 'wh-pp-build *deriv*))
        (truthy "S is a major wh-question" (subsetp '(major quest wh-quest s) (fe c)))
        (truthy "S is np-quest (an NP, not a PP, was fronted)" (member 'np-quest (fe c)))

        (let ((whc (getr :wh-comp c)))
          (truthy "the S has a :wh-comp register (the fronted `who')" whc)
          (truthy "the wh-element is a wh pron-np" (and whc (subsetp '(pron-np wh) (fe whc))))
          (truthy "the wh-comp is marked utilized" (and whc (member 'utilized (fe whc))))

          (let ((subj (daughter 'np c)))
            (check "the subject is the inverted overt `bob'" (word-of subj) 'bob)

            (let* ((vp     (daughter 'vp c))
                   (nps    (and vp (daughters 'np vp)))
                   (book   (find-if (lambda (n) (eq (word-of n) 'book)) nps))
                   (pps    (and vp (daughters 'pp vp)))
                   (pp     (car pps))
                   (ppobj  (and pp (daughter 'np pp))))
              (truthy "a VP attached to S" vp)
              (check "the verb is `give'" (and vp (getr 'word (daughter 'verb vp))) 'give)
              (truthy "the aux carries do-support (`did')"
                      (let ((aux (daughter 'aux c)))
                        (and aux (getr 'word (daughter 'do aux)))))

              ;; The overt object is `the book' (the theme); the recipient is the
              ;; stranded PP's gap.
              (truthy "the VP's overt object is `the book'" book)
              (truthy "the VP has exactly one PP (the stranded `to')"
                      (and pps (= (length pps) 1)))
              (check "the stranded preposition is `to'"
                     (and pp (getr 'word (daughter 'prep pp))) 'to)
              (truthy "the stranded PP's object is a trace (the wh-gap)"
                      (and ppobj (member 'trace (fe ppobj))))
              (check "the stranded PP's trace is bound to the wh-element (who)"
                     (and ppobj (getr 'binding ppobj)) whc)
              (truthy "final (question) punctuation attached" (daughter 'finalpunc c))

              ;; Case frame: agt = Bob, neut = the book (theme), dat = who
              ;; (recipient, marked by the stranded `to').
              (let ((cf (and vp (getr 'caseframe vp))))
                (truthy "the VP has a case frame" cf)
                (check "the predicate is give" (and cf (get cf 'pred)) 'give)
                (closeframe openframe)
                (let ((filled (cadr (first (get cf 'hypo-slots)))))
                  (truthy "the frame has filled cases" filled)
                  (let ((agt  (assoc 'agt filled))
                        (neut (assoc 'neut filled))
                        (dat  (assoc 'dat filled)))
                    (truthy "an agent case was filled" agt)
                    (check "the agent is the subject `bob'" (cadr agt) subj)
                    (truthy "a neutral (theme) case was filled" neut)
                    (check "the theme is `the book'" (cadr neut) book)
                    (truthy "a dative (recipient) case was filled" dat)
                    (check "the recipient is the stranded PP's trace (the wh-gap)"
                           (cadr dat) ppobj)
                    (check "the recipient was filled via the preposition `to'"
                           (caddr dat) 'to)))))))))

    (format t "~&gram1-stranded-recipient-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-stranded-recipient-test t)
