;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-nested-relative-flagship-test.lisp
;;;
;;; "I gave the boy who you wanted to give the books to a book." parsed end to
;;; end, FULLY DICT-DRIVEN -- the deepest, most heavily nested sentence in the
;;; suite. It is the number-free core of Marcus's example "I gave the boy who you
;;; wanted to give the books to three books." (the theme is `a book' here, not
;;; `three books': a NUMBER used as a determiner is a separate gram4 construction
;;; not yet ported; everything else parses cleanly).
;;;
;;; A ditransitive main clause whose RECIPIENT is modified by a relative clause,
;;; and that relative clause itself nests want subject-control + a long-distance
;;; preposition-stranded gap:
;;;
;;;   [S I gave [NP the boy_i  [S-rel who_i  you wanted [ delta_j to give the
;;;                                                        books [to t_i] ] ] ]
;;;            [NP a book] ]
;;;
;;;     main:     agt = I, dat = the boy (the recipient, relative-modified),
;;;               neut = a book
;;;     relative: who = the boy (the relativiser, bound to the head NP);
;;;               agt = you, neut = the want-complement
;;;     embedded: agt = you (delta, subj-less control), neut = the books,
;;;               dat = who = the boy (recipient, via the stranded `to')
;;;
;;; So a SINGLE noun phrase, `the boy', plays three roles bound across three
;;; clauses: the main-clause dative, the relative pronoun, and -- via the
;;; stranded `to' deep in the want-complement -- the embedded give's recipient.
;;; And the embedded subject (delta) is independently controlled UP to `you'.
;;; NO new rules: this is pure composition of the relative-clause, want
;;; subj-less-control, long-distance-stranding, ditransitive, and clause layers
;;; that the earlier tests built one at a time.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-nested-relative-flagship-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-nested-relative-flagship-test (&optional verbose)
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

      ;; The whole grammar at once -- this sentence reaches into nearly every
      ;; clause-layer group, so load them all rather than hand-curate.
      (load-full-grammar)

      (let ((ok (parse-sentence
                 "i gave the boy who you wanted to give the books to a book ."
                 :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds" ok t)
        (truthy "no BAD (ungrammatical) constituent anywhere"
                (notany (lambda (n) (member 'bad (fe n))) *nodelist*))
        (truthy "main S is a major declarative" (subsetp '(decl major s) (fe c)))
        (check "the main subject is `I'" (np-word (daughter 'np c)) 'i)

        (let* ((vp   (daughter 'vp c))
               (objs (and vp (daughters 'np vp)))
               ;; the recipient is the relative-modified object; the theme is the
               ;; plain one.
               (boy  (find-if (lambda (n) (daughter 's n)) objs))
               (book (find-if (lambda (n) (and (not (daughter 's n))
                                               (eq (np-word n) 'book))) objs)))
          (check "the main verb is `give' (gave)"
                 (and vp (getr 'word (daughter 'verb vp))) 'gave)
          (check "the main clause has two objects" (length objs) 2)
          (check "the relative-modified object is `the boy'" (np-word boy) 'boy)
          (truthy "the boy NP is labelled modified" (and boy (member 'modified (fe boy))))
          (check "the plain object (theme) is `a book'" (np-word book) 'book)

          ;; --- The relative clause on `the boy' ---
          (let* ((relS  (and boy (daughter 's boy)))
                 (relwh (and relS (getr :wh-comp relS))))
            (truthy "the boy NP dominates a relative S"
                    (and relS (subsetp '(sec relative s) (fe relS))))
            (truthy "the relative clause has a wh-comp (`who')" relwh)
            (check "the relative pronoun is bound to the head NP `the boy'"
                   (and relwh (getr 'binding relwh)) boy)

            (let* ((relvp   (and relS (daughter 'vp relS)))
                   (relsubj (and relS (daughter 'np relS)))
                   (compnp  (and relvp (find-if (lambda (n) (member 'comp-np (fe n)))
                                                (daughters 'np relvp)))))
              (check "the relative subject is `you'" (np-word relsubj) 'you)
              (check "the relative verb is `wanted' (root want)"
                     (and relvp (getr 'word (daughter 'verb relvp))) 'wanted)
              (truthy "want's object is a comp-np" (and compnp (member 'comp-np (fe compnp))))
              (check "the comp-np is marked inf-comp"
                     (and compnp (getr 'markers compnp)) '(inf-comp))

              ;; --- The want-complement: delta to give the books to t_who ---
              (let* ((infs    (and compnp (daughter 's compnp)))
                     (infsubj (and infs (daughter 'np infs)))
                     (infvp   (and infs (daughter 'vp infs)))
                     ;; the only direct NP object is the theme; the recipient
                     ;; sits in the stranded PP, not here.
                     (theme   (and infvp (find-if (lambda (n) (not (member 'trace (fe n))))
                                                  (daughters 'np infvp))))
                     (infpp   (and infvp (car (daughters 'pp infvp))))
                     (ppobj   (and infpp (daughter 'np infpp))))
                (truthy "the comp-np dominates an embedded inf-S"
                        (and infs (subsetp '(sec comp-s inf-s s) (fe infs))))
                (check "the embedded verb is `give'"
                       (and infvp (getr 'word (daughter 'verb infvp))) 'give)

                ;; Subject control (subj-less): the embedded delta -> `you'.
                (truthy "the embedded subject is a delta"
                        (and infsubj (member 'delta (fe infsubj))))
                (check "the embedded delta is controlled by the relative subject `you'"
                       (and infsubj (getr 'binding infsubj)) relsubj)

                (check "the embedded theme is `the books'" (np-word theme) 'books)

                ;; The long-distance stranded gap: the embedded recipient is the
                ;; relative pronoun -- i.e. the head `the boy'.
                (check "the embedded stranded preposition is `to'"
                       (and infpp (getr 'word (daughter 'prep infpp))) 'to)
                (truthy "the stranded PP's object is a trace"
                        (and ppobj (member 'trace (fe ppobj))))
                (check "the stranded PP's trace is bound to the relative pronoun (= the boy)"
                       (and ppobj (getr 'binding ppobj)) relwh)

                (truthy "final punctuation attached" (daughter 'finalpunc c))

                ;; --- Three case frames, the unifying picture ---
                (let ((mcf (and vp (getr 'caseframe vp)))
                      (wcf (and relvp (getr 'caseframe relvp)))
                      (ecf (and infvp (getr 'caseframe infvp))))
                  (check "main predicate is give" (and mcf (get mcf 'pred)) 'give)
                  (check "relative predicate is want" (and wcf (get wcf 'pred)) 'want)
                  (check "embedded predicate is give" (and ecf (get ecf 'pred)) 'give)
                  (closeframe openframe)
                  (let ((mf (cadr (first (get mcf 'hypo-slots))))
                        (ef (cadr (first (get ecf 'hypo-slots)))))
                    ;; main: I gave the boy a book
                    (check "main agent is `I'" (cadr (assoc 'agt mf)) (daughter 'np c))
                    (check "main dative (recipient) is `the boy'" (cadr (assoc 'dat mf)) boy)
                    (check "main neutral (theme) is `a book'" (cadr (assoc 'neut mf)) book)
                    ;; embedded: you(delta) give the books to who(=the boy)
                    (truthy "embedded agent (the giver) filled" (assoc 'agt ef))
                    (check "embedded agent is the delta (= you, by control)"
                           (cadr (assoc 'agt ef)) infsubj)
                    (check "embedded theme is `the books'" (np-word (cadr (assoc 'neut ef))) 'books)
                    (check "embedded recipient is the stranded-gap trace (= the boy)"
                           (cadr (assoc 'dat ef)) ppobj)
                    (check "embedded recipient was filled via the preposition `to'"
                           (caddr (assoc 'dat ef)) 'to)))))))))

    (format t "~&gram1-nested-relative-flagship-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-nested-relative-flagship-test t)
