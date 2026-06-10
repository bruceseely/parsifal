;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-clause-test.lisp
;;;
;;; A full declarative clause parsed end to end against Marcus's verbatim
;;; gram1/gram3 rules: "the dog ran .". This builds on the NP-construction
;;; integration (gram3-np-test) and adds the clause layer, driving the
;;; verb case frames (case.l) under live rule firing for the first time:
;;;
;;;   MAJOR-DECL-S   ss-start [np][verb] -> start a declarative clause
;;;   UNMARKED-ORDER parse-subj          -> attach the subject NP
;;;   STARTAUX / AUX-COMPLETE / AUX-ATTACH
;;;                  parse-aux + build-aux -> the (empty) aux for "ran"
;;;   MAIN-VERB      parse-vp            -> build the VP, attach the verb
;;;   VP-VERB        attachment crule    -> associate a case frame with
;;;                  the VP and fill its spec/pred/subj slots
;;;   VP-DONE / S-DONE                   -> finish the VP, then the clause
;;;
;;; Result tree:
;;;   [S decl/major [NP the dog] [aux] [VP [verb ran]] [finalpunc .]]
;;; with a case frame associated to the VP.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-clause-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)




(defun gram1-clause-test (&optional verbose)
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
               (setf results (and pass results)))))

      (reset-rule-table)
      (reset-lexicon)

      ;; --- Lexicon (in :parsifal so features are EQ to the linked rules).
      (dolist (w (list 'the 'dog 'ran (intern "." :parsifal)))
        (setf (symbol-plist w) nil))
      (df the feats (det ngstart def ns npl))
      (df dog feats (noun ns n3p))
      ;; "ran": a past-tense intransitive main verb with an agent case frame.
      (df ran feats (verb mainverb past v-3s) cf (agt) markers (act))
      (%df (list (intern "." :parsifal) 'feats '(finalpunc punc)))
      (dolist (w (list 'the 'dog 'ran (intern "." :parsifal))) (expandsim w))

      ;; --- Grammar: the gram3 NP rules + the gram1 clause layer.
      (register-grammar *np-rules* *clause-rules*)

      ;; --- Parse "the dog ran ." end to end.
      (let ((ok (parse-sentence "the dog ran ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"the dog ran .\"" ok t)

        (check "full clause derivation (NP build + clause layer)"
               (reverse (mapcar #'symbol-name *deriv*))
               '("INITIAL-RULE" "STARTNP" "DETERMINER" "DET-QUANT-DONE"
                 "ADJ" "NOUN" "NBAR" "NP-COMPLETE" "NBAR-COMPLETE"
                 "NBAR-DONE" "NP-DONE" "MAJOR-DECL-S" "UNMARKED-ORDER"
                 "STARTAUX" "AUX-COMPLETE" "AUX-ATTACH" "MAIN-VERB"
                 "VP-DONE" "S-DONE"))

        ;; Tree: [S decl/major [NP ...] [aux] [VP [verb]] [finalpunc]].
        (truthy "S is a declarative major clause"
                (subsetp '(decl major s) (fe c)))
        (let ((subj (daughter 'np c)))
          (truthy "subject NP attached to S" subj)
          (truthy "subject NP is the singular \"the dog\""
                  (and subj (member 'ns (fe subj))
                       (daughter 'noun (daughter 'nbar subj)))))
        (let ((vp (daughter 'vp c)))
          (truthy "a VP was attached to S" vp)
          (truthy "the verb is under the VP" (and vp (daughter 'verb vp)))
          ;; VP-VERB (attachment crule) associated a case frame with the VP.
          (truthy "the VP-VERB crule associated a case frame with the VP"
                  (and vp (getr 'caseframe vp))))
        (truthy "an aux node attached to S" (daughter 'aux c))
        (truthy "final punctuation attached to S" (daughter 'finalpunc c))))

    (format t "~&gram1-clause-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-clause-test t)
