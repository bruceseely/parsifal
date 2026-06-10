;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-transitive-test.lisp
;;;
;;; A full *transitive* declarative clause parsed end to end against
;;; Marcus's verbatim gram1/gram3 rules: "the dog ate the bone .".
;;; Builds on the intransitive clause (gram1-clause-test) and adds object
;;; handling:
;;;
;;;   ... MAIN-VERB builds the VP ...
;;;   STARTNP / DETERMINER / NOUN / NBAR / NP-COMPLETE ...  -- the OBJECT NP
;;;   OBJECTS        ss-vp [=np]        -> attach the object NP to the VP
;;;   VP-NP          attachment crule   -> fill an obj slot of the VP's
;;;                                        case frame with the object NP
;;;   VP-DONE / S-DONE                  -> finish
;;;
;;; Result tree:
;;;   [S decl/major [NP the dog] [aux] [VP [verb ate] [NP the bone]]
;;;                 [finalpunc .]]
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-transitive-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-transitive-test (&optional verbose)
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
      (dolist (w (list 'the 'dog 'ate 'bone (intern "." :parsifal)))
        (setf (symbol-plist w) nil))
      (df the feats (det ngstart def ns npl))
      (df dog feats (noun ns n3p) markers (anim hanim))
      (df bone feats (noun ns n3p) markers (physob inanim))
      ;; "ate": a past-tense transitive main verb -- object (neut) + agent.
      (df ate feats (verb mainverb past v-3s) cf (neut agt) markers (act))
      (%df (list (intern "." :parsifal) 'feats '(finalpunc punc)))
      (dolist (w (list 'the 'dog 'ate 'bone (intern "." :parsifal))) (expandsim w))

      ;; --- Grammar: NP rules + clause layer + the VP-NP object crule.
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*)

      ;; --- Parse "the dog ate the bone ." end to end.
      (let ((ok (parse-sentence "the dog ate the bone ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"the dog ate the bone .\"" ok t)

        ;; Two NPs built (subject + object), the clause layer, and OBJECTS.
        (check "transitive derivation (subject NP, clause, object NP, OBJECTS)"
               (reverse (mapcar #'symbol-name *deriv*))
               '("INITIAL-RULE" "STARTNP" "DETERMINER" "DET-QUANT-DONE"
                 "ADJ" "NOUN" "NBAR" "NP-COMPLETE" "NBAR-COMPLETE"
                 "NBAR-DONE" "NP-DONE" "MAJOR-DECL-S" "UNMARKED-ORDER"
                 "STARTAUX" "AUX-COMPLETE" "AUX-ATTACH" "MAIN-VERB"
                 "STARTNP" "DETERMINER" "DET-QUANT-DONE" "ADJ" "NOUN"
                 "NBAR" "OBJECTS" "NP-COMPLETE" "NBAR-COMPLETE"
                 "NBAR-DONE" "NP-DONE" "VP-DONE" "S-DONE"))

        ;; Tree: [S decl/major [NP subj] [aux] [VP [verb] [NP obj]] [finalpunc]].
        (truthy "S is a declarative major clause"
                (subsetp '(decl major s) (fe c)))
        (truthy "subject NP attached to S" (daughter 'np c))
        (let ((vp (daughter 'vp c)))
          (truthy "a VP was attached to S" vp)
          (truthy "the verb is under the VP" (and vp (daughter 'verb vp)))
          (truthy "the VP carries a case frame" (and vp (getr 'caseframe vp)))
          ;; the object NP is attached UNDER the VP (subject is under S).
          (let ((obj (and vp (daughter 'np vp))))
            (truthy "the object NP is attached to the VP" obj)
            (truthy "the object NP is \"the bone\""
                    (let* ((nbar (and obj (daughter 'nbar obj)))
                           (noun (and nbar (daughter 'noun nbar))))
                      (and noun (eq (getr 'word noun) 'bone))))))
        (truthy "final punctuation attached to S" (daughter 'finalpunc c))))

    (format t "~&gram1-transitive-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-transitive-test t)
