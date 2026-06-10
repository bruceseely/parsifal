;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram3-np-test.lisp
;;;
;;; Full NP construction against Marcus's real gram1/gram3 rules: parse
;;; "the dog ." (determiner + noun + final punctuation) end to end. This
;;; exercises the whole wait-and-see apparatus together for the first
;;; time:
;;;
;;;   AS rule          STARTNP        -- attention-shift to start the NP
;;;   creation crule   NP-START       -- associate a case frame with it
;;;   normal rules     DETERMINER, DET-QUANT-DONE, ADJ, NOUN, NBAR
;;;                                   -- with their packet transitions
;;;   NR rules         NBAR-COMPLETE, NP-COMPLETE
;;;                                   -- node reactivation, fired by the
;;;                                      utterance rule looking ahead
;;;   attachment crule NP-NBAR        -- case-frame wiring on attach
;;;   then             NP-UTTERANCE   -- assemble the utterance
;;;
;;; Result tree:  [S [NP [det the] [nbar [noun dog]]] [finalpunc .]],
;;; with number agreement giving the NP the `ns' (singular) feature.
;;;
;;; Rules are Marcus's verbatim (modulo whitespace); LINKed into
;;; :parsifal so data symbols are EQ to the runtime's.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram3-np-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram3-np-test (&optional verbose)
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
      (dolist (w (list 'the 'dog (intern "." :parsifal)))
        (setf (symbol-plist w) nil))
      (df the feats (det ngstart def ns npl))
      (df dog feats (noun ns n3p))
      (%df (list (intern "." :parsifal) 'feats '(finalpunc punc)))
      (dolist (w (list 'the 'dog (intern "." :parsifal))) (expandsim w))

      ;; --- Grammar: the gram3 NP rules + the NP-UTTERANCE rule.
      (register-grammar *np-rules* *np-utterance-rule*)

      ;; --- Parse "the dog ." end to end.
      (let ((ok (parse-sentence "the dog ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"the dog .\"" ok t)

        ;; The full wait-and-see derivation: AS (STARTNP), the det/qp/adj/
        ;; noun chain, both NR completions, and the utterance rule.
        (check "12-rule derivation, AS + both NR rules firing"
               (reverse (mapcar #'symbol-name *deriv*))
               '("INITIAL-RULE" "STARTNP" "DETERMINER" "DET-QUANT-DONE"
                 "ADJ" "NOUN" "NBAR" "NP-COMPLETE" "NBAR-COMPLETE"
                 "NBAR-DONE" "NP-DONE" "NP-UTTERANCE"))

        ;; The tree: [S [NP [det the] [nbar [noun dog]]] [finalpunc .]].
        (truthy "S labelled np-utterance" (member 'np-utterance (fe c)))
        (let ((np (daughter 'np c)))
          (truthy "an NP was attached to S" np)
          (truthy "the NP carries det / np features"
                  (and np (subsetp '(np det) (fe np))))
          (truthy "number agreement made the NP singular (ns)"
                  (and np (member 'ns (fe np))))
          (truthy "the determiner is under the NP"
                  (and np (daughter 'det np)))
          (let ((nbar (and np (daughter 'nbar np))))
            (truthy "an NBAR is under the NP" nbar)
            (truthy "the noun is under the NBAR"
                    (and nbar (daughter 'noun nbar)))))
        (truthy "final punctuation attached to S" (daughter 'finalpunc c))))

    (format t "~&gram3-np-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram3-np-test t)
