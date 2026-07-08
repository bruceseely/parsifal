;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-dictionary-test.lisp
;;;
;;; Dictionary-driven parse: a full declarative clause whose words come
;;; entirely from the LOADED defs.l dictionary (defs-dictionary.dict,
;;; auto-loaded by dictionary.lisp at system load) -- NO hand-built `df'.
;;; "the lecture meets ." :
;;;
;;;   the      -- dict determiner (feats: det ns n3p npl def)
;;;   lecture  -- dict noun       (feats: noun ns; markers: socev mentobj)
;;;   meets    -- morpho -> dict verb `meet' (feats: mainverb pres v-3s
;;;               tnsless; cf ((neut) (com) agt); markers act)
;;;   .        -- dict finalpunc
;;;
;;; This exercises the real lexicon feeding the grammar, which depends on
;;; the `redund' feature-closure being applied at expansion (a det must
;;; become an `ngstart' for STARTNP; a tensed verb must become a `verb'
;;; for MAJOR-DECL-S) -- the dict lists neither explicitly.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-dictionary-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-dictionary-test (&optional verbose)
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

      ;; --- NO lexicon setup: the words come from the loaded dictionary.
      ;; (We only reset the rule table; the dictionary stays as loaded.)
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*)

      ;; Sanity: the dictionary really does define these words, and the
      ;; redund closure supplies their grammar-driving features.
      (expandsim 'the) (expandsim 'lecture) (expandsim 'meet)
      (truthy "dict `the' is a determiner that redund-closes to ngstart"
              (and (member 'det (get 'the 'features))
                   (member 'ngstart (get 'the 'features))))
      (truthy "dict `lecture' is a noun with its defs.l markers"
              (and (member 'noun (get 'lecture 'features))
                   (member 'mentobj (get 'lecture 'markers))))
      (truthy "dict `meet' redund-closes to a verb"
              (member 'verb (get 'meet 'features)))

      ;; --- Parse "the lecture meets ." entirely from the dictionary.
      (let ((ok (parse-sentence "the lecture meets ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"the lecture meets .\"" ok t)
        (check "full clause derivation (dictionary-driven)"
               (reverse (mapcar #'symbol-name *deriv*))
               '("INITIAL-RULE" "STARTNP" "DETERMINER" "DET-QUANT-DONE"
                 "ADJ" "NOUN" "NBAR" "NP-COMPLETE" "NBAR-COMPLETE"
                 "NBAR-DONE" "NP-DONE" "MAJOR-DECL-S" "UNMARKED-ORDER"
                 "STARTAUX" "AUX-COMPLETE" "AUX-ATTACH" "MAIN-VERB"
                 "VP-DONE" "S-DONE"))
        (truthy "S is a declarative major clause"
                (subsetp '(decl major s) (fe c)))
        (let ((subj (daughter 'np c)))
          (truthy "subject NP attached" subj)
          (truthy "subject is the dictionary noun \"lecture\""
                  (let ((noun (and subj (daughter 'noun (daughter 'nbar subj)))))
                    (and noun (eq (getr 'word noun) 'lecture)))))
        (let ((vp (daughter 'vp c)))
          (truthy "VP attached with verb and a case frame"
                  (and vp (daughter 'verb vp) (getr 'caseframe vp)))
          ;; "meets" was morpho'd to the dictionary verb `meet': the verb
          ;; node carries meet's canonical-word feature *MEET (its surface
          ;; `word' register stays the inflected "meets").
          (truthy "the verb resolved to the dictionary verb \"meet\""
                  (and vp (member (intern "*MEET" :parsifal)
                                  (fe (daughter 'verb vp))))))
        (truthy "final punctuation attached" (daughter 'finalpunc c))))

    (format t "~&gram1-dictionary-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-dictionary-test t)
