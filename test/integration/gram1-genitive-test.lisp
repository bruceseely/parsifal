;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-genitive-test.lisp
;;;
;;; Genitive / possessive NPs, parsed end to end (*genitive-rules*).
;;;
;;;     the man 's dog sees the woman .   ->  subject = "the man's dog"
;;;
;;; PARSIFAL parses a genitive by making the possessor the DETERMINER of the head
;;; noun. Two rules (Marcus gram3.l, verbatim) do it:
;;;   POSS-NP        when a completed NP is followed by the possessive clitic `'s
;;;                  (dict: (df \'s feats (poss))), attach it as `poss and re-label
;;;                  the NP a poss-np, then run np-done so it drops back to CPOOL.
;;;   POSSESSIVE-DET a poss-np in CPOOL is wrapped in a fresh POSS-DET det node
;;;                  (ngstart, def), which then fires STARTNP/DETERMINER for the
;;;                  head noun -- so the head NP is [def] and its det's `np'
;;;                  daughter is the possessor.
;;; A proper name ("mitch's") reaches poss-np via END-OF-NAME (*proper-noun-rules*)
;;; and a possessive pronoun ("my") via the PRONOUN rule; both then feed
;;; POSSESSIVE-DET. So the same head structure covers all three.
;;;
;;; man/dog/woman/see/mitch/my/'s are all dictionary-driven; uses the whole
;;; grammar (load-full-grammar), so this doubles as a composition check.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-genitive-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-genitive-test (&optional verbose)
  (let ((results t))
    (flet ((truthy (test-name actual)
             (let ((pass (and actual t)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass (format t "    expected non-NIL, got NIL~%")))
               (setf results (and pass results))))
           (check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%" expected actual)))
               (setf results (and pass results)))))

      (load-full-grammar)

      ;; -- common-noun possessor: "the man's dog" --
      (let ((ok (parse-sentence "the man 's dog sees the woman ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (truthy "parse succeeds on \"the man 's dog sees the woman .\"" ok)
        (let* ((subj (daughter 'np c))
               (det  (and subj (daughter 'det subj)))
               (possr (and det (daughter 'np det))))
          (truthy "subject NP present" subj)
          (truthy "subject is a definite det NP" (and subj (subsetp '(def det) (fe subj))))
          (truthy "subject's determiner is a POSS-DET" (and det (member 'poss-det (fe det))))
          (check  "head noun is \"dog\""
                  (and subj (getr 'word (daughter 'noun (daughter 'nbar subj)))) 'dog)
          (truthy "the poss-det wraps a poss-np possessor" (and possr (member 'poss-np (fe possr))))
          (truthy "possessor carries the clitic 's as its `poss'" (and possr (daughter 'poss possr)))
          (check  "possessor head noun is \"man\""
                  (and possr (getr 'word (daughter 'noun (daughter 'nbar possr)))) 'man)))

      ;; -- proper-name possessor: "mitch's dog" (poss-np via END-OF-NAME) --
      (let ((ok (parse-sentence "mitch 's dog sees the woman ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (truthy "parse succeeds on \"mitch 's dog sees the woman .\"" ok)
        (let* ((subj (daughter 'np c))
               (possr (and subj (daughter 'np (daughter 'det subj)))))
          (truthy "name possessor is a poss-np name NP"
                  (and possr (subsetp '(poss-np name) (fe possr))))
          (check  "possessor name is \"mitch\""
                  (and possr (getr 'word (daughter 'noun possr))) 'mitch)))

      ;; -- possessive pronoun: "my dog" (poss-np via PRONOUN) --
      (let ((ok (parse-sentence "my dog sees the woman ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (truthy "parse succeeds on \"my dog sees the woman .\"" ok)
        (let* ((subj (daughter 'np c))
               (possr (and subj (daughter 'np (daughter 'det subj)))))
          (truthy "pronoun possessor is a poss-np pron-np"
                  (and possr (subsetp '(poss-np pron-np) (fe possr)))))))

    (format t "~&gram1-genitive-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-genitive-test t)
