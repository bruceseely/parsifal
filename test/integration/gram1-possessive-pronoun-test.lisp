;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-possessive-pronoun-test.lisp
;;;
;;; `her' is two words -- the accusative pronoun and the possessive determiner --
;;; and which one it is depends on what follows:
;;;
;;;     the woman sees her dog .   ->  object = "her dog"  (poss-det + noun)
;;;     the woman sees her .       ->  object = "her"      (plain pron-np)
;;;
;;; Marcus's dictionary enters only the accusative, giving the possessive to
;;; `hers' (in English the INDEPENDENT possessive -- "the dog is hers" -- never a
;;; determiner). So "her dog" parsed as the object `her' plus a stranded `dog'
;;; that nothing could attach, and the head noun was dropped without a word.
;;;
;;; The fix is a lexical MARK plus a diagnosis rule, in the shape of Marcus's own
;;; WHAT-DIAG for det\relpron-ambiguous `what':
;;;   supplement.dict  gives `her' the feature `poss-ambig' -- both readings.
;;;   POSS-PRONOUN-DIAG (*possessive-pronoun-rules*, priority 4 in npool, OUR
;;;                     extension) looks at the NEXT buffer cell and adds
;;;                     `poss-pronoun' when a noun group starts there. It marks
;;;                     the word `poss-diag' either way, so the no-op branch
;;;                     cannot match itself forever.
;;; PRONOUN (default priority 10) then does the rest, exactly as it already did
;;; for `my': a poss-pronoun NP is labelled poss-np, and POSSESSIVE-DET wraps it
;;; as the head noun's determiner.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-possessive-pronoun-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-possessive-pronoun-test (&optional verbose)
  (let ((results t))
    (flet ((truthy (test-name actual)
             (let ((pass (and actual t)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass (format t "    expected non-NIL, got NIL~%")))
               (setf results (and pass results))))
           (falsy (test-name actual)
             (let ((pass (null actual)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass (format t "    expected NIL, got ~s~%" actual)))
               (setf results (and pass results))))
           (check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%" expected actual)))
               (setf results (and pass results)))))

      (load-full-grammar)

      ;; -- possessive reading: "her dog" is one NP, with `her' its determiner --
      (let ((ok (parse-sentence "the woman sees her dog ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (truthy "parse succeeds on \"the woman sees her dog .\"" ok)
        (let* ((obj   (daughter 'np (daughter 'vp c)))
               (det   (and obj (daughter 'det obj)))
               (possr (and det (daughter 'np det))))
          (truthy "object NP present" obj)
          (check  "object head noun is \"dog\" -- NOT dropped"
                  (and obj (getr 'word (daughter 'noun (daughter 'nbar obj)))) 'dog)
          (truthy "object's determiner is a POSS-DET" (and det (member 'poss-det (fe det))))
          (truthy "the poss-det wraps a poss-np pron-np"
                  (and possr (subsetp '(poss-np pron-np) (fe possr))))
          (check  "the possessor word is \"her\""
                  (and possr (getr 'word (daughter 'pronoun possr))) 'her)
          (truthy "the diagnosis ran and chose the possessive reading"
                  (and possr (subsetp '(poss-diag poss-pronoun)
                                      (fe (daughter 'pronoun possr)))))))

      ;; -- accusative reading: nothing starts a noun group, so `her' is the object --
      (let ((ok (parse-sentence "the woman sees her ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (truthy "parse succeeds on \"the woman sees her .\"" ok)
        (let ((obj (daughter 'np (daughter 'vp c))))
          (truthy "object NP present" obj)
          (truthy "object is a pron-np" (and obj (member 'pron-np (fe obj))))
          (falsy  "object is NOT a poss-np" (and obj (member 'poss-np (fe obj))))
          (falsy  "object has no determiner" (and obj (daughter 'det obj)))
          (check  "the object word is \"her\"" (and obj (getr 'word (daughter 'pronoun obj))) 'her)
          (truthy "the diagnosis RAN here too (poss-diag)"
                  (and obj (member 'poss-diag (fe (daughter 'pronoun obj)))))
          (falsy  "...and left the possessive reading off"
                  (and obj (member 'poss-pronoun (fe (daughter 'pronoun obj)))))))

      ;; -- the diagnosis is position-blind: a subject genitive works the same --
      (let ((ok (parse-sentence "her dog sees the woman ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (truthy "parse succeeds on \"her dog sees the woman .\"" ok)
        (let* ((subj (daughter 'np c))
               (possr (and subj (daughter 'np (daughter 'det subj)))))
          (check  "subject head noun is \"dog\""
                  (and subj (getr 'word (daughter 'noun (daughter 'nbar subj)))) 'dog)
          (truthy "subject's possessor is a poss-np pron-np"
                  (and possr (subsetp '(poss-np pron-np) (fe possr))))))

      ;; -- unambiguous pronouns are untouched: `him' stays accusative, `his'
      ;;    stays a determiner (neither carries poss-ambig, so the rule never
      ;;    fires on them) --
      (let ((ok (parse-sentence "the woman sees him ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (truthy "parse succeeds on \"the woman sees him .\"" ok)
        (let ((obj (daughter 'np (daughter 'vp c))))
          (falsy "\"him\" object is NOT a poss-np" (and obj (member 'poss-np (fe obj))))))
      (let ((ok (parse-sentence "the woman sees his dog ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (truthy "parse succeeds on \"the woman sees his dog .\"" ok)
        (let* ((obj (daughter 'np (daughter 'vp c)))
               (possr (and obj (daughter 'np (daughter 'det obj)))))
          (check  "\"his dog\" head noun is \"dog\""
                  (and obj (getr 'word (daughter 'noun (daughter 'nbar obj)))) 'dog)
          (truthy "\"his\" is still the poss-np possessor"
                  (and possr (subsetp '(poss-np pron-np) (fe possr)))))))

    (format t "~&gram1-possessive-pronoun-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-possessive-pronoun-test t)
