;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-apposition-test.lisp
;;;
;;; Apposition, parsed end to end (*apposition-rules* -- OUR extension, the first
;;; genuinely new grammar rule; gram1-gram5 have no apposition).
;;;
;;;     the dog , spot sees the woman .   ->  subject = "the dog" NP, named "spot"
;;;
;;; A just-completed common-noun NP directly followed by `, NAME' takes the name
;;; as an appositive while KEEPING its own (specific) type. One rule does it:
;;;   APPOSITIVE   IN NP-COMPLETE, on [=apunc][=name]: the comma sits in 1st so
;;;                PROPNAME (an attention shift needing a name in 1st) never fires
;;;                on the name; consume the comma (1st) AND the name NP (2nd, built
;;;                by PROPNAME during lookahead) in one rule -- as gram1 NP-UTTERANCE
;;;                grabs 1st+2nd -- attaching the name under the head's `appos'
;;;                daughter, then Run np-done next. Fires at default priority 10,
;;;                ahead of NP-DONE (15), so it intercepts before the NP finalises.
;;;
;;; dog/woman/see/, are dictionary-driven; `spot' (a proper name) is supplied with
;;; a small df, exactly as gram1-proper-noun-test supplies `John'. Uses the whole
;;; grammar (load-full-grammar), so this doubles as a composition check that our
;;; extension composes cleanly with every Marcus group.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-apposition-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-apposition-test (&optional verbose)
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
      ;; `spot' is not in the dictionary; supply it as a proper name (hanim).
      (df spot feats (name ns n3p) markers (hanim))
      (expandsim 'spot)

      ;; -- subject apposition: "the dog , spot sees the woman ." --
      (let ((ok (parse-sentence "the dog , spot sees the woman ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (truthy "parse succeeds on \"the dog , spot sees the woman .\"" ok)
        (truthy "top S is a major declarative" (subsetp '(decl major s) (fe c)))
        (let* ((subj  (daughter 'np c))
               (appos (and subj (daughter 'appos subj))))
          (truthy "subject NP present" subj)
          (check  "subject keeps its head-noun type \"dog\""
                  (and subj (getr 'word (daughter 'noun (daughter 'nbar subj)))) 'dog)
          (truthy "subject has an `appos' daughter" appos)
          (truthy "the appositive is a name NP" (and appos (member 'name (fe appos))))
          (check  "appositive name is \"spot\""
                  (and appos (getr 'word (daughter 'noun appos))) 'spot)))

      ;; -- object apposition: "the woman sees the dog , spot ." --
      (let ((ok (parse-sentence "the woman sees the dog , spot ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (truthy "parse succeeds on \"the woman sees the dog , spot .\"" ok)
        (let* ((obj   (daughter 'np (daughter 'vp c)))
               (appos (and obj (daughter 'appos obj))))
          (truthy "object NP present" obj)
          (check  "object keeps its head-noun type \"dog\""
                  (and obj (getr 'word (daughter 'noun (daughter 'nbar obj)))) 'dog)
          (check  "object appositive name is \"spot\""
                  (and appos (getr 'word (daughter 'noun appos))) 'spot)))

      ;; -- no false positives: a plain NP (no comma) has no appos --
      (let ((ok (parse-sentence "the dog sees the woman ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (truthy "parse still succeeds without apposition" ok)
        (let ((subj (daughter 'np c)))
          (truthy "plain subject has NO appos daughter"
                  (not (and subj (daughter 'appos subj)))))))

    (format t "~&gram1-apposition-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-apposition-test t)
