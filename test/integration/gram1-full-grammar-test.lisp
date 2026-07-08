;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-full-grammar-test.lisp
;;;
;;; The WHOLE-GRAMMAR LOAD PATH. Every other integration test curates the
;;; handful of rule groups its one sentence needs. This test instead loads
;;; the ENTIRE validated grammar at once -- (load-full-grammar), which
;;; registers *full-grammar* (all 26 composable groups, with the complete
;;; VP-NP) -- and then parses a sentence from EVERY construction family
;;; against that single grammar, in one image.
;;;
;;; It pins down two things the curated slices could not:
;;;
;;;   1. The union COMPOSES. Loading all groups together introduces no
;;;      parse-time rule conflicts: INITIAL-RULE dispatches each sentence
;;;      type to the right packets, and the diagnostic rules (THAT-DIAG,
;;;      WHICH-DIAGN, SUBJ-QUEST?, REDUCED-RELATIVE, the wh/existential
;;;      openers, ...) stay disjoint. A declarative is still DECL, a yes-no
;;;      is still YNQUEST, an existential is still EXISTENTIAL -- nobody
;;;      steals anybody's sentence.
;;;
;;;   2. One loaded grammar parses MANY sentences in one image. This used to
;;;      be impossible: PARSE-SENTENCE reset the parse globals but left the
;;;      buffer-position feature vectors (*1stfvec*/*2ndfvec*/*3rdfvec*)
;;;      dirty, so a `verb' (or any feature) left in a buffer slot by one
;;;      parse would spuriously satisfy a `[=verb]' pattern in the next
;;;      (e.g. "is there a meeting ?" misfiring REDUCED-RELATIVE after any
;;;      sentence ending in a tensed verb). RESET-PARSER-STATE now re-zeroes
;;;      those vectors (and clears the open case frame), restoring the
;;;      matcher invariant, so the battery below runs end to end without any
;;;      between-parse reset.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-full-grammar-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-full-grammar-test (&optional verbose)
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

      ;; ONE load of the WHOLE grammar -- no per-sentence rule-group curation.
      (load-full-grammar)

      ;; Sanity: the rule table really holds the whole grammar, not a slice.
      ;; (Every group's signature rule is registered together.)
      (dolist (rule '("INITIAL-RULE" "MAJOR-DECL-S" "MAIN-VERB" "YES-NO-Q"
                      "THERE" "WH-QUEST" "AUX-INVERSION" "CREATE-DELTA-SUBJ"
                      "WHICH-DIAGN" "WH-RELATIVE-CLAUSE" "PROPNAME" "MODAL" "FUTURE"))
        (truthy (format nil "rule ~a is registered" rule)
                (act-of-rule (intern rule :parsifal))))

      ;; The battery: one sentence per construction family, each parsed
      ;; against the SAME loaded grammar, back to back, in this one image.
      ;; For each: the parse must succeed AND the top S must carry the
      ;; expected sentence-type features.
      (let ((battery
              '(("the boy meets the boy ."                          (decl major s))
                ("schedule a meeting for friday ."                  (imper major s))
                ("the boy wants to go ."                            (decl major s))
                ("the boy persuaded the girl to go ."               (decl major s))
                ("the boy promised the girl to go ."                (decl major s))
                ("the boy believes that the lecture meets ."        (decl major s))
                ("a meeting seems to have been scheduled for friday ." (decl major s))
                ("i will schedule a meeting ."                      (decl major s))
                ("john should have scheduled the meeting ."         (decl major s))
                ("who did the boy see ?"                            (wh-quest major quest s))
                ("what did the boy break ?"                         (wh-quest major quest s))
                ("did the boy meet you ?"                           (ynquest major quest s))
                ("is there a meeting ?"                             (existential ynquest s))
                ("the boy who runs ."                               (np-utterance s)))))
        (dolist (item battery)
          (destructuring-bind (sentence expected) item
            (let ((ok (parse-sentence sentence
                                      :initial-rule (intern "INITIAL-RULE" :parsifal))))
              (truthy (format nil "parse succeeds: ~a" sentence) ok)
              (when ok
                (check (format nil "  top S is ~{~a~^/~}: ~a" expected sentence)
                       (and (subsetp expected (fe c)) t) t))))))

      ;; A couple of deeper spot-checks, to prove these are real parses and
      ;; not just type-labelled stubs -- after the LAST battery sentence the
      ;; grammar is still fully live, so re-parse and inspect structure.
      (let ((ok (parse-sentence "the boy persuaded the girl to go ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (truthy "spot-check parse (object control) succeeds" ok)
        (when ok
          (let* ((vp     (daughter 'vp c))
                 (nps    (and vp (daughters 'np vp)))
                 (compnp (car nps)))
            (check "object-control verb is persuade"
                   (and vp (getr 'word (daughter 'verb vp))) 'persuaded)
            (check "object-control still binds the embedded delta to the object"
                   (getr 'binding (daughter 'np (daughter 's compnp)))
                   (cadr nps)))))

      (let ((ok (parse-sentence "is there a meeting ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (truthy "spot-check parse (existential) succeeds" ok)
        (when ok
          (truthy "existential clause is labelled existential"
                  (member 'existential (fe c))))))

    (format t "~&gram1-full-grammar-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-full-grammar-test t)
