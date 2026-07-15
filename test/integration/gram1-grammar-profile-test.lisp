;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-grammar-profile-test.lisp
;;;
;;; GRAMMAR PROFILES -- the named/scoped-grammar hook (register-grammar-profile,
;;; grammar-profile-groups, load-grammar). A profile is a keyword -> a list of
;;; rule groups; loading a RESTRICTED profile makes PARSE-SENTENCE reject
;;; (return NIL on) any construction whose rule groups the profile omits, while
;;; still parsing the constructions it keeps. That reject-out-of-scope behaviour
;;; is the whole point of a scoped grammar, so this test pins it down.
;;;
;;; It checks:
;;;   1. The registry ships :full and :marcus, and load-grammar brings them up
;;;      (equivalent to load-full-grammar / load-marcus-grammar).
;;;   2. An unknown profile errors (with the known profiles named).
;;;   3. register-grammar-profile adds a COHERENT restricted subset (NP + basic
;;;      clause + PP), and under it: a simple clause parses, but a that-complement
;;;      -- whose rule group the subset omits -- is REJECTED (NIL), using a
;;;      sentence whose every word is in the lexicon so the failure is purely
;;;      grammatical, not a dropped word.
;;;   4. Re-loading :full restores the omitted construction (the restriction was
;;;      the profile's doing, not a broken grammar).
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-grammar-profile-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)

(defun gram1-grammar-profile-test (&optional verbose)
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
               (setf results (and pass results)))))

      (flet ((parses (sentence)
               (parse-sentence sentence
                               :initial-rule (intern "INITIAL-RULE" :parsifal))))

        ;; 1. The registry ships :full and :marcus; load-grammar brings them up.
        (truthy ":full is a registered profile"   (assoc :full   *grammar-profiles*))
        (truthy ":marcus is a registered profile" (assoc :marcus *grammar-profiles*))
        (truthy "load-grammar :full parses a simple clause"
                (progn (load-grammar :full) (parses "the man sees the dog .")))
        (truthy "load-grammar :full parses a that-complement"
                (parses "the man knows that the boy sees the dog ."))

        ;; 2. An unknown profile errors, naming what is known.
        (truthy "load-grammar of an unknown profile signals an error"
                (nth-value 1 (ignore-errors (load-grammar :no-such-profile))))

        ;; 3. Register a COHERENT restricted subset and load it.
        (register-grammar-profile
         :test-core
         (list *np-rules* *np-utterance-rule* *clause-rules* *vp-np-full-rule*
               *pronoun-rule* *pp-rules*))
        (truthy ":test-core registered" (assoc :test-core *grammar-profiles*))
        (load-grammar :test-core)
        (truthy ":test-core parses a simple clause (in scope)"
                (parses "the man sees the dog ."))
        ;; The that-complement's rule group is NOT in :test-core -> reject it.
        ;; Every word here is in the lexicon, so a NIL is purely grammatical.
        (falsy ":test-core REJECTS a that-complement (out of scope)"
               (parses "the man knows that the boy sees the dog ."))

        ;; 4. Reloading :full restores the omitted construction.
        (load-grammar :full)
        (truthy "reloading :full restores the that-complement"
                (parses "the man knows that the boy sees the dog ."))))

    (format t "~&gram1-grammar-profile-test: ~:[FAILED <<<~;passed~]~%" results)
    results))

(gram1-grammar-profile-test t)
