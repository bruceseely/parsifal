;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-unknown-words-test.lisp
;;;
;;; Diagnosing MISSING-WORD failures. READ-SENTENCE silently drops any
;;; token MORPHO cannot resolve, so a perfectly grammatical sentence with
;;; one out-of-vocabulary word just returns NIL -- indistinguishable, at a
;;; glance, from a real grammar gap. Two aids make the cause visible:
;;;
;;;   (unknown-words STRING)   -- the tokens the lexicon doesn't have
;;;                               (NIL if every word is known);
;;;   *warn-unknown-words*     -- when non-NIL (default), READ-SENTENCE
;;;                               signals an UNKNOWN-WORDS-WARNING listing
;;;                               what it dropped. It is a dedicated
;;;                               WARNING subclass (not a SIMPLE-WARNING)
;;;                               so it survives a user init file that does
;;;                               `(setf sb-ext:*muffled-warnings* 'simple-warning)'.
;;;
;;; (The original trigger was "I saw the man with the telescope ." failing
;;; because `telescope' was absent; it is now in the supplement, so this
;;; test uses a still-out-of-vocabulary word, `wizard', instead.)
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-unknown-words-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-unknown-words-test (&optional verbose)
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

      (load-full-grammar)

      ;; (1) the detector: pinpoints the out-of-vocabulary token.
      (check "unknown-words finds the missing word"
             (unknown-words "I saw the wizard .") '("wizard"))
      (check "unknown-words is NIL when every word is known"
             (unknown-words "the boy meets the boy .") nil)
      (check "unknown-words handles several missing words in order"
             (unknown-words "the wizard zapped the goblin .")
             '("wizard" "zapped" "goblin"))

      ;; (2) parsing an out-of-vocabulary sentence signals the warning
      ;; (and still returns NIL, the word having been dropped).
      (let ((warned nil) (result :unset))
        (handler-bind ((unknown-words-warning
                         (lambda (c)
                           (setf warned (copy-list (unknown-words-of c)))
                           (muffle-warning c))))
          (setf result (parse-sentence "I saw the wizard ."
                                       :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (truthy "an UNKNOWN-WORDS-WARNING was signalled" warned)
        (check "the warning names the dropped word" warned '("wizard"))
        (check "the parse still returns NIL (word was dropped)" result nil))

      ;; (3) *warn-unknown-words* NIL suppresses the warning.
      (let ((warned nil)
            (*warn-unknown-words* nil))
        (handler-bind ((unknown-words-warning
                         (lambda (c) (declare (ignore c)) (setf warned t))))
          (parse-sentence "I saw the wizard ."
                          :initial-rule (intern "INITIAL-RULE" :parsifal)))
        (check "no warning when *warn-unknown-words* is NIL" warned nil))

      ;; (4) a fully-known sentence neither warns nor (here) fails.
      (let ((warned nil))
        (handler-bind ((unknown-words-warning
                         (lambda (c) (declare (ignore c)) (setf warned t))))
          (truthy "a fully-known sentence still parses"
                  (parse-sentence "the boy meets the boy ."
                                  :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "no warning for a fully-known sentence" warned nil)))

    (format t "~&gram1-unknown-words-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-unknown-words-test t)
