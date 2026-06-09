;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-

(in-package :parsifal)

;;; Tests for system/core/runtime/util.lisp.
;;;
;;; (util-test)   -> t if all pass, nil otherwise
;;; (util-test t) -> verbose

(defun util-test (&optional verbose)
  (let ((results t))
    (flet ((check (test-name actual expected)
             (let ((pass (eql actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%"
                           expected actual)))
               (setf results (and pass results)))))

      ;; --- concat ----------------------------------------------------

      (check "concat joins two symbols' printnames"
             (concat 'foo 'bar) (intern "FOOBAR" :parsifal))
      (check "concat appends a number as its digits, not a char code"
             (concat 'g 5) (intern "G5" :parsifal))
      (check "concat synthesizes a prefixed rule name from a symbol"
             (concat 'act-of- 'run) (intern "ACT-OF-RUN" :parsifal))
      ;; A string contributes its characters verbatim (case-exact),
      ;; while a symbol's printname is upcased by the reader -- so a
      ;; lowercase string part stays lowercase. (concat is normally
      ;; called with symbols/numbers; this just pins the behavior.)
      (check "concat keeps a string part's case verbatim"
             (concat "new" 'york) (intern "newYORK" :parsifal))
      (check "concat interns in :parsifal (EQ to the read-syntax symbol)"
             (concat 'a 'b) 'ab)
      (check "concat of no args is the empty symbol"
             (concat) (intern "" :parsifal))

      ;; --- cat (macro shorthand for concat) --------------------------

      (check "cat expands to a concat call"
             (cat 'foo 'bar) (concat 'foo 'bar))
      (check "cat builds the same prefixed rule name"
             (cat 'act-of- 'run) (intern "ACT-OF-RUN" :parsifal)))

    results))
