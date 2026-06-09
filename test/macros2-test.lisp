;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-

(in-package :parsifal)

;;; Tests for system/core/runtime/macros2.lisp.
;;;
;;; (macros2-test)   -> t if all pass, nil otherwise
;;; (macros2-test t) -> same, but prints every case as it runs
;;;
;;; macros2.l mostly dissolves into CL natives; the only runtime code
;;; it contributes is the SAY trace macro and its SAY-IT printer, so
;;; that is all there is to exercise here.


(defun macros2-test (&optional verbose)
  (let ((results t))
    (flet ((check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%"
                           expected actual)))
               (setf results (and pass results)))))

      ;; --- SAY macroexpansion: bare tokens quote, `$ x' evaluates -----

      (check "say quotes bare tokens, leaves $-escaped forms raw"
             (macroexpand-1 '(say |Running| $ type |rule| $ rule))
             '(say-it '|Running| type '|rule| rule))

      (check "say with no arguments expands to a bare say-it"
             (macroexpand-1 '(say))
             '(say-it))

      (check "leading $ escapes the first item"
             (macroexpand-1 '(say $ (+ 2 3) |items|))
             '(say-it (+ 2 3) '|items|))


      ;; --- SAY output: literals printed as-is, $-values evaluated -----

      (let ((type 'np) (rule 'parse-subj))
        (check "say prints literals and evaluated values, space-separated"
               (string-trim '(#\Newline)
                            (with-output-to-string (*standard-output*)
                              (say |Running| $ type |rule| $ rule)))
               "Running NP rule PARSE-SUBJ"))

      (check "say evaluates a $-escaped expression"
             (string-trim '(#\Newline)
                          (with-output-to-string (*standard-output*)
                            (say $ (+ 2 3) |items|)))
             "5 items")


      ;; --- SAY-IT directly: fresh-line + space-separated princ -------

      (check "say-it princs items space-separated, no trailing space"
             (string-trim '(#\Newline)
                          (with-output-to-string (*standard-output*)
                            (say-it 'a 'b 'c)))
             "A B C")

      (check "say-it of nothing emits just the fresh line"
             (with-output-to-string (*standard-output*)
               (say-it))
             (string #\Newline)))

    results))
