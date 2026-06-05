;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/reference/glang-cl/pratt-test.lisp
;;;
;;; Validates the Pratt parser machinery using arithmetic-style test
;;; denotations. These denotations are NOT part of the real glang
;;; vocabulary -- they are installed only for the duration of the test
;;; and removed afterwards. The goal is to prove the Pratt loop +
;;; fixity macros work in isolation, before we layer on Marcus's actual
;;; rule denotations.

(in-package #:glang-cl)


(defun install-pratt-test-denotations ()
  ;; left-associative infix `add', lower precedence
  (infix add 10 (list 'add *left* (right)))
  ;; left-associative infix `mul', higher precedence
  (infix mul 20 (list 'mul *left* (right)))
  ;; prefix grouping `('
  (prefix |(| 0 (prog1 (right) (check '|)|)))
  ;; `)' is just a delimiter
  (delim |)|))


(defun clear-pratt-test-denotations ()
  (dolist (sym '(add mul |(| |)|))
    (remprop sym :nud)
    (remprop sym :led)
    (remprop sym :lbp)))


(defun pratt-test (&optional verbose)
  (install-pratt-test-denotations)
  (unwind-protect
       (let ((results t))
         (flet ((check (name actual expected)
                  (let ((pass (equal actual expected)))
                    (when (or verbose (not pass))
                      (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass name)
                      (unless pass
                        (format t "    expected: ~s~%    actual:   ~s~%"
                                expected actual)))
                    (setf results (and pass results)))))

           ;; --- tokenizer ---

           (check "tokenize empty input"
                  (tokenize "")
                  '())

           (check "tokenize integers and identifiers"
                  (tokenize "1 add 2 mul 3")
                  '(1 add 2 mul 3))

           (check "tokenize single-char punctuation"
                  (tokenize "{rule [foo]}")
                  '(|{| rule |[| foo |]| |}|))

           (check "tokenize compound tokens (`-->', `PRIORITY:')"
                  (tokenize "RULE FOO PRIORITY: 5 IN BAR -->")
                  '(rule foo |PRIORITY:| 5 in bar -->))

           (check "tokenize skips %...% inline comments"
                  (tokenize "%a comment% 5")
                  '(5))

           (check "tokenize skips ;; line comments"
                  (tokenize (format nil ";; comment~%5"))
                  '(5))

           (check "tokenize skips (comment ...) section comments"
                  (tokenize "(comment a section) 5")
                  '(5))


           ;; --- parser: atoms passing through ---

           (check "parse a single integer"
                  (parse-string "5")
                  5)

           (check "parse a single symbol"
                  (parse-string "foo")
                  'foo)


           ;; --- parser: infix and precedence ---

           (check "parse a single infix expression"
                  (parse-string "1 add 2")
                  '(add 1 2))

           (check "left-associative chain"
                  (parse-string "1 add 2 add 3")
                  '(add (add 1 2) 3))

           (check "mul binds tighter than add (right-side)"
                  (parse-string "1 add 2 mul 3")
                  '(add 1 (mul 2 3)))

           (check "mul binds tighter than add (left-side)"
                  (parse-string "1 mul 2 add 3")
                  '(add (mul 1 2) 3))

           (check "mul on both sides of add"
                  (parse-string "1 mul 2 add 3 mul 4")
                  '(add (mul 1 2) (mul 3 4)))


           ;; --- parser: grouping ---

           (check "parens force lower-precedence grouping"
                  (parse-string "(1 add 2) mul 3")
                  '(mul (add 1 2) 3))

           (check "nested parens"
                  (parse-string "((1 add 2) mul (3 add 4))")
                  '(mul (add 1 2) (add 3 4))))

         (when verbose
           (format t "~&pratt-test: ~:[FAILED~;passed~]~%" results))
         results)
    (clear-pratt-test-denotations)))
