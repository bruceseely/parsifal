;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-

(in-package :parsifal)

;;; Tests for system/core/runtime/maclisp-chars.lisp.
;;;
;;; (maclisp-chars-test)   -> t if all pass, nil otherwise
;;; (maclisp-chars-test t) -> verbose


(defun maclisp-chars-test (&optional verbose)
  (let ((results t))
    (flet ((check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%"
                           expected actual)))
               (setf results (and pass results)))))

      ;; --- explodec / exploden ---------------------------------------

      (check "explodec breaks a symbol into one-char symbols"
             (mapcar #'symbol-name (explodec 'cat))
             '("C" "A" "T"))
      (check "explodec interns into :parsifal (EQ to the letter symbols)"
             (explodec 'cat)
             (list 'c 'a 't))
      (check "explodec of a number gives digit symbols"
             (mapcar #'symbol-name (explodec 123))
             '("1" "2" "3"))
      (check "exploden gives character codes"
             (exploden 'ab)
             '(65 66))

      ;; --- implode / maknam ------------------------------------------

      (check "implode of char-symbols rebuilds the interned symbol"
             (implode (explodec 'cat))
             'cat)
      (check "implode mixes a marker symbol with exploded chars"
             (implode (cons '* (explodec 'cat)))
             '*cat)
      (check "implode accepts raw character codes"
             (implode '(67 65 84))
             'cat)
      (check "maknam builds the right name"
             (symbol-name (maknam (explodec 'cat)))
             "CAT")
      (check "maknam returns an UNinterned symbol"
             (eq (maknam (explodec 'cat)) 'cat)
             nil)

      ;; --- readlist --------------------------------------------------

      (check "readlist reads a number from digit symbols"
             (readlist (explodec 123))
             123)
      ;; Printnames are concatenated with no separators (this is what
      ;; df+ relies on to read a phrase into a single symbol), so a
      ;; multi-element list needs an explicit space symbol between
      ;; tokens.
      (check "readlist concatenates with no separators"
             (readlist '(|(| a b |)|))
             '(ab))
      (check "readlist reads a multi-element list across a space"
             (readlist (list '|(| 'a '| | 'b '|)|))
             '(a b))

      ;; --- ascii / getchar / flatc -----------------------------------

      (check "ascii maps a code to its one-char symbol"
             (ascii 65)
             'a)
      (check "ascii of a digit code matches the digit symbol"
             (ascii 49)
             '|1|)
      (check "getchar pulls the Nth (1-based) character"
             (getchar 'cat 3)
             't)
      (check "getchar with flatc reaches the last character"
             (getchar 'cat (flatc 'cat))
             't)
      (check "getchar out of range is NIL"
             (getchar 'cat 9)
             nil)
      (check "flatc counts printname characters"
             (flatc 'cat)
             3))

    results))
