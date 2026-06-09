;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/core/runtime/maclisp-chars.lisp
;;;
;;; MacLISP character / symbol primitives that com.l and morpho lean
;;; on heavily but that aren't part of com.l itself -- in Marcus's
;;; world they came from the Lisp or `myutil'. They turn symbols and
;;; numbers into lists of their constituent characters and back, which
;;; is how the lexicon loader and the morphology engine take words
;;; apart (strip suffixes, rebuild roots) and reassemble them.
;;;
;;; A "char-symbol" here is a one-character symbol interned in
;;; :parsifal -- e.g. (explodec 'cat) => (C A T). They are interned
;;; (not gensymed) so they compare EQ against the char-class lists
;;; morpho consults (*vowels*, *numbers*, ...), exactly as Marcus's
;;; obarray-interned characters did.
;;;
;;; Faithful to MacLISP semantics, adapted to CL:
;;;   explodec  object -> list of one-char symbols of its printname
;;;   exploden  object -> list of character CODES of its printname
;;;   implode   list of char-symbols/codes/strings -> interned symbol
;;;   maknam    same, but an UNinterned symbol
;;;   readlist  read one Lisp form from the concatenated printnames
;;;   ascii     character code -> one-char symbol  (CL code-char)
;;;   getchar   object, N -> its Nth character (1-based) as a symbol
;;;   flatc     object -> number of characters in its printname

(in-package :parsifal)


(defun %chars->string (parts)
  "Concatenate PARTS into a string. Each part is rendered as: an
   integer -> the character with that code; anything else -> its
   STRING (a symbol's printname, or a string/character verbatim).
   This is the shared core of IMPLODE / MAKNAM / READLIST."
  (with-output-to-string (out)
    (dolist (part parts)
      (write-string (if (integerp part)
                        (string (code-char part))
                        (string part))
                    out))))

(defun explodec (x)
  "Object -> list of one-character symbols of its printed (PRINC)
   representation, interned in :parsifal. (explodec 'cat) => (C A T),
   (explodec 123) => (|1| |2| |3|)."
  (map 'list
       (lambda (ch) (intern (string ch) :parsifal))
       (princ-to-string x)))

(defun exploden (x)
  "Object -> list of the character CODES of its printed representation.
   (exploden 'ab) => (65 66)."
  (map 'list #'char-code (princ-to-string x)))

(defun implode (parts)
  "List of char-symbols / char-codes / strings -> the interned symbol
   whose name is their concatenation. (implode '(c a t)) => CAT,
   (implode (cons '* (explodec 'cat))) => *CAT."
  (intern (%chars->string parts) :parsifal))

(defun maknam (parts)
  "Like IMPLODE, but returns a fresh UNinterned symbol -- the caller
   wants the name without touching the package."
  (make-symbol (%chars->string parts)))

(defun readlist (parts)
  "Read one Lisp form from the concatenation of PARTS' printnames.
   (readlist (explodec 123)) => 123,
   (readlist '(|(| a b |)|)) => (A B).
   *READ-BASE* is bound to 10: Marcus wraps the numeric uses in
   `(for (ibase 10.) ...)', and base 10 is what every digit-reading
   call site wants. *PACKAGE* is bound to :parsifal so any symbols read
   are interned there -- matching EXPLODEC / IMPLODE / ASCII and the
   single MacLISP obarray, rather than whatever package happens to be
   current at the call site (which df+'s phrase tokens depend on)."
  (let ((*read-base* 10)
        (*package* (find-package :parsifal)))
    (with-input-from-string (in (%chars->string parts))
      (read in nil nil))))

(defun ascii (code)
  "Character CODE -> the one-character symbol for that character,
   interned in :parsifal. (ascii 65) => A."
  (intern (string (code-char code)) :parsifal))

(defun getchar (x n)
  "The Nth character (1-based) of X's printname, as a one-character
   symbol, or NIL if N is out of range. (getchar 'cat 3) => T."
  (let ((s (princ-to-string x)))
    (and (<= 1 n (length s))
         (intern (string (char s (1- n))) :parsifal))))

(defun flatc (x)
  "Number of characters in X's printed representation.
   (flatc 'cat) => 3."
  (length (princ-to-string x)))
