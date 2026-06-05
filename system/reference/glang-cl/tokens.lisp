;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/reference/glang-cl/tokens.lisp
;;;
;;; A small character-driven tokenizer for Marcus's grammar-language source
;;; text. Mirrors the behaviour of his MacLISP readtable hack in glang.l
;;; lines 60-98: the characters `[]{}()';,.=' are each a single-character
;;; symbol; everything else accumulates into one symbol or number until a
;;; separator. Whitespace, %...% inline comments, `;;...' line comments,
;;; and `(comment ...)' section comments are skipped.
;;;
;;; This intentionally does not reuse our existing PIDGIN-RULE-LEXER:
;;; Marcus's reader treats `:', `*', `-' as identifier characters (so
;;; `PRIORITY:', `*ten', `-->' each read as one symbol) and our other
;;; lexer splits them. Doing a custom pass here is simpler than building
;;; an adapter that re-glues those cases.

(in-package #:glang-cl)


(defvar *eof* (gensym "EOF")
  "Sentinel returned by the token stream when input is exhausted.
   Use EQ to compare.")


(defparameter +single-char-symbols+
  '(#\[ #\] #\{ #\} #\( #\) #\' #\; #\, #\. #\=)
  "Characters that each form their own single-character symbol token,
   matching Marcus's `(explodec '|[]\\{}()',.=|)' setsyntax line.")


(defun single-char-symbol-p (char)
  (member char +single-char-symbols+))


(defun whitespace-char-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return #\Page)))


(defun token-terminator-p (char)
  (or (whitespace-char-p char) (single-char-symbol-p char)))


(defun tokenize (string)
  "Return a list of tokens parsed from STRING. Each token is either a
   symbol (interned in #:glang-cl) or an integer."
  (let ((pos 0)
        (n (length string))
        (tokens '()))
    (labels
        ((peek () (when (< pos n) (char string pos)))

         (skip-line-comment ()
           ;; require at least 2 leading `;'
           (loop while (and (< pos n) (char= (char string pos) #\;))
                 do (incf pos))
           (loop while (and (< pos n) (char/= (char string pos) #\Newline))
                 do (incf pos)))

         (skip-percent-comment ()
           (incf pos)                                   ; opening %
           (loop while (and (< pos n) (char/= (char string pos) #\%))
                 do (incf pos))
           (when (< pos n) (incf pos)))                 ; closing %

         (skip-comment-form ()
           ;; (comment ... ) -- skip if the parenthesised form starts
           ;; with the literal word `comment'.
           (let ((save pos))
             (incf pos)                                 ; the `('
             (let ((id-start pos))
               (loop while (and (< pos n) (alpha-char-p (char string pos)))
                     do (incf pos))
               (cond
                 ((and (= 7 (- pos id-start))
                       (string-equal "comment" string
                                     :start2 id-start :end2 pos))
                  (loop while (and (< pos n) (char/= (char string pos) #\)))
                        do (incf pos))
                  (when (< pos n) (incf pos))           ; closing `)'
                  t)
                 (t
                  (setf pos save)
                  nil)))))

         (skip-whitespace-and-comments ()
           (loop while (< pos n) do
             (let ((c (peek)))
               (cond
                 ((whitespace-char-p c) (incf pos))
                 ((and (char= c #\;)
                       (< (1+ pos) n)
                       (char= (char string (1+ pos)) #\;))
                  (skip-line-comment))
                 ((char= c #\%)
                  (skip-percent-comment))
                 ((char= c #\()
                  (unless (skip-comment-form) (return)))
                 (t (return)))))))

      (loop
        (skip-whitespace-and-comments)
        (unless (< pos n) (return))
        (let ((c (peek)))
          (cond
            ((single-char-symbol-p c)
             (incf pos)
             (push (intern (string c) :glang-cl) tokens))
            (t
             (let ((start pos))
               (loop while (and (< pos n)
                                (not (token-terminator-p (char string pos))))
                     do (incf pos))
               (let ((text (subseq string start pos)))
                 (multiple-value-bind (num idx)
                     (parse-integer text :junk-allowed t)
                   (cond
                     ((and num (= idx (length text)))
                      (push num tokens))
                     (t
                      ;; Upcase to match CL's default reader (so source
                      ;; code `add' and tokenized "add" produce the
                      ;; same symbol ADD). Marcus uses lowercase via
                      ;; his `uctolc' flag; we may revisit when
                      ;; matching his compiled output exactly.
                      (push (intern (string-upcase text) :glang-cl)
                            tokens)))))))))))
    (nreverse tokens)))


(defun make-token-stream (string)
  "Return a thunk that yields successive tokens from STRING, then *EOF*.
   Suitable for the Pratt parser's ADVANCE."
  (let ((toks (tokenize string)))
    (lambda ()
      (if toks (pop toks) *eof*))))
