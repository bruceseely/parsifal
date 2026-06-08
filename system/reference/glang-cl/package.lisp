;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/reference/glang-cl/package.lisp
;;;
;;; Package definition for the Common Lisp port of Marcus's grammar-language
;;; parser (glang.l in notes/from-marcus/). See README in this directory
;;; for the porting strategy.

(defpackage #:glang-cl
  ;; :use #:parsifal -- so unqualified references in denotation source
  ;; (e.g. `(prefix attach 10 ...)') resolve to the PARSIFAL runtime
  ;; symbols, and the tokenizer's `(intern "ATTACH" :glang-cl)' finds
  ;; the inherited PARSIFAL:ATTACH. Symbols that don't exist in
  ;; :parsifal (rule names, feature symbols, packet names) are still
  ;; interned into :glang-cl and form a separate, data-only namespace.
  (:use #:cl #:parsifal)
  (:nicknames #:glang)
  (:export
   ;; --- entry points ---
   #:tokenize
   #:make-token-stream
   #:parse-string

   ;; --- Pratt parser machinery ---
   #:advance
   #:verify
   #:associate
   #:pratt-parse
   #:right
   #:eat-token
   #:check
   #:is-token
   #:not-token
   #:parse-list
   #:lederr

   ;; --- fixity macros ---
   #:nilfix
   #:prefix
   #:suffix
   #:infix
   #:infixr
   #:infixd
   #:delim

   ;; --- special variables ---
   #:*token*
   #:*left*
   #:*drbp*
   #:*rulename*
   #:*token-stream*
   #:*eof*

   ;; --- error condition ---
   #:glang-parse-error))
