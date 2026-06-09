;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/core/runtime/util.lisp
;;;
;;; util.l is Marcus's operator / debugging shell: the top-level read
;;; loop, trace switches, tree printers, and the terminal-control
;;; "movie" machinery. Most of it is the interactive environment around
;;; the parser, not the parser itself, and much of it is tied to MacLISP
;;; terminal primitives (`cursorpos', split-screen) that have no modern
;;; equivalent. So util.l ports in increments, bottom-up, taking the
;;; pieces the ported runtime (and the upcoming display / supervisor
;;; layers) actually need.
;;;
;;; Increment 1: the symbol-synthesis utility `concat' / `cat'.
;;; Marcus defines these in macros2.l (lines 50, 55) but their consumers
;;; live here, so the macros2 port deferred them to util.l (see the
;;; "DEFERRED TO util.l" note in macros2.lisp).
;;;
;;;   concat  any number of objects -> the interned symbol whose name is
;;;           their concatenated printnames. MacLISP's `concat'.
;;;   cat     the macro Marcus writes at call sites; `(cat a b)' rewrites
;;;           to `(concat a b)'.

(in-package :parsifal)


;;; ===========================================================
;;; Symbol synthesis -- concat / cat (macros2.l 50, 55)
;;; ===========================================================
;;;
;;; `concat' builds a symbol by stringing together its arguments'
;;; printnames -- e.g. (concat ':act-of- 'run) => :ACT-OF-RUN,
;;; (concat 'g 5) => G5. Unlike IMPLODE (maclisp-chars.lisp), which
;;; reads an integer argument as a character CODE, every CONCAT argument
;;; contributes its PRINC representation, so the number 5 contributes the
;;; digit "5", not character 5. Interned in :parsifal, matching the rest
;;; of the port's single-obarray convention (cf. IMPLODE / READLIST).

(defun concat (&rest args)
  "The interned symbol whose printname is the concatenation of ARGS'
   printnames. Mirrors MacLISP `concat' (macros2.l 50 rewrites `cat' to
   it). Each arg contributes its PRINC string, so numbers contribute
   their digits."
  (intern (apply #'concatenate 'string (mapcar #'princ-to-string args))
          :parsifal))

(defmacro cat (&rest args)
  "Marcus's `cat' (macros2.l 50: `(defun cat macro (x) (rplaca x
   'concat))') -- shorthand that rewrites to a CONCAT call."
  `(concat ,@args))
