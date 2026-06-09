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


;;; ===========================================================
;;; Phrase display (util.l 545-575)
;;; ===========================================================
;;;
;;; The surface-phrase layer: gather the words a node spans, in input
;;; order, and print them. Used by tracing (`cfprint') and by case.l's
;;; deferred interactive supervisor (`super-smqval' prints the phrase it
;;; is asking the operator to grade).
;;;
;;; `collectw' walks down a node's daughters to its leaf word-nodes.
;;; Marcus's `daughters' register is a disembodied property list and he
;;; recurses over `(cdr (getr 'daughters node))'; the port's `daughters'
;;; register is instead a gensym whose plist carries the type->kids
;;; mappings (see `attach', buffer-ops.lisp), so the equivalent is to
;;; walk `(symbol-plist (getr 'daughters node))' -- which is exactly that
;;; `cdr' (Marcus's list has a leading NIL header; the gensym's plist
;;; does not). Leaf word-nodes are recognised by their `word' register
;;; (set in `nodify', input.lisp) and never reach the daughters branch.

(defun print2 (x)
  "Marcus's no-slashification printer (an undelivered Franz util): print
   X verbatim, i.e. PRINC. Returns X. (See the macros2.lisp note that
   `print2' = PRINC.)"
  (princ x))

(defun collectw (node)
  "Collect (INPUTPOS . WORD) pairs for every leaf word NODE dominates.
   Mirrors util.l 550, adapted to the port's gensym-backed `daughters'."
  (cond ((getr 'word node)
         (list (cons (getr 'inputpos node) (getr 'word node))))
        (t (mapcan (lambda (funcset)
                     (and (not (atom funcset))
                          (mapcan #'collectw funcset)))
                   (symbol-plist (getr 'daughters node))))))

(defun phrasify (node)
  "NODE's surface words in input order, each restored to its original
   case (`origcase'). Mirrors util.l 545. (Marcus's `sortcar' by the
   pair's CAR is CL:SORT keyed on CAR.)"
  (mapcar (lambda (x) (origcase (cdr x)))
          (sort (collectw node) #'< :key #'car)))

(defun prphrase (phrase)
  "Print each word of PHRASE (via PRINT2) and return T. Mirrors
   util.l 559."
  (mapc #'print2 phrase)
  t)

(defun nodes-to-words (nodestring)
  "Flatten a list of nodes into their surface words. Mirrors util.l 562."
  (mapcan #'phrasify nodestring))

(defun cfprint (node)
  "Print NODE's (open) case frame for debugging: a blank gap, the
   surface phrase, the predicate, then each case of CERTAIN-FRAME with
   its phrase. Mirrors util.l 565.

   Two faithful-reproduction notes: (1) Marcus prints the *cases* of the
   global CERTAIN-FRAME, not of NODE's own frame -- reproduced verbatim;
   cfprint relies on CERTAIN-FRAME being bound to the frame of interest.
   (2) Output spacing differs slightly from Marcus's: the port's say-it
   space-separates items where his print2 ran them together."
  (openchek node)
  (terpri)
  (terpri)
  (prphrase (phrasify node))
  (say |   pred:| $ pred)
  (mapc (lambda (case)
          (say |  | $ (car case) |:|)
          (prphrase (phrasify (cadr case))))
        (cadr certain-frame))
  t)
