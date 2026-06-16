;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/core/runtime/macros2.lisp
;;;
;;; CL port of Marcus's macros2.l (57 lines). That file is mostly a
;;; MacLISP-dialect support layer: printing helpers (`say', `warn'),
;;; renaming macros for an external set-operations library
;;; (`util/set'), and a couple of arg/bit shims. Under Common Lisp
;;; almost all of it dissolves into the standard library, so the only
;;; runtime code this file actually contributes is the `say' trace
;;; macro and its printer. The rest is accounted for below so the port
;;; is auditable against the original.
;;;
;;; PORTED HERE
;;;   say          -- trace/print macro. `(say a b $ x)' prints the
;;;                   literal tokens a, b and then the *value* of x;
;;;                   the `$' marker flips an item from quoted to
;;;                   evaluated. Expands to a SAY-IT call.
;;;   say-it       -- the variadic printer SAY expands into. Marcus
;;;                   keeps the macro (quote/escape transform) and the
;;;                   printer (terminal output) separate; we preserve
;;;                   that split. This is the minimal princ-based
;;;                   version; util.l's richer say-it1/say1 (the `$$'
;;;                   splice, cursor/highlight control) supersede it
;;;                   when util.l is ported.
;;;
;;; DISSOLVED INTO CL NATIVES (handled at each call site when the
;;; consuming file -- parse.l, case.l -- is ported)
;;;   warn         -- Marcus's `(warn LEVEL items...)' becomes CL's
;;;                   native CL:WARN with a string message at each
;;;                   hand-ported call site; the numeric severity level
;;;                   is dropped. Already done this way in buffer-ops.lisp.
;;;                   Defining a `warn' macro here would shadow CL:WARN, so
;;;                   we deliberately do not. (Grammar rules can also embed
;;;                   a warn via the `!'-escape, which glang-cl emits
;;;                   verbatim; those are written `(warner ...)' -- the
;;;                   rewrite Marcus's own `warn' macro performs -- and the
;;;                   `warner' macro ported below handles them. See it.)
;;;   meet  -> intersectq    } eq-based set ops from Marcus's external
;;;   meet1 -> intersectq2   } util/set library. On lists of feature
;;;   union1-> unionq2       } symbols, CL:INTERSECTION / CL:UNION /
;;;   setminus               } CL:SET-DIFFERENCE (default EQL test) are
;;;                            identical. Where Marcus relies on
;;;                            survivor order (setminus), use the
;;;                            REMOVE-IF idiom as buffer-ops.lisp does
;;;                            in DEACTIVATE.
;;;   logand / logor -- CL:LOGAND / CL:LOGOR are native (Marcus defined
;;;                     them over `boole'); nothing to port.
;;;
;;; DROPPED (zero call sites anywhere in parse.l/case.l/com.l/util.l
;;; and natively available if ever needed)
;;;   meet2, setminus1, set-union, set-difference, set-intersection,
;;;   make-eq-set (-> setifyq), make-equal-set (-> setify).
;;;   set-difference in particular must NOT be defined as a macro -- it
;;;   is the name of a CL standard function.
;;;
;;; PORTED IN util.l
;;;   cat -> concat  -- symbol synthesis by concatenating printnames.
;;;                     The three parse.l uses are already obsoleted by
;;;                     this port's design: makesym (node-ops.lisp)
;;;                     builds node symbols, and act-of-rule
;;;                     (parse-loop.lisp) replaces `(cat ':act-of- ..)'
;;;                     with a plist lookup. The remaining uses are all
;;;                     in util.l (date/dump strings); cat/concat now
;;;                     live in util.lisp.
;;;
;;; DEFERRED TO util.l (port-time translation, alongside its caller)
;;;   default-arg    -- variadic-arg default; becomes an &optional
;;;                     parameter default at port time. Its call sites
;;;                     are all in util.l's `ptree', so it ports when
;;;                     ptree does.
;;;
;;; The MacLISP pragmas `(declare (macros t))' and the `(*lexpr ...)'
;;; declaration have no CL equivalent and are dropped.

(in-package :parsifal)


;;; ===========================================================
;;; say -- trace/print macro (macros2.l lines 3-11)
;;; ===========================================================
;;;
;;; Marcus walks the argument list quoting every token except the one
;;; following a `$', which is left to be evaluated, then hands the
;;; whole list to the SAY-IT lexpr. We reproduce that transform at
;;; macroexpansion time: bare items become (QUOTE item), and `$ x'
;;; contributes the unquoted form x.

(defmacro say (&rest items)
  "Print a trace line. Bare tokens are printed literally; a token
   following `$' is evaluated and its value printed. E.g.
   (say |Running| $ type |rule| $ (car *activerule*))."
  `(say-it ,@(loop with rest = items
                   while rest
                   for item = (pop rest)
                   if (eq item '$)
                     collect (pop rest)        ; `$ x' -> evaluate x
                   else
                     collect `(quote ,item)))) ; bare token -> quoted


(defun say-it (&rest items)
  "Minimal port of Marcus's SAY-IT (util.l say-it1): start a new line
   then PRINC each item, space-separated. We keep Marcus's leading
   TERPRI (unconditional newline) rather than FRESH-LINE. Marcus's
   `print2' was a no-slashification printer, i.e. PRINC. The
   terminal-control extras (the `$$' splice marker, cursor/highlight
   sequences) live in util.l's say-it1 and will be folded in when
   util.l is ported."
  (terpri)
  (loop for (item . more) on items
        do (princ item)
        when more do (write-char #\Space))
  (values))


;;; ===========================================================
;;; warner -- diagnostic warning (macros2.l 37-49)
;;; ===========================================================
;;;
;;; Marcus's `warn' is a macro that rewrites `(warn LEVEL items...)' to a
;;; `warner' *lexpr call, quoting each item except one following `$' (the
;;; same splice transform as SAY). The runtime's own hand-ported
;;; diagnostics just call CL:WARN with a string (the numeric severity
;;; LEVEL is dropped) -- but a grammar rule can also embed a warn via the
;;; `!'-escape (glang.l), and glang-cl emits that verbatim. CL:WARN cannot
;;; take Marcus's numeric level as its datum, so those grammar warns are
;;; written `(warner ...)' -- exactly the rewrite Marcus's `warn' macro
;;; performs -- and routed through this macro, which does the quoting and
;;; calls CL:WARN with a real format-string datum. (We define `warner',
;;; not `warn', so CL:WARN is never shadowed.)

(defmacro warner (level &rest items)
  "Marcus's `warner' (the target his `warn' macro rewrites to): print a
   diagnostic warning. LEVEL is a severity number; ITEMS are message
   tokens, quoted like SAY except a token following `$' is evaluated.
   Routes to CL:WARN with a real string datum. Returns NIL."
  `(progn
     (warn "[parsifal warn ~a] ~{~a~^ ~}"
           ,level
           (list ,@(loop with rest = items
                         while rest
                         for item = (pop rest)
                         if (eq item '$)
                           collect (pop rest)        ; `$ x' -> evaluate x
                         else
                           collect `(quote ,item)))) ; bare token -> quoted
     nil))
