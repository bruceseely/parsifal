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


;;; ===========================================================
;;; Tree printers (util.l 390-524)
;;; ===========================================================
;;;
;;; Marcus's util.l tree printers (`tree'/`stree' and the case tree
;;; `short-ctree') drew their indentation from the MacLISP terminal
;;; primitives cursorpos/chrct/linel -- which this port stubbed to
;;; no-ops (see the supervisor note below), so the originals were
;;; deferred. These two are faithful in STRUCTURE -- the same recursion
;;; over a node's daughter groups (STREE) and over its case frame
;;; (CTREE), the same trace-binding display and cycle guard -- but
;;; compute indentation from the recursion depth (a plain space prefix)
;;; instead of the cursor, so they need no terminal control. They are
;;; debugging aids for following a parse; nothing in the parser calls
;;; them. Both print to a stream and return no values.

(defun tree-indent (level stream)
  "Indent two spaces per tree LEVEL."
  (dotimes (i (* 2 level)) (write-char #\Space stream)))

(defun leaf-word-node-p (node)
  "A leaf word-node: NODIFY gives it a `word' register and leaves its
   `daughters' register as the symbol WORD (input.lisp)."
  (or (getr 'word node) (eq (getr 'daughters node) 'word)))

(defun node-label (node)
  "Head symbol + feature list, e.g.  S  (DECL MAJOR S)."
  (format nil "~a  ~a" (car node) (fe node)))

(defun node-words (node)
  "NODE's surface words in input order, like PHRASIFY but falling back to
   the word symbol when `origword' is unset (PHRASIFY uses ORIGCASE
   bare, which is NIL unless read-sentence recorded the original case)."
  (mapcar (lambda (x) (or (origcase (cdr x)) (cdr x)))
          (sort (collectw node) #'< :key #'car)))

(defun node-min-pos (node)
  "Leftmost input position of any leaf NODE dominates -- used to print
   daughter groups in surface (left-to-right) order regardless of the
   order ATTACH left them on the daughters plist."
  (let ((ws (collectw node)))
    (if ws (reduce #'min ws :key #'car) most-positive-fixnum)))

(defun daughter-groups (node)
  "List of (FUNCTION . DAUGHTER) pairs for NODE -- each child paired with
   the grammatical function (daughter type: np/aux/vp/verb/...) it fills,
   sorted into surface order. The port's `daughters' register is a gensym
   whose plist is (type1 (kids1) type2 (kids2) ...); ATTACH prepends, so
   each kid list is reversed back to attachment order before sorting."
  (let ((groups '()))
    (do ((pl (symbol-plist (getr 'daughters node)) (cddr pl)))
        ((null pl))
      (when (listp (cadr pl))
        (dolist (kid (reverse (cadr pl)))
          (push (cons (car pl) kid) groups))))
    (sort groups #'< :key (lambda (g) (node-min-pos (cdr g))))))

(defun stree (node &optional (stream *standard-output*))
  "Print NODE's SURFACE-STRUCTURE (constituent) tree: every node as its
   head + feature list, indented by depth, recursing through its
   daughter groups -- each child prefixed by the grammatical function it
   fills (np/aux/vp/verb/...). Leaf word-nodes print their original-case
   word; a trace/* node prints `trace -> <phrase>' for what it is bound
   to. Port of util.l's `tree' with nsw+fsw+funcsw all on."
  (stree1 node nil 0 stream)
  (values))

(defun stree1 (node func level stream)
  (tree-indent level stream)
  (when func (format stream "~(~a~): " func))
  (cond
    ((null node) (format stream "<nil>~%"))
    ((is-any-of node '(trace *))
     (format stream "~a  -> ~(~{~a~^ ~}~)~%"
             (node-label node) (node-words (binding node))))
    ((leaf-word-node-p node)
     (format stream "~(~a~)~%"
             (or (origcase (getr 'word node)) (getr 'word node))))
    (t
     (format stream "~a~%" (node-label node))
     (dolist (g (daughter-groups node))
       (stree1 (cdr g) (car g) (1+ level) stream)))))

(defun ctree (node &optional (stream *standard-output*))
  "Print NODE's CASE tree (the semantics): the predicate, its specifier
   (the aux -- tense/modal/...), then each filled case labelled with the
   grammatical function that filled it (AGT via SUBJ, NEUT via OBJ, ...),
   recursing into fillers that carry their own frame -- so a clausal
   complement unfolds into its embedded predicate. A trace/* filler
   prints the phrase it binds; a filler already on the recursion path (a
   structural cycle) is printed as a back-pointer instead of recursed.
   Port of util.l's `short-ctree'. Flushes the open frame first so a
   still-open matrix PRED is visible (cf. CFPRINT)."
  (closeframe openframe)
  (ctree1 node 0 (list node) stream)
  (values))

(defun ctree1 (node level seen stream)
  (tree-indent level stream)
  (cond
    ((null node) (format stream "<nil>~%"))
    ((is-any-of node '(trace *))
     (format stream "trace -> ~(~{~a~^ ~}~)~%" (node-words (binding node))))
    ((getc 'pred node)
     (format stream "PRED: ~a~%" (getc 'pred node))
     (let ((spec (cadr (getc 'spec node))))
       (when (and spec (consp spec))
         (tree-indent (1+ level) stream)
         (format stream "SPEC: ~a~@[ ~(~{~a~^ ~}~)~]~%"
                 (fe spec) (node-words spec))))
     (dolist (case (reverse (getc 'cases node)))
       (destructuring-bind (cname filler gfunc) case
         (tree-indent (1+ level) stream)
         (format stream "~a via ~a:~%" cname gfunc)
         (cond ((member filler seen :test #'equal)
                (tree-indent (+ 2 level) stream)
                (format stream "<^ ~a ~(~{~a~^ ~}~)>~%"
                        (car filler) (node-words filler)))
               (t (ctree1 filler (+ 2 level) (cons node seen) stream)))))
     (dolist (mod (getc 'mods node))
       (tree-indent (1+ level) stream)
       (format stream "MOD:~%")
       (ctree1 mod (+ 2 level) (cons node seen) stream)))
    ((leaf-word-node-p node)
     (format stream "~(~a~)~%"
             (or (origcase (getr 'word node)) (getr 'word node))))
    (t (format stream "~(~{~a~^ ~}~)~%" (node-words node)))))


;;; ===========================================================
;;; Interactive supervisor (case.l 448-477)
;;; ===========================================================
;;;
;;; These three belong to case.l but were deferred to util.l because
;;; they need the phrase-display layer above. They are the hand-scoring
;;; "supervisor" path: instead of computing a semantic-marker fit,
;;; super-smqval shows the operator a phrase and a frame and asks for a
;;; 0/1/2 grade. The automatic SMQVAL path (case-frame.lisp) is what the
;;; parser actually uses; nothing in the runtime calls these.
;;;
;;; Two dependencies are not in any delivered Marcus source:
;;;   cursorpos -- the MacLISP terminal-control primitive. The trio uses
;;;     only `(cursorpos 'c)' (clear screen), which is cosmetic; ported
;;;     as a no-op stub. (Its query / positioning forms, used by the
;;;     deferred tree printers, can be filled in when those land.)
;;;   fitspg1   -- the fit-possibility generator super-fit1-of feeds to
;;;     super-smqval. Undelivered; stubbed to signal a clear error so the
;;;     dormancy is explicit (a NIL stub would instead fail obscurely in
;;;     super-fit1-of's `(apply #'max ())').

(defvar super-clears t
  "Supervisor screen-clear control (case.l 446): T clears the screen
   before each query, NIL leaves it, and `no-super' disables the
   supervisor entirely (super-smqval just returns a neutral 0).")

(defun cursorpos (&rest args)
  "No-op stub for MacLISP's terminal cursor primitive (undelivered).
   The supervisor uses only `(cursorpos 'c)' (clear screen), which is
   cosmetic. Returns NIL."
  (declare (ignore args))
  nil)

(defun fitspg1 (node caseset all)
  "Undelivered in every Marcus source -- the supervisor's fit-possibility
   generator (case.l 476). Stubbed to signal so the interactive path's
   dormancy is explicit rather than failing obscurely downstream."
  (declare (ignore node caseset all))
  (error "fitspg1 is not in any delivered Marcus source; the interactive ~
          supervisor (super-fit-of / super-fit1-of) cannot run until it ~
          is supplied."))

(defun super-smqval (case node frame)
  "Ask the operator to grade how well NODE fills CASE in FRAME: show the
   phrase and frame, read 0 (bad) / 1 (ok) / 2 (good), and map them to
   the SMQVAL scale -2 / 0 / 1. Mirrors case.l 448. (Marcus's `prog' +
   `huh?' retry loop becomes a LOOP; `lessp' is `<'.)"
  (cond ((eq super-clears 'no-super) 0)
        (t (when super-clears (cursorpos 'c))
           (terpri)
           (print '|________________|)
           (say |How much do you like| $ (phrasify node) |as| $ case |with|)
           (cfprint frame)
           (let ((temp (loop
                         (say |
            0 - bad
            1 - ok
            2 - good
            --->|)
                         (let ((in (read)))
                           (when (and (numberp in) (< -1 in 4))
                             (return in))))))
             (terpri)
             (print '|________________|)
             (cond ((= temp 0) -2)
                   ((= temp 1) 0)
                   ((= temp 2) 1))))))

(defun super-fit1-of (node caseset framenode)
  "Grade NODE against every fit-possibility CASESET offers (via FITSPG1)
   and return the best operator score. Mirrors case.l 473. (Marcus's
   one-shot `do' is a LET.)"
  (apply #'max
         (mapcar (lambda (case) (super-smqval case node framenode))
                 (let ((temp (fitspg1 node caseset t)))
                   (append (car temp) (cdr temp))))))

(defmacro super-fit-of (node-form caseset-form)
  "Supervisor counterpart of FIT-OF. Mirrors case.l 470, where it is a
   fexpr that evaluates its NODE and CASESET arguments and *also*
   evaluates the last element of the CASESET form as the frame node
   (`(eval (car (last (cadr l))))'). Reproduced as a macro: the frame
   node is the last element of CASESET-FORM."
  `(super-fit1-of ,node-form ,caseset-form ,(car (last caseset-form))))
