;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/core/runtime/declr.lisp
;;;
;;; CL port of Marcus's declr.l (the parser's global-state declarations
;;; and a handful of foundational macros for node access and packet
;;; activation).
;;;
;;; Marcus's file is in two halves:
;;;   1. A single `(special ...)` declaration listing every variable
;;;      the wait-and-see main loop and the compiled rules read or
;;;      write.
;;;   2. A small set of macros: FLAGS, SETFLAGS, SETUP**,
;;;      CLEAR-CURRENT-S, ACTIVATENODE, FE, SETFE, SETR, GETR.
;;;
;;; Naming convention (see notes/cl-adaptation.md "Minimal-change
;;; ruleset"):
;;;
;;;   * Marcus uses `:keyword'-prefixed variable names liberally
;;;     (`:activepackets', `:current-s', `:wh-comp', ...). We can't
;;;     keep those because CL's KEYWORD package is constant -- you
;;;     can't SETQ a keyword. They become earmuffed specials --
;;;     `*activepackets*', `*current-s*', `*wh-comp*'.
;;;
;;;   * Plain symbols Marcus declared special (`s', `c', `1st', `2nd',
;;;     `3rd', `nth', `tyo', `tyi', `pred', etc.) are preserved
;;;     verbatim. glang-cl emits these names directly into compiled
;;;     rule bodies (`(attach c 1st 'np)' and so on), so renaming
;;;     would force a parallel change on the parser-output side.
;;;
;;;   * Two name collisions with CL standard names are kept distinct:
;;;     `prinlength' and `prinlevel' are MacLISP names for what CL
;;;     calls `*print-length*' and `*print-level*'. We keep `prinlength'
;;;     and `prinlevel' as separate variables here -- Marcus's parser
;;;     reads its own, not CL's.
;;;
;;; Skipped from Marcus's file:
;;;
;;;   * `(fasl ...)' file-load lines: replaced by ASDF.
;;;   * `(declare (macros t))': MacLISP compiler-mode marker.
;;;   * The commented-out `(fixnum ...)' and `(notype ...)' type
;;;     declarations.
;;;   * `(*lexpr ptree union meet cat semcall)': CL `&rest' already
;;;     covers variadic arglists.
;;;   * The MacLISP-readtable line `(setsyntax '\# 2)' which made `#'
;;;     alphabetic so `###started' would read as a single symbol;
;;;     we use a `|...|'-escaped symbol below.

(in-package :parsifal)


;;; ===========================================================
;;; Special variable declarations
;;; ===========================================================
;;;
;;; All variables here are declaimed special so unqualified references
;;; in compiled rule bodies bind dynamically, matching Marcus's
;;; assumption.

;;; --- Active-packet / rule machinery -----------------------------------
(defvar *activepackets*  nil)
(defvar *activerule*     nil)
(defvar *activenodestak* nil)
(defvar *nextrule*       nil)

;;; --- Look-ahead buffer ------------------------------------------------
;;;
;;; `*bufmax*' tracks the highest occupied buffer index, so an empty
;;; buffer is -1. The buffer array itself is allocated and bound in
;;; buffer-ops.lisp (10-element array, matching parse.l line 5).
(defvar *bufpntr*        0)
(defvar *bufpntrstak*    nil)
(defvar *bufmax*        -1)
(defvar *buffer*         nil)
(defvar *buffer-gc*      nil)

;;; --- Current-sentence / wh-completion --------------------------------
(defvar s                nil)
(defvar c                nil)
(defvar *current-s*      nil)
(defvar *wh-comp*        nil)
(defvar *rset            nil)        ; Marcus's spelling -- one leading *
(defvar *parsecomplete*  nil)

;;; --- Buffer-position registers ---------------------------------------
;;;
;;; `1ST', `2ND', `3RD' are the three buffer positions; `NTH' is the
;;; current cursor. The `feat'/`fvec' pairs cache feature lists and
;;; feature vectors per position so the pattern matcher avoids
;;; recomputing them on every rule test.
(defvar |1ST|            nil)
(defvar |2ND|            nil)
(defvar |3RD|            nil)
(defvar nth              nil)
(defvar *1stfeat*        nil)
(defvar *2ndfeat*        nil)
(defvar *3rdfeat*        nil)
(defvar *1stfvec*        nil)
(defvar *2ndfvec*        nil)
(defvar *3rdfvec*        nil)

;;; --- Rule indexing ---------------------------------------------------
(defvar *int-index*           nil)
(defvar *nr-types*            nil)
(defvar *as-types*            nil)
(defvar *index-to-fvec-alist* nil)
(defvar *findex-counter*      0)

;;; --- Trace / debugging flags ----------------------------------------
(defvar |*###started*|        nil)   ; Marcus's name; `#' was made
                                     ; alphabetic in his readtable
(defvar word-entry-switch     nil)
(defvar *word-entry-switch*   nil)   ; Marcus lists both spellings;
                                     ; kept separate until use clarifies
(defvar testnum               nil)
(defvar tree?                 nil)
(defvar ttysw                 nil)
(defvar bottom-tty            nil)
(defvar top-tty               nil)
(defvar *displayflag*         nil)
(defvar *noshow-defaults*     nil)
(defvar *showrule*            nil)
(defvar *noshowr*             nil)
(defvar *noshowr-defaults*    nil)
(defvar pswait                nil)
(defvar abbr-temp             nil)
(defvar endpuncs              nil)
(defvar poport                nil)
(defvar piport                nil)
(defvar tyo                   nil)
(defvar tyi                   nil)
(defvar ctrace                nil)
(defvar *crtrace*             nil)
(defvar *ptrace*              nil)
(defvar *ntrace*              nil)
(defvar *psych-sw*            nil)
(defvar *carefulsw*           nil)
(defvar *breaksw*             nil)
(defvar *ruletrap*            nil)
(defvar *nodecountsw*         nil)

;;; --- Sentence I/O / derivation ---------------------------------------
(defvar *deriv*               nil)
(defvar *pstatus*             nil)
(defvar *sent-list*           nil)
(defvar *sent*                nil)
(defvar *osent*               nil)
(defvar prinlength            nil)   ; not CL's *print-length*
(defvar prinlevel             nil)   ; not CL's *print-level*
(defvar *wstring*             nil)
(defvar *savewstring*         nil)
(defvar *nodelist*            nil)
(defvar *sentence-types*      nil)

;;; --- Frame / semantic state ------------------------------------------
(defvar hypoth-frames         nil)
(defvar certain-frame         nil)
(defvar openframe             nil)
(defvar pred                  nil)
(defvar old-frames            nil)

;;; --- Misc temporaries and timing -------------------------------------
(defvar *ttemp*               nil)
(defvar *ttemp1*              nil)
(defvar *ttemp2*              nil)
(defvar *ptime*               nil)
(defvar *gctime*              nil)
(defvar *realtime*            nil)
(defvar *endtime*             nil)
(defvar *gram-stats*          nil)
(defvar rule-unfunc           nil)
(defvar rule-func             nil)


(declaim (special
          *activepackets* *activerule* *activenodestak* *nextrule*
          *bufpntr* *bufpntrstak* *bufmax* *buffer* *buffer-gc*
          s c *current-s* *wh-comp* *rset *parsecomplete*
          |1ST| |2ND| |3RD| nth
          *1stfeat* *2ndfeat* *3rdfeat*
          *1stfvec* *2ndfvec* *3rdfvec*
          *int-index* *nr-types* *as-types*
          *index-to-fvec-alist* *findex-counter*
          |*###started*| word-entry-switch *word-entry-switch*
          testnum tree? ttysw bottom-tty top-tty
          *displayflag* *noshow-defaults*
          *showrule* *noshowr* *noshowr-defaults*
          pswait abbr-temp endpuncs poport piport tyo tyi
          ctrace *crtrace* *ptrace* *ntrace*
          *psych-sw* *carefulsw* *breaksw* *ruletrap* *nodecountsw*
          *deriv* *pstatus* *sent-list* *sent* *osent*
          prinlength prinlevel *wstring* *savewstring*
          *nodelist* *sentence-types*
          hypoth-frames certain-frame openframe pred old-frames
          *ttemp* *ttemp1* *ttemp2*
          *ptime* *gctime* *realtime* *endtime* *gram-stats*
          rule-unfunc rule-func))


;;; ===========================================================
;;; Foundational macros
;;; ===========================================================
;;;
;;; Marcus's representation: a node is a cons cell whose CAR is a
;;; gensym (the node's name) and whose CDR is a fixnum of flag bits.
;;; The node's feature list is the symbol-value of the CAR; its
;;; registers live on the CAR's property list. This double-use of the
;;; head symbol is a MacLISP-era memory trick we preserve as-is.

(defmacro flags (node)
  "Flag bits live in a node's CDR."
  `(cdr ,node))

(defmacro setflags (node val)
  `(rplacd ,node ,val))

(defmacro fe (node)
  "Feature list of NODE -- stored as the symbol-value of (car NODE)."
  `(symbol-value (car ,node)))

(defmacro setfe (node features)
  `(setf (symbol-value (car ,node)) ,features))

(defmacro setr (reg value node)
  "Set register REG of NODE to VALUE. Registers live on the property
   list of the node's head symbol."
  `(setf (get (car ,node) ,reg) ,value))

(defmacro getr (register node)
  `(get (car ,node) ,register))


(defmacro clear-current-s ()
  "Clear the current-S and wh-comp slots together (they're entangled --
   *wh-comp* only makes sense relative to *current-s*)."
  `(when *current-s*
     (setq *current-s* nil
           *wh-comp*   nil)))


(defmacro activatenode (node)
  "Push the current node + its packet stack, then switch focus to
   NODE. Mirrors declr.l line 59."
  `(progn
     (when c (push (cons c *activepackets*) *activenodestak*))
     (setq *activepackets* nil)
     (clear-current-s)
     (setq c ,node)))


(defmacro setup** (nname nfeat nfvec)
  "Refresh the feature vector NFVEC and feature list NFEAT for the
   node currently in lexical `NODE'. Mirrors declr.l line 39.

   Note: the body references `node' as a free variable, matching
   Marcus's MacLISP-era assumption that the caller has a lexical
   `NODE' in scope. We keep that contract here -- callers must
   establish NODE before invoking SETUP**.

   Deviation from declr.l: Marcus's source conses TEMP (nil when
   no :findex existed) onto the saved feature list, which only
   works correctly if every feature has been pre-indexed via
   FEATINDEXIFY (defs.l does this for the standard ontology). To
   make setup** robust for features that haven't been touched by
   a pattern yet, we lazily assign :findex inside the loop --
   same logic FEATINDEXIFY uses -- and always store the integer
   index in NFEAT. Cleanup on the next call is then a clean
   pass over a list of small integers."
  `(progn
     ;; Clear the slots currently held by NFEAT.
     (do ((findex ,nfeat (cdr findex)))
         ((null findex))
       (setf (aref ,nfvec (car findex)) 0))
     ;; Mark NFVEC inconsistent while we update it (in case we get
     ;; interrupted mid-way).
     (setq ,nfeat 'changing)
     ;; Walk NODE's feature list; flip the corresponding bit on,
     ;; assigning :findex lazily if needed, and record the index.
     (do ((f (fe node) (cdr f))
          (result nil)
          (idx    nil))
         ((null f) (setq ,nfeat result))
       (setq idx (or (get (car f) :findex)
                     (setf (get (car f) :findex)
                           (prog1 *findex-counter*
                             (incf *findex-counter*)))))
       (setf (aref ,nfvec idx) 1)
       (push idx result))
     (setq ,nname node)))
