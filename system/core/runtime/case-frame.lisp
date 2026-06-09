;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/core/runtime/case-frame.lisp
;;;
;;; The case-frame mechanism from case.l (Marcus's Appendix E), ported
;;; in increments. This first increment is the data type and its access
;;; / open-close caching -- the foundation the scoring, hypothesis
;;; generation, and slot-filling build on.
;;;
;;; A case-frame is a gensym (made by MAKESYM) whose plist holds:
;;;   case-frame   NORMAL or MOD (the frame type)
;;;   assoc-node   the node this frame is for
;;;   pred         the root word heading the frame
;;;   cases        the filled <slot>s
;;;   hypo-slots   the <slotframe>s -- ways to fill the remaining slots
;;;   objs-needed  count of obligatory slots still unfilled
;;; A node points at its frame through its `caseframe' register.
;;; (See Kurt Van Lehn's data-type comment at case.l lines 4-46.)
;;;
;;; PRED, HYPO-SLOTS, and OBJS-NEEDED are cached in like-named special
;;; variables while a frame is "open"; OPENFRAME/CLOSEFRAME/OPENCHEK
;;; swap them in and out of the frame's plist.
;;;
;;; Notes:
;;; - `openframe' is both a special var (the currently open frame) and a
;;;   function (open a frame); CL lets a symbol hold both, as Marcus
;;;   relies on.
;;; - `putc' (case.l 537) guards on the node's `caseframe' but writes to
;;;   its `case-frame' prop, while `getc' reads `caseframe' -- they don't
;;;   round-trip. Reproduced as-is; `putc's only caller (alt-fillslot)
;;;   stores ambiguity data nothing yet reads, and `getc' is unused.

(in-package :parsifal)


;;; Case specials not already declared in declr.lisp.
(defvar hypo-slots '((nil nil))
  "Open frame's <slotframe>s -- the ways left to fill its slots.")
(defvar objs-needed 0
  "Open frame's count of obligatory slots still unfilled.")


;;; ===========================================================
;;; Creation and access (case.l 52-82)
;;; ===========================================================

(defun newcf (type)
  "Make a fresh case-frame of TYPE (NORMAL/MOD/...), make it the open
   frame, and return it. Mirrors case.l 52."
  (clearcf)
  (let ((cf (makesym 'casefr)))
    (setq openframe cf)
    (setf (get cf 'case-frame) type)
    cf))

(defun case-frame (node)
  "NODE's associated case-frame, if any. Mirrors case.l 59."
  (getr 'caseframe node))

(defun associate-cf (cf node)
  "Attach CF to NODE (and, the first time, record NODE as CF's
   assoc-node and announce it to semantics). Mirrors case.l 61."
  (setr 'caseframe cf node)
  (unless (get cf 'assoc-node)
    (setf (get cf 'assoc-node) node)
    (semcall 'newframe cf node (get cf 'case-frame))))

(defun assoc-node (cf)
  "The node CF is a frame for. Mirrors case.l 67."
  (get cf 'assoc-node))

(defun open-obj-cases (slotfr)
  "How many object slots SLOTFR still leaves open: 0 if none, 2 for an
   *obj case, else 1. Mirrors case.l 114."
  (cond ((or (null (car slotfr)) (null (caar slotfr))) 0)
        ((eq (caaar slotfr) '*obj) 2)
        (t 1)))

(defun maxunls (node)
  "Most object slots any of NODE's frame's hypotheses leaves unfilled.
   Mirrors case.l 69."
  (openchek node)
  (do ((sfs hypo-slots (cdr sfs)) (i 0))
      ((null sfs) i)
    (setq i (max i (open-obj-cases (car sfs))))))

(defun minunls (node)
  "Fewest object slots any of NODE's frame's hypotheses leaves
   unfilled. Mirrors case.l 76."
  (openchek node)
  (do ((sfs hypo-slots (cdr sfs)) (i 64))
      ((null sfs) i)
    (setq i (min i (open-obj-cases (car sfs))))))


;;; ===========================================================
;;; Open / close caching (case.l 484-508)
;;; ===========================================================

(defun openchek (node)
  "Make NODE's frame the open one (closing the previous open frame
   first if it differs). Mirrors case.l 484."
  (let ((cf (getr 'caseframe node)))
    (unless (equal openframe cf)
      (when openframe (closeframe openframe))
      (openframe cf))))

(defun closeframe (cf)
  "Flush the cached PRED / HYPO-SLOTS / OBJS-NEEDED back onto CF's
   plist. Mirrors case.l 490."
  (when cf
    (setf (get cf 'hypo-slots)  hypo-slots)
    (setf (get cf 'pred)        pred)
    (setf (get cf 'objs-needed) objs-needed)))

(defun openframe (cf)
  "Make CF the open frame, loading its cached slots into the specials;
   with CF nil, clear them. Mirrors case.l 496. (NB: `openframe' is
   also the special var holding the open frame.)"
  (setq openframe cf)
  (cond (cf (setq hypo-slots  (get cf 'hypo-slots)
                  pred        (get cf 'pred)
                  objs-needed (get cf 'objs-needed)))
        (t (clearcf))))

(defun clearcf ()
  "Close the open frame and reset the cached slots to empty. Mirrors
   case.l 503."
  (closeframe openframe)
  (setq hypo-slots (list (list nil nil)))
  (setq objs-needed 0)
  (setq pred nil))


;;; ===========================================================
;;; Case-register access (case.l 537-544)
;;; ===========================================================

(defun putc (reg value node)
  "Put VALUE under REG on NODE's frame. Mirrors case.l 537 verbatim,
   including its `caseframe'-guard / `case-frame'-write mismatch (see
   file header)."
  (and (get (car node) 'caseframe)
       (setf (get (get (car node) 'case-frame) reg) value)))

(defun getc (reg node)
  "Get REG from NODE's frame (a dummy symbol if NODE has none, so GET
   just returns NIL). Mirrors case.l 542."
  (get (or (get (car node) 'caseframe) '***total-dummy***) reg))


;;; ===========================================================
;;; Semantics trace hook (case.l 547-560)
;;; ===========================================================
;;;
;;; In the delivered source the "call to semantics" is reduced to an
;;; optional trace -- there is no external semantic component.

(defun nodep (foo)
  "Is FOO a node, i.e. (SYMBOL . FLAGS)? Mirrors case.l 552."
  (and (listp foo) (atom (car foo)) (numberp (cdr foo))))

(defun node-w-feats (list)
  "Render LIST for tracing, expanding any node into (head \\ features).
   Mirrors case.l 554."
  (do ((l list (cdr l)) (result))
      ((null l) (nreverse result))
    (push (cond ((nodep (car l)) (list (caar l) '\\ (fe (car l))))
                ((car l)))
          result)))

(defun semcall (&rest l)
  "Trace-only `call to semantics' (the real call is gone in the
   delivered source). Mirrors case.l 547."
  (unless (member (get openframe 'case-frame) '(nil dummy-mod))
    (when ctrace
      (apply #'say-it (cons '|To semantics:| (node-w-feats l))))))


;;; ===========================================================
;;; Deferred parse.l hook now unblocked (parse.l 586)
;;; ===========================================================

(defun alt-fillslot (a b c)
  "Record an ambiguous slot-fill on the current sentence node S, rather
   than committing it. Mirrors parse.l 586; needed `putc', hence it
   lands with case.l. (Companion of alt-attach in buffer-ops.)"
  (putc 'ambig-fillslot (list a b c) s))


;;; ===========================================================
;;; Surface-structure monitoring (case.l 574-598)
;;; ===========================================================
;;;
;;; `create' and `attachment' crules are the grammar's way to run a bit
;;; of code whenever a node of some type is created, or a node of some
;;; type is attached under a father of some type. They are registered by
;;; CRULE-INDEX and fired by CREATE-MONITOR (from NEWNODE) and
;;; ATTACH-MONITOR (from ATTACH).
;;;
;;; Storage: Marcus keeps creation crules on the variable :create-rules
;;; and attachment crules on the :attach-rules symbol's plist (keyed by
;;; father-node type). We use one reboundable list and one reboundable
;;; hash-table -- same shape, isolable in tests (cf. *rule-index*).
;;;
;;; NB: glang-cl does not yet compile the `{CREATE ...}' /
;;; `{ATTACHMENT CRULE ...}' rule forms into CRULE-INDEX calls; that is
;;; a separate glang-cl extension. The runtime monitor mechanism here
;;; works against crules registered directly (or by a future emission).

(defvar *create-rules* nil
  "Alist of creation crules: each entry (NODE-TYPE FN NAME).")
(defvar *attach-rules* (make-hash-table :test #'eq)
  "Father-node-type -> alist of attachment crules (ATTACH-TYPE FN NAME).")
(defvar fnode nil "Father node, bound by ATTACH-MONITOR for a crule body.")
(defvar snode nil "Attached node, bound by ATTACH-MONITOR for a crule body.")

(defun reset-crules ()
  "Forget all create/attach crules (between grammar reloads / tests)."
  (setq *create-rules* nil)
  (clrhash *attach-rules*))

(defun crule-index (type indexinfo)
  "Register a crule. For 'creation, INDEXINFO is (NODE-TYPE FN NAME).
   For 'attachment, INDEXINFO arrives as ((FATHER-TYPE . ATTACH-TYPE)
   FN NAME) and is rewritten in place to (ATTACH-TYPE FN NAME), filed
   under FATHER-TYPE. Mirrors case.l 574."
  (cond ((eq type 'attachment)
         (let* ((ftype  (caar indexinfo))
                (bucket (gethash ftype *attach-rules*)))
           (rplaca indexinfo (cdar indexinfo))
           (unless (member indexinfo bucket :test #'equal)
             (setf (gethash ftype *attach-rules*)
                   (cons indexinfo bucket)))))
        ((eq type 'creation)
         (push indexinfo *create-rules*))))

(defun create-monitor (type)
  "Run the creation crule for TYPE, if any. Called by NEWNODE. Mirrors
   case.l 585."
  (let ((crule (assoc type *create-rules* :test #'eq)))
    (when crule
      (when *crtrace* (say |Running create-rule| $ (caddr crule)))
      (funcall (cadr crule)))))

(defun attach-monitor (fn dn type)
  "When DN is attached under FN as TYPE, run the matching attachment
   crule (if FN's node-type has one for TYPE), with FNODE/SNODE bound to
   FN/DN. Called by ATTACH. Mirrors case.l 591."
  (let* ((attach-rules (gethash (getr 'type fn) *attach-rules*))
         (crule (and attach-rules (assoc type attach-rules :test #'eq))))
    (when crule
      (when *crtrace* (say |Running attach-rule| $ (caddr crule)))
      (setq fnode fn snode dn)
      (funcall (cadr crule))))
  t)
