;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/core/runtime/buffer-ops.lisp
;;;
;;; Buffer & packet-stack primitives from parse.l. Together with
;;; primitives.lisp these are enough for a compiled rule body to
;;; execute its full action sequence -- modify features, manipulate
;;; the active-packet set, attach/drop nodes, shuffle the look-ahead
;;; buffer.
;;;
;;; What this file covers:
;;;
;;;   Top-level bindings (parse.l 1-15)
;;;     *buffer*, *1stfvec*, *2ndfvec*, *3rdfvec*,
;;;     *index-to-fvec-alist* are allocated here.
;;;
;;;   Buffer maintenance (parse.l 247-333)
;;;     insert-index-pos, remove-index-pos, bufrestore
;;;
;;;   Packet activation (parse.l 647-656)
;;;     activate, deactivate
;;;
;;;   Attach / drop / insert-node (parse.l 563-602)
;;;
;;; Deferred to a later commit:
;;;   - `set*' and `setup*' (parse.l 253, 304) -- they depend on
;;;     `testrules' from the main parse loop.
;;;   - `buffer-gc' (parse.l 175) -- iterates the whole buffer +
;;;     packet stack; defer until we have the parse loop.
;;;   - Node creation: `makenode', `makesym', `newnode',
;;;     `node-reset', `nodegc' -- needs a name-counter convention
;;;     and the `:nodelist' bookkeeping; not load-bearing for the
;;;     attach/drop tests we want right now.
;;;   - Case-frame monitor hooks (`attach-monitor', `create-monitor')
;;;     -- they live in case.l, which we haven't ported; for now
;;;     we stub them as no-ops.

(in-package :parsifal)


;;; ===========================================================
;;; Top-level array bindings (parse.l lines 1-15)
;;; ===========================================================

(setf *buffer* (make-array 10 :initial-element nil))
(setf *1stfvec* (make-array 512 :element-type 'bit :initial-element 0))
(setf *2ndfvec* (make-array 512 :element-type 'bit :initial-element 0))
(setf *3rdfvec* (make-array 512 :element-type 'bit :initial-element 0))
(setf *index-to-fvec-alist*
      (list (cons 0 *1stfvec*)
            (cons 1 *2ndfvec*)
            (cons 2 *3rdfvec*)))


;;; ===========================================================
;;; Case-frame hooks
;;; ===========================================================
;;;
;;; `attach' (below) calls `attach-monitor' and `newnode' (node-ops)
;;; calls `create-monitor'. These are the real case-frame surface-
;;; structure monitors, now defined in case-frame.lisp (case.l 585-598);
;;; with no create/attach crules registered they are no-ops, so a
;;; grammar without case-frame rules behaves as before. They are
;;; forward-referenced here (case-frame.lisp loads last).

;; (Real RULE-INDEX lives in parse-loop.lisp now -- this file used to
;; carry a no-op stub during the bootstrap.)


;;; ===========================================================
;;; Buffer maintenance (parse.l 247-333)
;;; ===========================================================

(defun remove-index-pos (index)
  "Shift buffer contents down from INDEX, decrementing *bufmax*.
   Mirrors parse.l line 315."
  (do ((i index (1+ i)))
      ((= i *bufmax*)
       (setf (aref *buffer* *bufmax*) nil)
       (decf *bufmax*)
       t)
    (setf (aref *buffer* i) (aref *buffer* (1+ i)))))

(defun insert-index-pos (index node)
  "Shift buffer contents up from INDEX, insert NODE at INDEX, and
   bump *bufmax*. Mirrors parse.l line 323."
  (when (> *bufmax* 3)
    (warn "Inserting ~s makes ~d items in buffer" node (+ 2 *bufmax*)))
  (when (> (+ 2 *bufmax*) (length *buffer*))
    (error "Inserting ~s will overflow buffer array" node))
  (incf *bufmax*)
  (do ((i *bufmax* (1- i)))
      ((= i index)
       (setf (aref *buffer* index) node)
       t)
    (setf (aref *buffer* i) (aref *buffer* (1- i)))))

(defun bufrestore ()
  "Pop a saved *bufpntr* off *bufpntrstak* and restore it. Mirrors
   parse.l line 311."
  (if *bufpntrstak*
      (setq *bufpntr* (pop *bufpntrstak*))
      (warn "Attempting to pop empty buffer pointer stack.")))

(defun last* ()
  "The buffer node immediately before the current position, or NIL
   when *bufpntr* is at the buffer start. Marcus names this `:last'
   (parse.l 565); a keyword can't head a CL call form, so the grammar
   word `last' -- which Marcus's glang emits as `(:last)' -- emits
   `(last*)' here instead (see glang-cl denotations.lisp)."
  (and (not (zerop *bufpntr*))
       (aref *buffer* (1- *bufpntr*))))


;;; ===========================================================
;;; Packet activation (parse.l 647-656)
;;; ===========================================================

(defun deactivate (packets)
  "Remove PACKETS (a list of packet names) from the active set,
   preserving the order of the survivors -- CL's SET-DIFFERENCE
   doesn't, MacLISP's `setminus' does."
  (setq *activepackets*
        (remove-if (lambda (p) (member p packets :test #'eq))
                   *activepackets*)))

(defun activate (packets)
  "Add PACKETS to the active set. PACKETS may be a single symbol or
   a list. Mirrors parse.l line 650 -- including the empty-set
   shortcut and the single-symbol branch."
  (setq *activepackets*
        (cond ((null *activepackets*) packets)
              ((atom packets)
               (if (member packets *activepackets* :test #'eq)
                   *activepackets*
                   (cons packets *activepackets*)))
              (t (union packets *activepackets* :test #'eq)))))


;;; ===========================================================
;;; Attach / drop / insert-node (parse.l 563-602)
;;; ===========================================================

(defun attach1 (dn fn type)
  "Reversed-argument-order ATTACH so that DN can be evaluated before
   FN -- relevant when FN's evaluation creates a new node that
   should not shadow C. Mirrors parse.l line 563."
  (attach fn dn type))

(defun attach (fn dn type)
  "Attach DN as a TYPE-daughter of FN. Sets DN's father register to
   FN, adds DN to FN's daughters[type] list, and sets DN's
   attached-to-tree flag (bit 1).

   Marcus's `daughters' register holds a `(ncons nil)' -- a fresh
   cons cell used as a `disembodied property list' (MacLISP
   `putprop' walks the cdr as a plist). CL's GET only works on
   symbols, so we substitute a fresh uninterned symbol from GENSYM:
   same role (private namespace for type->daughters mappings),
   compatible with CL's plist accessors. The wider parser code uses
   `(get (getr 'daughters node) type)' to retrieve the list, which
   works either way."
  (let ((daughters (or (getr 'daughters fn)
                       (setr 'daughters (gensym "DAUGHTERS-") fn))))
    (setr 'father fn dn)
    (setf (get daughters type) (cons dn (get daughters type)))
    (setflags dn (logior 1 (flags dn)))
    (attach-monitor fn dn type)))

(defun alt-attach (dn fn type)
  "Record an ambiguous (deferred) attachment of DN to FN as TYPE on
   the current sentence node S, rather than committing it now. Mirrors
   parse.l line 583. The companion `alt-fillslot' is deferred -- it
   needs case.l's `putc'."
  (setr 'ambig-attach (list dn fn type) s))

(defun drop (index)
  "Drop the current node C back into the buffer (at *bufpntr*+INDEX),
   then pop the packet stack to restore the previous active node and
   its packets. Mirrors parse.l line 589.

   The IFNOT skips the buffer insertion if C is already in the tree
   (`getr 'father c' is non-nil) OR already in the buffer (linear
   scan from *bufmax* downward)."
  (unless (or (getr 'father c)
              (do ((i *bufmax* (1- i)))
                  ((minusp i) nil)
                (when (eq (aref *buffer* i) c) (return t))))
    (insert-index-pos (+ *bufpntr* index) c))
  (setq *activepackets* (cdar *activenodestak*))
  (setq c (car (pop *activenodestak*)))
  (clear-current-s))

(defun insert-node (node index)
  "Activate NODE, then drop it into buffer position
   *bufpntr*+INDEX. Mirrors parse.l line 599."
  (activatenode node)
  (drop index))


;;; ===========================================================
;;; Word input / buffer-position setup (parse.l 304-309, 506-514)
;;; ===========================================================

(defun nextword ()
  "Pop and return the next word-node from *wstring*. Returns NIL when
   input is exhausted, which the caller (set*) interprets as
   end-of-sentence rather than as an error.

   Marcus's nextword (parse.l 506) emits trace / timing chatter when
   *word-entry-switch* or *ptrace* is set; we skip that until those
   subsystems land."
  (pop *wstring*))

(defun setup* (node index)
  "Make NODE the current buffer-position-INDEX node. Stores INT-INDEX
   and NTH, then expands SETUP** for the matching position to clear
   and refill the feature vector and bind the corresponding buffer
   register (|1ST|/|2ND|/|3RD|). Mirrors parse.l line 304.

   SETUP** is a macro that references `node' lexically -- as long as
   the caller (i.e. this function) has NODE in scope, the expansion
   does the right thing."
  (setq *int-index* index
        nth         node)
  (cond ((= index 0) (setup** |1ST| *1stfeat* *1stfvec*))
        ((= index 1) (setup** |2ND| *2ndfeat* *2ndfvec*))
        ((= index 2) (setup** |3RD| *3rdfeat* *3rdfvec*))))
