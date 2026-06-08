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
;;; Stubs for case-frame hooks (case.l -- not yet ported)
;;; ===========================================================
;;;
;;; Marcus's `attach' calls `attach-monitor' and `newnode' calls
;;; `create-monitor'. Both look up rules under :attach-rules /
;;; :create-rules; when no case-frame rules are defined (which is
;;; how a grammar without case-frame rules behaves), they are
;;; effectively no-ops. We provide that no-op shape here so attach
;;; can call them without an unbound-function error.

(defun create-monitor (type)
  (declare (ignore type))
  nil)

(defun attach-monitor (fn dn type)
  (declare (ignore fn dn type))
  nil)


;;; ===========================================================
;;; Rule-indexing stub (parse.l 400 -- not yet ported)
;;; ===========================================================
;;;
;;; The compiler emits one (RULE-INDEX 'kind '(packets) 'indexf
;;; '(priority pat-fn rule-name act-fn)) call per rule, intended to
;;; register the rule under the active-packet indexing structures
;;; the wait-and-see main loop consults. Until we port that loop,
;;; we accept the call and discard it -- this is what lets a
;;; freshly-compiled rule body eval as a top-level PROGN without
;;; an undefined-function error.

(defun rule-index (kind packets indexf rule-record)
  (declare (ignore kind packets indexf rule-record))
  nil)


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
