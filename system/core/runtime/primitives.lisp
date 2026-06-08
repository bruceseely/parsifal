;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/core/runtime/primitives.lisp
;;;
;;; First batch of runtime primitives from Marcus's parse.l (lines
;;; ~80-500). Compiled rule bodies emitted by glang-cl call these
;;; functions; without them, rule emissions are inert.
;;;
;;; What this batch covers -- the "leaf" primitives, pure operations
;;; on a node's feature list with no buffer or packet-stack side
;;; effects:
;;;
;;;   fast-is, testindices, featindexify  -- feature-vector matching
;;;   addf1, remf1                        -- add/remove features
;;;   is, is-not-all-of, is-none-of,
;;;       is-any-of                       -- relational predicates
;;;   transfer, liftr                     -- copy features/registers
;;;
;;; Plus two MacLISP/Franz-Lisp idioms that recur throughout
;;; parse.l and need to exist before further porting:
;;;
;;;   for      -- a single-binding LET (Marcus's util/macros1, which
;;;               he hasn't supplied; behaviour deduced from usage)
;;;   consprop -- prepend onto a property-list value (from fixes.l)
;;;
;;; Deferred to a later commit:
;;;   - Node creation: makenode, makesym, newnode
;;;   - Buffer & packet mutation: attach, drop, insert-node,
;;;     activate, deactivate
;;;   - Tree search: find-node, father-node, node-above, binding, io
;;;   - Main parse loop, rule indexing.

(in-package :parsifal)


;;; ===========================================================
;;; MacLISP / Franz-Lisp idioms used by the primitive bodies
;;; ===========================================================

(defmacro for (bindings &body body)
  "Marcus's util/macros1 `for' (he hasn't supplied that file; we deduce
   the shape from every use site in parse.l and case.l). BINDINGS is a
   flat list of alternating var/init pairs:

     (for (v1 i1 v2 i2 ...) body...)
   =>
     (let* ((v1 i1) (v2 i2) ...) body...)

   The body is evaluated with the bindings in scope, sequential like
   LET*, returning the last form's value. Specials get a dynamic
   rebind, which callers rely on for forms like
   `(for (prinlength 5) ...)'."
  (do ((b bindings (cddr b))
       (pairs nil (cons (list (first b) (second b)) pairs)))
      ((null b)
       `(let* ,(nreverse pairs) ,@body))))

(defun consprop (atom value property)
  "From fixes.l: prepend VALUE onto the list stored under
   (GET ATOM PROPERTY)."
  (push value (get atom property))
  value)


;;; ===========================================================
;;; Feature-vector index machinery
;;; ===========================================================
;;;
;;; Each feature symbol gets a unique small-integer index stored
;;; under its :FINDEX property. *FINDEX-COUNTER* hands out the next
;;; available one. *INDEX-TO-FVEC-ALIST* maps a buffer position
;;; (0/1/2) to the bit-array for that position; that array is what
;;; FAST-IS consults at runtime.
;;;
;;; The slow path -- FEATINDEXIFY -- runs at compile time (it's
;;; called from FAST-IS's macro body), so by the time a rule body
;;; executes, all its feature symbols already have :FINDEX values.

(defun featindexify (feats)
  "Look up (and lazily assign) the :findex of each feature symbol in
   FEATS. Returns the list of indices in reverse order of the input,
   matching parse.l line 455 behaviour."
  (do ((fs feats (cdr fs))
       (findices nil))
      ((null fs) findices)
    (setq findices
          (cons (or (get (car fs) :findex)
                    (progn
                      (consprop :gram-stats (car fs) 'flist)
                      (setf (get (car fs) :findex)
                            (prog1 *findex-counter*
                              (incf *findex-counter*)))))
                findices))))

(defun testindices (index findices)
  "T iff every index in FINDICES has a 1 in the feature vector
   associated with buffer position INDEX (0=1st, 1=2nd, 2=3rd)."
  (let ((nfvec (cdr (assoc index *index-to-fvec-alist*))))
    (do ((fis findices (cdr fis)))
        ((null fis) t)
      (when (zerop (aref nfvec (car fis))) (return nil)))))

(defmacro fast-is (index feats)
  "Compile-time-expand FAST-IS into a TESTINDICES call with the
   feature symbols already resolved to their indices. Mirrors
   parse.l line 82's `defun fast-is macro' (MacLISP old-style
   defmacro). FEATS must be a literal list of feature symbols (or
   a quoted literal); FEATINDEXIFY runs at macro-expansion time."
  (let ((feat-list (if (and (consp feats) (eq (car feats) 'quote))
                       (cadr feats)
                       feats)))
    `(testindices ,index ',(featindexify feat-list))))


;;; ===========================================================
;;; Feature-list manipulation
;;; ===========================================================

(defun addf1 (node feats)
  (setfe node (append feats (fe node))))

(defun remf1 (feats node)
  "Argument order intentionally reversed from ADDF1: `Remove the
   features foo, bar of zorch' parses more naturally that way
   (parse.l line 471). We walk the node's feature list in order and
   drop the listed features, preserving the order of the survivors;
   CL's SET-DIFFERENCE doesn't guarantee that."
  (setfe node
         (remove-if (lambda (f) (member f feats :test #'eq))
                    (fe node))))


;;; ===========================================================
;;; Relational predicates
;;; ===========================================================
;;;
;;; These mirror the English-language pattern matchers:
;;;   [* IS NP]               -> (is node '(np))
;;;   [* IS NOT NP]           -> (is-not-all-of node '(np))
;;;   [* IS ANY OF NP, ADJ]   -> (is-any-of node '(np adj))
;;;   [* IS NONE OF ...]      -> (is-none-of node '(...))

(defun is (node feats)
  "Node must have ALL features."
  (let ((fset (fe node)))
    (do ((testfs feats (cdr testfs)))
        ((null testfs) t)
      (unless (member (car testfs) fset :test #'eq)
        (return nil)))))

(defun is-not-all-of (node feats)
  "Node must be lacking AT LEAST ONE feature."
  (let ((fset (fe node)))
    (do ((testfs feats (cdr testfs)))
        ((null testfs) nil)
      (unless (member (car testfs) fset :test #'eq)
        (return t)))))

(defun is-none-of (node feats)
  "Node must have NONE of the features."
  (let ((fset (fe node)))
    (do ((testfs feats (cdr testfs)))
        ((null testfs) t)
      (when (member (car testfs) fset :test #'eq)
        (return nil)))))

(defun is-any-of (node feats)
  "Node must have AT LEAST ONE feature."
  (let ((fset (fe node)))
    (do ((testfs feats (cdr testfs)))
        ((null testfs) nil)
      (when (member (car testfs) fset :test #'eq)
        (return t)))))


;;; ===========================================================
;;; Feature & register transfer between nodes
;;; ===========================================================

(defun transfer (features source dest)
  "Add to DEST the subset of FEATURES that SOURCE also has. We walk
   FEATURES in order to preserve that ordering on DEST; CL's
   INTERSECTION doesn't guarantee a particular order."
  (addf1 dest
         (let ((src-feats (fe source)))
           (remove-if-not (lambda (f) (member f src-feats :test #'eq))
                          features))))

(defun liftr (reg source dest)
  "Copy SOURCE's register REG into DEST's."
  (setr reg (getr reg source) dest))


;;; ===========================================================
;;; MacLISP arithmetic name aliases
;;; ===========================================================
;;;
;;; Marcus's grammar writes arithmetic in MacLISP names -- `plus' for
;;; addition, `times' for multiplication -- and glang-cl's compiler
;;; emits exactly those symbols. We provide one-line wrappers around
;;; the CL operators so the emitted code runs without further rewiring.

(defun plus  (&rest args) (apply #'+ args))
(defun times (&rest args) (apply #'* args))
