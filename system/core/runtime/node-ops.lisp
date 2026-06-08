;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/core/runtime/node-ops.lisp
;;;
;;; Node-creation and tree-search primitives from parse.l. With this
;;; file in place, compiled rule bodies can do everything except run
;;; under the wait-and-see main loop: create new nodes, traverse the
;;; tree, look up daughters, find current-s.
;;;
;;; What this file covers:
;;;
;;;   Node creation (parse.l 516-613)
;;;     newnode, makenode, makesym
;;;
;;;   Tree search (parse.l 618-644)
;;;     father-node, node-above, daughters, daughter,
;;;     find-node, find-node1, binding, io
;;;
;;;   Current-S / wh-comp (parse.l 355, 525-532)
;;;     setup-current-s, current-s, wh-comp, s-type
;;;
;;;   Node-identity helper (parse.l 384)
;;;     nid1
;;;
;;; Deferred to a later commit:
;;;   - `head', `word', `root-of': depend on phrase-structure logic
;;;     and morphology; not on the path for first end-to-end parses.
;;;   - `nid', `node-id': need `phrasify' from util.l (tree printing)
;;;     and Marcus's print2 helper.
;;;   - `node-reset', `nodegc': cleanup; not critical for runtime
;;;     correctness, and `node-reset' relies on a `gennum'-counted
;;;     prefix table we extend lazily in `makesym' instead.

(in-package :parsifal)


;;; ===========================================================
;;; Node creation (parse.l 516-613)
;;; ===========================================================

(defun makesym (prefix)
  "Generate a fresh symbol with name PREFIX<N>, where N is a per-prefix
   counter stored under PREFIX's `gennum' property. Mirrors parse.l
   line 609. The new symbol is interned in :parsifal so that node
   identity survives reload."
  (let* ((gennum (or (get prefix 'gennum)
                     (progn (consprop 'makesym prefix 'prefixes)
                            0)))
         (next   (1+ gennum)))
    (setf (get prefix 'gennum) next)
    (intern (format nil "~A~D" prefix next) :parsifal)))

(defun makenode (type)
  "Build a fresh node (cons of head-symbol and zero flags). Mirrors
   parse.l line 604."
  (let ((nname (makesym type)))
    (setf (symbol-value nname) nil)
    (cons nname 0)))

(defun newnode (type flist)
  "Create a node with the given TYPE and feature list, activate it
   (becomes the new C, pushing the old onto *activenodestak*), and
   fire the case-frame create-monitor. Mirrors parse.l line 516."
  (let ((node (makenode type)))
    (setr 'type type node)
    (push node *nodelist*)
    (setfe node (cons type flist))
    (activatenode node)
    (create-monitor type)
    node))


;;; ===========================================================
;;; Tree search (parse.l 618-644)
;;; ===========================================================

(defun daughters (type node)
  "All daughters of NODE under TYPE. NIL if there are none."
  (get (getr 'daughters node) type))

(defun daughter (type node)
  "The unique daughter of NODE under TYPE. Signals an error if there
   are multiple. Mirrors parse.l line 392; Marcus returns the symbol
   `error' there, but in CL we ERROR for fail-fast."
  (let ((d (daughters type node)))
    (cond ((null (cdr d)) (car d))
          (t (error "multiple daughters of type ~a under ~a"
                    type node)))))

(defun father-node (node)
  "Find NODE's parent. Tries the `father' register first; if that's
   nil, walks *activenodestak* looking for the entry whose head is
   NODE and returns the head of the entry above it. Mirrors parse.l
   line 623."
  (or (getr 'father node)
      (do ((nodes (cons (list c) *activenodestak*) (cdr nodes)))
          ((null nodes) nil)
        (when (eq node (caar nodes))
          (return (caadr nodes))))))

(defun node-above (type node)
  "Walk up from NODE, returning the first ancestor whose feature list
   contains TYPE. Mirrors parse.l line 618."
  (cond ((null node) nil)
        ((member type (fe node) :test #'eq) node)
        (t (node-above type (father-node node)))))

(defun binding (node)
  "Follow a chain of trace/* nodes to the bound element. NIL passes
   through; a node lacking both `trace' and `*' returns itself.
   Mirrors parse.l line 633."
  (cond ((null node) nil)
        ((is-any-of node '(trace *)) (binding (getr 'binding node)))
        (t node)))

(defun find-node (dest start)
  "First daughter of START under DEST. Mirrors parse.l line 639. The
   carefulsw branch from Marcus's source -- a `Cannot find the X
   of Y' warning -- is preserved but only fires when *carefulsw* is
   non-nil."
  (cond ((car (get (getr 'daughters start) dest)))
        ((and *carefulsw*
              (warn "Cannot find the ~a of ~a" dest start)))))

(defun find-node1 (dest start)
  "FIND-NODE composed with BINDING -- follows any trace/* binding so
   callers see the real referent. Mirrors parse.l line 644."
  (binding (find-node dest start)))

(defun io (node)
  "The indirect object of NODE: second daughter under `np'. Mirrors
   parse.l line 630."
  (cadr (get (getr 'daughters node) 'np)))


;;; ===========================================================
;;; Current-S / wh-comp (parse.l 355, 525-532)
;;; ===========================================================

(defun setup-current-s ()
  "Lazily populate *current-s* and *wh-comp* by walking up from C
   looking for an S-typed ancestor. Mirrors parse.l line 525."
  (unless *current-s*
    (setq *current-s* (node-above 's c))
    (setq *wh-comp*   (getr :wh-comp *current-s*))))

(defun current-s ()
  (setup-current-s)
  *current-s*)

(defun wh-comp ()
  (setup-current-s)
  *wh-comp*)

(defun s-type (node)
  "Intersection of NODE's feature list with *sentence-types*; tells
   you what kind of sentence (decl, ynquest, ...) a node represents.
   Mirrors parse.l line 355."
  (intersection (fe node) *sentence-types* :test #'eq))


;;; ===========================================================
;;; Node-identity helper (parse.l 384)
;;; ===========================================================

(defun nid1 (node)
  "Two-element identity for a node: (head-symbol feature-list).
   Mirrors parse.l line 384. The fuller `nid' (which adds `:' and
   the phrasification) needs the tree printer from util.l, which is
   not yet ported."
  (list (car node) (fe node)))
