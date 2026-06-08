;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-

(in-package :parsifal)

;;; Tests for system/core/runtime/node-ops.lisp.
;;;
;;; (node-ops-test)   -> t if all pass, nil otherwise
;;; (node-ops-test t) -> verbose


(defun node-ops-test (&optional verbose)
  (let ((results t))
    (flet ((check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%"
                           expected actual)))
               (setf results (and pass results)))))


      ;; --- makesym ----------------------------------------------------

      (let ((np-sym (gensym "TESTNP-")))
        ;; Use a fresh symbol so we don't collide with prior runs.
        (setf (get np-sym 'gennum) nil)
        (let* ((s1 (makesym np-sym))
               (s2 (makesym np-sym))
               (s3 (makesym np-sym)))
          (check "makesym returns distinct symbols"
                 (and (not (eq s1 s2)) (not (eq s2 s3)) (not (eq s1 s3)))
                 t)
          (check "makesym increments gennum"
                 (get np-sym 'gennum)
                 3)
          (check "makesym name uses PREFIX<N> pattern"
                 (mapcar #'symbol-name (list s1 s2 s3))
                 (mapcar (lambda (n)
                           (format nil "~A~D" np-sym n))
                         '(1 2 3)))))


      ;; --- makenode ---------------------------------------------------

      (let* ((type   (gensym "TT-"))
             (node   (makenode type)))
        (check "makenode head's symbol-value is nil"
               (symbol-value (car node))
               nil)
        (check "makenode flags are 0"
               (cdr node)
               0))


      ;; --- newnode ----------------------------------------------------

      (let ((tt-type (gensym "TT-")))
        ;; Stub *activenodestak* so activatenode has something to push
        ;; onto, and clear c.
        (let ((*activenodestak* nil)
              (*activepackets*  nil)
              (*current-s*      nil)
              (*wh-comp*        nil)
              (*nodelist*       nil)
              (c                nil))
          (let ((node (newnode tt-type '(quant det))))
            (check "newnode sets feature list (type . flist)"
                   (fe node)
                   (cons tt-type '(quant det)))
            (check "newnode stores 'type register"
                   (getr 'type node)
                   tt-type)
            (check "newnode pushed onto *nodelist*"
                   (first *nodelist*)
                   node)
            (check "newnode activated the node (c switched)"
                   c
                   node))))


      ;; --- daughters / daughter --------------------------------------

      (let* ((fn-head (gensym "FN-"))
             (dn1-head (gensym "DN1-"))
             (dn2-head (gensym "DN2-"))
             (fn  (cons fn-head 0))
             (dn1 (cons dn1-head 0))
             (dn2 (cons dn2-head 0)))
        (setf (symbol-plist fn-head) nil)
        (attach fn dn1 'np)
        (attach fn dn2 'pp)
        (check "daughters returns the list under TYPE"
               (daughters 'np fn)
               (list dn1))
        (check "daughters of absent type returns nil"
               (daughters 'comp fn)
               nil)
        (check "daughter returns the unique value"
               (daughter 'np fn)
               dn1)
        ;; Add a second np daughter; daughter should now error.
        (let ((dn3 (cons (gensym "DN3-") 0)))
          (attach fn dn3 'np)
          (check "daughter signals on multiple matches"
                 (handler-case (progn (daughter 'np fn) :no-error)
                   (error () :errored))
                 :errored)))


      ;; --- find-node / find-node1 / binding --------------------------

      (let* ((fn-head (gensym "FN-"))
             (dn-head (gensym "DN-"))
             (fn  (cons fn-head 0))
             (dn  (cons dn-head 0)))
        (setf (symbol-plist fn-head) nil
              (symbol-value dn-head) nil)
        (attach fn dn 'subj)
        (check "find-node returns first daughter under TYPE"
               (find-node 'subj fn)
               dn)
        (check "find-node of missing TYPE returns nil"
               (find-node 'obj fn)
               nil)

        ;; binding: pass-through for a non-trace node.
        (check "binding of plain node returns itself"
               (binding dn)
               dn)

        ;; binding: follow trace -> referent.
        (let* ((target-head (gensym "TGT-"))
               (target (cons target-head 0)))
          (setf (symbol-value target-head) nil
                (symbol-plist target-head) nil
                (symbol-value dn-head)     '(trace))
          (setr 'binding target dn)
          (check "binding of trace follows 'binding register"
                 (binding dn)
                 target)
          (check "find-node1 = find-node + binding"
                 (find-node1 'subj fn)
                 target)))


      ;; --- io --------------------------------------------------------
      ;; io is `cadr of np daughters'.

      (let* ((fn-head (gensym "FN-"))
             (subj-head (gensym "SUBJ-"))
             (iobj-head (gensym "IOBJ-"))
             (fn   (cons fn-head 0))
             (subj (cons subj-head 0))
             (iobj (cons iobj-head 0)))
        (setf (symbol-plist fn-head) nil)
        ;; attach prepends to the daughter list, so we want subj first
        ;; in the eventual list, iobj second -- attach iobj first,
        ;; then subj.
        (attach fn iobj 'np)
        (attach fn subj 'np)
        (check "io returns second np daughter (cadr)"
               (io fn)
               iobj))


      ;; --- father-node / node-above ----------------------------------

      (let* ((s-head (gensym "S-"))
             (vp-head (gensym "VP-"))
             (np-head (gensym "NP-"))
             (s-node  (cons s-head 0))
             (vp-node (cons vp-head 0))
             (np-node (cons np-head 0)))
        (setf (symbol-value s-head)  '(s)
              (symbol-value vp-head) '(vp)
              (symbol-value np-head) '(np)
              (symbol-plist s-head)  nil
              (symbol-plist vp-head) nil
              (symbol-plist np-head) nil)
        (attach s-node vp-node 'vp)
        (attach vp-node np-node 'np)

        (check "father-node via 'father register"
               (father-node np-node)
               vp-node)
        (check "father-node of root returns nil (no father, not on stack)"
               (let ((*activenodestak* nil) (c nil)) (father-node s-node))
               nil)
        (check "node-above with matching node returns it"
               (node-above 'np np-node)
               np-node)
        (check "node-above walks up to find 's"
               (let ((*activenodestak* nil) (c nil)) (node-above 's np-node))
               s-node)
        (check "node-above of absent type returns nil"
               (let ((*activenodestak* nil) (c nil)) (node-above 'comp np-node))
               nil))


      ;; --- current-s / wh-comp ---------------------------------------

      (let* ((s-head  (gensym "S-"))
             (vp-head (gensym "VP-"))
             (s-node  (cons s-head 0))
             (vp-node (cons vp-head 0)))
        (setf (symbol-value s-head)  '(s)
              (symbol-value vp-head) '(vp)
              (symbol-plist s-head)  nil
              (symbol-plist vp-head) nil)
        (attach s-node vp-node 'vp)
        (setr :wh-comp 'who-comp s-node)
        (let ((*current-s* nil)
              (*wh-comp*   nil)
              (*activenodestak* nil)
              (c vp-node))
          (check "current-s lazily finds the S ancestor"
                 (current-s)
                 s-node)
          (check "wh-comp returns the 'wh-comp register of *current-s*"
                 (wh-comp)
                 'who-comp)))


      ;; --- s-type ----------------------------------------------------

      (let* ((s-head (gensym "S-"))
             (n (cons s-head 0))
             (*sentence-types* '(decl ynquest imper whquest inf-s)))
        (setf (symbol-value s-head) '(s decl finite))
        (check "s-type: intersection of feats and *sentence-types*"
               (s-type n)
               '(decl)))


      ;; --- nid1 ------------------------------------------------------

      (let* ((head (gensym "N-"))
             (n (cons head 0)))
        (setf (symbol-value head) '(np quant))
        (check "nid1: (head fe)"
               (nid1 n)
               (list head '(np quant)))))

    results))
