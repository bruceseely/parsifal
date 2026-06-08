;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-

(in-package :parsifal)

;;; Tests for system/core/runtime/buffer-ops.lisp.
;;;
;;; (buffer-ops-test)   -> t if all pass, nil otherwise
;;; (buffer-ops-test t) -> verbose: prints every case


(defun buffer-ops-test (&optional verbose)
  (let ((results t))
    (flet ((check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%"
                           expected actual)))
               (setf results (and pass results)))))


      ;; --- activate / deactivate --------------------------------------

      (let ((*activepackets* nil))
        (activate '(p1 p2))
        (check "activate into empty set"
               *activepackets*
               '(p1 p2)))

      (let ((*activepackets* '(p3)))
        (activate '(p1 p2))
        (check "activate (list) unions with existing"
               (sort (copy-list *activepackets*) #'string<)
               '(p1 p2 p3)))

      (let ((*activepackets* '(p1 p2)))
        (activate 'p3)
        (check "activate (atom) conses on"
               *activepackets*
               '(p3 p1 p2)))

      (let ((*activepackets* '(p1 p2)))
        (activate 'p1)
        (check "activate (atom) already present is a no-op"
               *activepackets*
               '(p1 p2)))

      (let ((*activepackets* '(p1 p2 p3)))
        (deactivate '(p2))
        (check "deactivate removes one"
               *activepackets*
               '(p1 p3)))

      (let ((*activepackets* '(p1 p2)))
        (deactivate '(p3))
        (check "deactivate of absent packet is no-op"
               *activepackets*
               '(p1 p2)))


      ;; --- buffer index manipulation ----------------------------------

      ;; Use the real *buffer*; reset it for each test block.
      (flet ((reset-buf ()
               (loop for i from 0 below (length *buffer*)
                     do (setf (aref *buffer* i) nil))
               (setq *bufmax* -1)
               (setq *bufpntr* 0)))

        (reset-buf)
        ;; Set up a buffer [A B C] (bufmax=2).
        (setf (aref *buffer* 0) 'a)
        (setf (aref *buffer* 1) 'b)
        (setf (aref *buffer* 2) 'c)
        (setq *bufmax* 2)
        (insert-index-pos 1 'x)
        (check "insert-index-pos: insert at 1 of [A B C] -> [A X B C]"
               (list (aref *buffer* 0) (aref *buffer* 1)
                     (aref *buffer* 2) (aref *buffer* 3))
               '(a x b c))
        (check "insert-index-pos: bumps *bufmax*"
               *bufmax*
               3)

        (reset-buf)
        (setf (aref *buffer* 0) 'a)
        (setf (aref *buffer* 1) 'b)
        (setf (aref *buffer* 2) 'c)
        (setq *bufmax* 2)
        (remove-index-pos 1)
        (check "remove-index-pos: drop index 1 of [A B C] -> [A C nil]"
               (list (aref *buffer* 0) (aref *buffer* 1) (aref *buffer* 2))
               '(a c nil))
        (check "remove-index-pos: decrements *bufmax*"
               *bufmax*
               1))


      ;; --- bufrestore -------------------------------------------------

      (let ((*bufpntr*       42)
            (*bufpntrstak*   '(7 4 0)))
        (bufrestore)
        (check "bufrestore: pops top of stack into *bufpntr*"
               *bufpntr*
               7)
        (check "bufrestore: stack tail remains"
               *bufpntrstak*
               '(4 0)))


      ;; --- attach -----------------------------------------------------

      (let* ((fn-head (gensym "FN"))
             (dn-head (gensym "DN"))
             (fn      (cons fn-head 0))
             (dn      (cons dn-head 0)))
        (setf (symbol-plist fn-head) nil)
        (setf (symbol-plist dn-head) nil)
        (attach fn dn 'np)
        (check "attach: dn's father is set to fn"
               (getr 'father dn)
               fn)
        (check "attach: fn's daughters[np] contains dn"
               (let ((d (getr 'daughters fn)))
                 (get d 'np))
               (list dn))
        (check "attach: dn's attached-flag (bit 1) is set"
               (logand 1 (flags dn))
               1)

        ;; A second attach of a different node into the same slot
        ;; should prepend to the existing list.
        (let* ((dn2-head (gensym "DN2"))
               (dn2      (cons dn2-head 0)))
          (setf (symbol-plist dn2-head) nil)
          (attach fn dn2 'np)
          (check "attach: second daughter is consed onto the list"
                 (let ((d (getr 'daughters fn)))
                   (get d 'np))
                 (list dn2 dn))))

      ;; attach1 swaps argument order
      (let* ((fn-head (gensym "FN"))
             (dn-head (gensym "DN"))
             (fn      (cons fn-head 0))
             (dn      (cons dn-head 0)))
        (setf (symbol-plist fn-head) nil)
        (setf (symbol-plist dn-head) nil)
        (attach1 dn fn 'pp)
        (check "attach1: same result with reversed first two args"
               (getr 'father dn)
               fn))


      ;; --- drop -------------------------------------------------------
      ;;
      ;; drop's contract: if c isn't already in the tree (no father) and
      ;; not already in the buffer, insert c into buffer at (+ *bufpntr*
      ;; index); then pop *activenodestak* to restore the prior c +
      ;; packets.

      (let* ((old-c-head (gensym "OLDC"))
             (new-c-head (gensym "NEWC"))
             (old-c (cons old-c-head 0))
             (new-c (cons new-c-head 0))
             (*buffer*         (make-array 10 :initial-element nil))
             (*bufmax*         -1)
             (*bufpntr*        0)
             (c                new-c)
             (*activepackets*  '(active-pkt))
             (*activenodestak* (list (cons old-c '(old-pkt))))
             (*current-s*      nil)
             (*wh-comp*        nil))
        (setf (symbol-plist new-c-head) nil)
        (setf (symbol-plist old-c-head) nil)
        (drop 0)
        (check "drop: new-c lands at *bufpntr*"
               (aref *buffer* 0)
               new-c)
        (check "drop: *bufmax* bumped"
               *bufmax*
               0)
        (check "drop: c restored from stack head"
               c
               old-c)
        (check "drop: *activepackets* restored from stack head"
               *activepackets*
               '(old-pkt))
        (check "drop: *activenodestak* popped"
               *activenodestak*
               nil))

      ;; If c already has a father (it's in the tree), drop should
      ;; NOT insert it into the buffer.
      (let* ((old-c-head (gensym "OLDC2"))
             (new-c-head (gensym "NEWC2"))
             (parent     (cons (gensym "P") 0))
             (old-c      (cons old-c-head 0))
             (new-c      (cons new-c-head 0))
             (*buffer*         (make-array 10 :initial-element nil))
             (*bufmax*         -1)
             (*bufpntr*        0)
             (c                new-c)
             (*activepackets*  '(p))
             (*activenodestak* (list (cons old-c '(q)))))
        (setf (symbol-plist new-c-head) nil)
        (setr 'father parent new-c)
        (drop 0)
        (check "drop: c-with-father is NOT inserted into buffer"
               *bufmax*
               -1))


      ;; --- insert-node -----------------------------------------------

      (let* ((old-c-head (gensym "OLDC3"))
             (new-c-head (gensym "NEWC3"))
             (old-c      (cons old-c-head 0))
             (new-c      (cons new-c-head 0))
             (*buffer*         (make-array 10 :initial-element nil))
             (*bufmax*         -1)
             (*bufpntr*        0)
             (c                old-c)
             (*activepackets*  '(pkt-old))
             (*activenodestak* nil)
             (*current-s*      nil)
             (*wh-comp*        nil))
        (setf (symbol-plist new-c-head) nil)
        (setf (symbol-plist old-c-head) nil)
        ;; activatenode pushes old-c, switches c to new-c.
        ;; drop then puts new-c in buffer and pops back to old-c.
        (insert-node new-c 0)
        (check "insert-node: new-c ends up in buffer"
               (aref *buffer* 0)
               new-c)
        (check "insert-node: c returns to old-c after the round trip"
               c
               old-c)))

    results))
