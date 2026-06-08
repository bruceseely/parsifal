;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-

(in-package :parsifal)

;;; Tests for system/core/runtime/declr.lisp.
;;;
;;; (declr-test)   -> t if all pass, nil otherwise
;;; (declr-test t) -> same, but prints every case as it runs


(defun declr-test (&optional verbose)
  (let ((results t))
    (flet ((check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%"
                           expected actual)))
               (setf results (and pass results)))))

      ;; --- specials are bound and SETQ-able ----------------------------

      (let ((*current-s* :outer-s))
        (check "*current-s* is a special (dynamic rebind works)"
               *current-s*
               :outer-s))

      (let ((c :outer-c))
        (check "c is a special"
               c
               :outer-c))

      (let ((|1ST| :buffer-head))
        (check "|1ST| is a special"
               |1ST|
               :buffer-head))


      ;; --- flags / setflags --------------------------------------------

      (let ((node (cons (gensym "N") 0)))
        (setflags node 42)
        (check "setflags / flags round-trip"
               (flags node)
               42))


      ;; --- fe / setfe (feature list as symbol-value of node head) ------

      (let* ((head (gensym "N"))
             (node (cons head 0)))
        (setf (symbol-value head) nil)
        (setfe node '(np finite))
        (check "setfe stores feature list under (car node)"
               (fe node)
               '(np finite)))


      ;; --- setr / getr (registers on the head symbol's plist) ----------

      (let* ((head (gensym "N"))
             (node (cons head 0)))
        (setf (symbol-plist head) nil)
        (setr 'subject 'foo-np node)
        (check "setr / getr round-trip"
               (getr 'subject node)
               'foo-np)
        (setr 'subject 'bar-np node)
        (check "setr overwrites prior value"
               (getr 'subject node)
               'bar-np))


      ;; --- clear-current-s ---------------------------------------------

      (let ((*current-s* (cons 'fake-s 0))
            (*wh-comp*   'fake-wh))
        (clear-current-s)
        (check "clear-current-s nils *current-s*"
               *current-s*
               nil)
        (check "clear-current-s nils *wh-comp*"
               *wh-comp*
               nil))

      (let ((*current-s* nil)
            (*wh-comp*   'something))
        (clear-current-s)
        (check "clear-current-s no-op when *current-s* already nil"
               *wh-comp*
               'something))


      ;; --- activatenode ------------------------------------------------

      (let* ((old-node (cons (gensym "OLD") 0))
             (new-node (cons (gensym "NEW") 0))
             (c               old-node)
             (*activepackets* '(pkt-a pkt-b))
             (*activenodestak* nil)
             (*current-s* nil)
             (*wh-comp*   nil))
        (activatenode new-node)
        (check "activatenode swaps in the new c"
               c
               new-node)
        (check "activatenode clears active packets"
               *activepackets*
               nil)
        (check "activatenode pushes (old-c . old-packets) onto stack"
               *activenodestak*
               (list (cons old-node '(pkt-a pkt-b))))))

    results))
