;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-

(in-package :parsifal)

;;; Tests for system/core/runtime/parse-loop.lisp.
;;;
;;; (parse-loop-test)   -> t if all pass, nil otherwise
;;; (parse-loop-test t) -> verbose


(defun parse-loop-test (&optional verbose)
  (let ((results t))
    (flet ((check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%"
                           expected actual)))
               (setf results (and pass results)))))


      ;; --- rule-index / testrules basics -----------------------------

      (let ((*rule-table* (make-hash-table :test #'eq))
            (*activepackets* '(pkt-a))
            (*activerule* nil))
        ;; Register one rule whose pattern always matches.
        (rule-index 'normal '(pkt-a) 'noindexf
                    (list 10
                          (lambda () t)
                          'rule-x
                          (lambda () 'fired-x)))
        (check "rule-index stored under packet"
               (and (gethash 'pkt-a *rule-table*) t)
               t)
        (check "testrules finds and sets *activerule*"
               (testrules 'normal 0)
               t)
        (check "testrules picked the registered rule"
               (first *activerule*)
               'rule-x))

      ;; testrules respects priority across packets
      (let* ((*rule-table* (make-hash-table :test #'eq))
             (*activepackets* '(pkt-a pkt-b))
             (*activerule* nil)
             (low-pri-fn  (lambda () 'low))
             (high-pri-fn (lambda () 'high)))
        (rule-index 'normal '(pkt-a) 'noindexf
                    (list 20 (lambda () t) 'low-rule  low-pri-fn))
        (rule-index 'normal '(pkt-b) 'noindexf
                    (list 5  (lambda () t) 'high-rule high-pri-fn))
        (testrules 'normal 0)
        (check "highest-priority rule (lowest number) wins across packets"
               (first *activerule*)
               'high-rule))

      ;; A pattern that returns nil is skipped
      (let* ((*rule-table* (make-hash-table :test #'eq))
             (*activepackets* '(pkt-a))
             (*activerule* nil))
        (rule-index 'normal '(pkt-a) 'noindexf
                    (list 10 (lambda () nil) 'never-fires (lambda () nil)))
        (rule-index 'normal '(pkt-a) 'noindexf
                    (list 20 (lambda () t)   'fires       (lambda () nil)))
        (testrules 'normal 0)
        (check "pattern returning nil is skipped"
               (first *activerule*)
               'fires))

      ;; rem-index removes a rule
      (let ((*rule-table* (make-hash-table :test #'eq))
            (*activepackets* '(pkt-a))
            (*activerule* nil))
        (rule-index 'normal '(pkt-a) 'noindexf
                    (list 10 (lambda () t) 'gone (lambda () nil)))
        (rem-index 'gone)
        (check "rem-index empties the bucket"
               (gethash 'pkt-a *rule-table*)
               nil)
        (check "after removal, testrules finds nothing"
               (testrules 'normal 0)
               nil))


      ;; --- parse-loop drives the cycle correctly ----------------------

      ;; One rule that sets *parsecomplete* -- loop fires once and ends.
      (let* ((act-fn-one (lambda () (setq *parsecomplete* t)))
             (*rule-table*    (make-hash-table :test #'eq))
             (*activepackets* '(pkt-a))
             (*activerule*    (list 'rule-one act-fn-one))
             (*nextrule*      nil)
             (*parsecomplete* nil)
             (*deriv*         nil)
             (|1ST| nil) (|2ND| nil) (|3RD| nil)
             (*bufpntr* 0)
             (*bufmax*  -1)
             (*buffer*  (make-array 10 :initial-element nil))
             (*wstring* nil)
             (*1stfeat* nil) (*2ndfeat* nil) (*3rdfeat* nil))
        (check "parse-loop returns T when *parsecomplete* is set"
               (parse-loop)
               t)
        (check "parse-loop pushed the fired rule onto *deriv*"
               *deriv*
               '(rule-one)))

      ;; A rule that sets *nextrule* -- loop fires it, then fires the
      ;; named follow-up rule, which sets *parsecomplete*.
      (let* ((*rule-table*    (make-hash-table :test #'eq))
             (*activepackets* nil)
             (*nextrule*      nil)
             (*parsecomplete* nil)
             (*deriv*         nil)
             (|1ST| nil) (|2ND| nil) (|3RD| nil)
             (*bufpntr* 0)
             (*bufmax*  -1)
             (*buffer*  (make-array 10 :initial-element nil))
             (*wstring* nil)
             (*1stfeat* nil) (*2ndfeat* nil) (*3rdfeat* nil))
        ;; Register the follow-up rule so act-of-rule can find its
        ;; action by name. Packets are empty here -- testrules
        ;; wouldn't pick this up; the chain runs solely via
        ;; *nextrule*.
        (rule-index 'normal '() 'noindexf
                    (list 0
                          (lambda () nil)
                          'follow-up
                          (lambda () (setq *parsecomplete* t))))
        (setq *activerule*
              (list 'starter
                    (lambda () (setq *nextrule* 'follow-up))))
        (check "parse-loop chains via *nextrule*"
               (parse-loop)
               t)
        (check "*deriv* records the chain in firing order (newest first)"
               *deriv*
               '(follow-up starter)))

      ;; No matching rule -> deadlock -> NIL
      (let ((*rule-table*    (make-hash-table :test #'eq))
            (*activepackets* nil)
            (*nextrule*      nil)
            (*parsecomplete* nil)
            (*deriv*         nil)
            (|1ST| nil) (|2ND| nil) (|3RD| nil)
            (*bufpntr* 0)
            (*bufmax*  -1)
            (*buffer*  (make-array 10 :initial-element nil))
            (*wstring* nil)
            (*1stfeat* nil) (*2ndfeat* nil) (*3rdfeat* nil))
        ;; Set *activerule* to a rule that falls through.
        (setq *activerule*
              (list 'fall-through (lambda () nil)))
        (check "parse-loop returns NIL on deadlock"
               (handler-bind ((warning #'muffle-warning))
                 (parse-loop))
               nil))


      ;; --- buffer-gc (parse.l 175-185) -------------------------------

      ;; All-attached run from *bufpntr* is removed entirely.
      (let* ((*buffer*  (make-array 10 :initial-element nil))
             (*bufpntr* 0)
             (*bufmax*  2)
             (*nr-types* nil))
        (setf (aref *buffer* 0) (cons (gensym) 1)  ; flags bit1 = attached
              (aref *buffer* 1) (cons (gensym) 1)
              (aref *buffer* 2) (cons (gensym) 1))
        (buffer-gc)
        (check "buffer-gc removes a run of attached nodes"
               *bufmax*
               -1))

      ;; An unattached node is left in place.
      (let* ((node      (cons (gensym) 0))      ; flags bit1 clear
             (*buffer*  (make-array 10 :initial-element nil))
             (*bufpntr* 0)
             (*bufmax*  0)
             (*nr-types* nil))
        (setf (aref *buffer* 0) node)
        (buffer-gc)
        (check "buffer-gc leaves an unattached node"
               (list *bufmax* (aref *buffer* 0))
               (list 0 node)))

      ;; Stops on an attached, unchecked NR-type node (keeps its slot);
      ;; the attached non-NR node ahead of it is still collected.
      (let* ((nr-head   (gensym "NR"))
             (nr-node   (cons nr-head 1))       ; attached, bit2 clear
             (plain     (cons (gensym) 1))      ; attached, non-NR
             (*buffer*  (make-array 10 :initial-element nil))
             (*bufpntr* 0)
             (*bufmax*  1)
             (*nr-types* (list 'foo-nr)))
        (setf (get nr-head 'type) 'foo-nr)
        (setf (aref *buffer* 0) plain
              (aref *buffer* 1) nr-node)
        (buffer-gc)
        (check "buffer-gc stops at an attached unchecked NR-type node"
               (list *bufmax* (aref *buffer* 0))
               (list 0 nr-node)))


      ;; --- last* (parse.l 565) ---------------------------------------

      (let ((*buffer*  (make-array 5 :initial-element nil))
            (*bufpntr* 0))
        (check "last* is NIL at the buffer start"
               (last*)
               nil))

      (let* ((prev      (cons (gensym) 0))
             (*buffer*  (make-array 5 :initial-element nil))
             (*bufpntr* 1))
        (setf (aref *buffer* 0) prev)
        (check "last* returns the node before *bufpntr*"
               (last*)
               prev))


      ;; --- alt-attach (parse.l 583) ----------------------------------

      (let* ((s-head (gensym "S"))
             (s      (cons s-head 0))
             (dn     (cons (gensym) 0))
             (fn     (cons (gensym) 0)))
        (setf (symbol-plist s-head) nil)
        (alt-attach dn fn 'np)
        (check "alt-attach records (dn fn type) on s under ambig-attach"
               (getr 'ambig-attach s)
               (list dn fn 'np))))

    results))
