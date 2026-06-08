;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-

(in-package :parsifal)

;;; Tests for system/core/runtime/primitives.lisp.
;;;
;;; (primitives-test)   -> t if all pass, nil otherwise
;;; (primitives-test t) -> verbose: prints every case


(defun make-test-node (&optional features)
  "Build a fresh node (head-gensym . flags) with FEATURES as its
   feature list and a clean property list."
  (let* ((head (gensym "NODE"))
         (node (cons head 0)))
    (setf (symbol-value head) features)
    (setf (symbol-plist head) nil)
    node))


;;; Pre-register two feature symbols so the FAST-IS macro can
;;; expand against them at file-compile time. The wrapper functions
;;; below capture that expansion; the test body then sets up an
;;; fvec and calls them.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (featindexify '(test-fast-np test-fast-verb)))

(defun %fast-is-np   () (fast-is 0 (test-fast-np)))
(defun %fast-is-verb () (fast-is 0 (test-fast-verb)))
(defun %fast-is-both () (fast-is 0 (test-fast-np test-fast-verb)))


(defun primitives-test (&optional verbose)
  (let ((results t))
    (flet ((check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%"
                           expected actual)))
               (setf results (and pass results)))))


      ;; --- for ---------------------------------------------------------

      (check "for binds and returns last form"
             (for (x 10) (* x x))
             100)

      (let ((*print-length* nil))
        (for (*print-length* 3)
             (check "for establishes dynamic binding on a special"
                    *print-length*
                    3))
        (check "for-binding does not escape"
               *print-length*
               nil))


      ;; --- consprop ---------------------------------------------------

      (let ((sym (gensym "S")))
        (setf (symbol-plist sym) nil)
        (consprop sym 'a 'frob)
        (consprop sym 'b 'frob)
        (check "consprop prepends in call order"
               (get sym 'frob)
               '(b a)))


      ;; --- featindexify --------------------------------------------

      (let ((*findex-counter* 0))
        ;; Use fresh symbols so the test is independent of previous runs.
        (let ((f1 (gensym "F"))
              (f2 (gensym "F"))
              (f3 (gensym "F")))
          (let ((idx (featindexify (list f1 f2 f3))))
            (check "featindexify returns indices in reverse input order"
                   idx
                   '(2 1 0)))
          (check "featindexify assigned :findex on each symbol"
                 (list (get f1 :findex) (get f2 :findex) (get f3 :findex))
                 '(0 1 2))
          (check "featindexify advanced *findex-counter*"
                 *findex-counter*
                 3)
          ;; Second call should reuse the existing :findex values.
          (let ((idx2 (featindexify (list f1 f2 f3))))
            (check "featindexify is idempotent for known symbols"
                   idx2
                   '(2 1 0)))
          (check "*findex-counter* unchanged on repeat"
                 *findex-counter*
                 3)))


      ;; --- testindices --------------------------------------------

      (let* ((fvec0 (make-array 4 :element-type 'bit :initial-element 0))
             (fvec1 (make-array 4 :element-type 'bit :initial-element 0))
             (*index-to-fvec-alist* (list (cons 0 fvec0) (cons 1 fvec1))))
        (setf (aref fvec0 1) 1)
        (setf (aref fvec0 3) 1)
        (check "testindices: all-on -> t"
               (testindices 0 '(1 3))
               t)
        (check "testindices: one missing -> nil"
               (testindices 0 '(1 2))
               nil)
        (check "testindices: empty findices -> t (vacuous)"
               (testindices 0 nil)
               t)
        (check "testindices: different buffer position"
               (testindices 1 '(1))
               nil))


      ;; --- fast-is (macro expansion + runtime) --------------------
      ;;
      ;; FAST-IS expanded at file-compile time, baking in the
      ;; :FINDEX values that FEATINDEXIFY assigned. Here we just
      ;; set up an fvec consistent with those indices and call the
      ;; precompiled wrapper functions.

      (let* ((np-idx   (get 'test-fast-np :findex))
             (verb-idx (get 'test-fast-verb :findex))
             (size     (1+ (max np-idx verb-idx)))
             (fvec0    (make-array size :element-type 'bit
                                        :initial-element 0)))
        (let ((*index-to-fvec-alist* (list (cons 0 fvec0))))
          (setf (aref fvec0 np-idx) 1)
          (check "fast-is: matches when bit set"
                 (%fast-is-np)
                 t)
          (check "fast-is: fails when bit unset"
                 (%fast-is-verb)
                 nil)
          (setf (aref fvec0 verb-idx) 1)
          (check "fast-is: matches all in conjunction"
                 (%fast-is-both)
                 t)))


      ;; --- addf1 / remf1 -------------------------------------------

      (let ((n (make-test-node '(np))))
        (addf1 n '(verb finite))
        (check "addf1 prepends new features"
               (fe n)
               '(verb finite np)))

      (let ((n (make-test-node '(np verb finite past))))
        (remf1 '(verb past) n)
        (check "remf1 drops listed features (order otherwise preserved)"
               (fe n)
               '(np finite)))


      ;; --- relational predicates -----------------------------------

      (let ((n (make-test-node '(np verb finite))))
        (check "is: all present -> t"
               (is n '(np verb))
               t)
        (check "is: one missing -> nil"
               (is n '(np past))
               nil)
        (check "is: empty -> t (vacuous)"
               (is n '())
               t)

        (check "is-not-all-of: missing one -> t"
               (is-not-all-of n '(np past))
               t)
        (check "is-not-all-of: all present -> nil"
               (is-not-all-of n '(np verb))
               nil)

        (check "is-none-of: none present -> t"
               (is-none-of n '(past pp))
               t)
        (check "is-none-of: one present -> nil"
               (is-none-of n '(past np))
               nil)

        (check "is-any-of: one present -> t"
               (is-any-of n '(past np))
               t)
        (check "is-any-of: none present -> nil"
               (is-any-of n '(past pp))
               nil))


      ;; --- transfer / liftr ----------------------------------------

      (let ((src  (make-test-node '(np verb finite past)))
            (dest (make-test-node '(np))))
        (transfer '(verb past pp) src dest)
        ;; Only the intersection should land on dest, prepended.
        (check "transfer copies the intersection only"
               (fe dest)
               '(verb past np)))

      (let ((src  (make-test-node))
            (dest (make-test-node)))
        (setr 'subject 'src-np src)
        (liftr 'subject src dest)
        (check "liftr copies one register"
               (getr 'subject dest)
               'src-np)))

    results))
