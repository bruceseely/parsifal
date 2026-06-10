;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/nr-rule-test.lisp
;;;
;;; Integration test for NR (node-reactivation) rule firing in the
;;; wait-and-see loop. SET* has two NR-firing sites (parse.l 253):
;;;   (A) a pre-check on the previously set-up node NTH, run when a rule
;;;       looks past the buffer start (set*(1+)); and
;;;   (B) the main loop, when an attached unchecked NR-type node is
;;;       removed.
;;; This exercises (A): the NR mechanism the real gram3 NP-COMPLETE /
;;; NBAR-COMPLETE rules rely on -- they fire because an utterance/clause
;;; rule looks ahead past the freshly built NP.
;;;
;;; Minimal grammar (toy, but drives the *real* loop + glang-cl + LINK):
;;;   INITIAL-RULE  create S, activate cpool + ss-start
;;;   MAKE-NBAR     [=noun] : build an nbar (an NR-type), drop it into
;;;                 the buffer
;;;   NBAR-UTT      [=nbar][=finalpunc] : looks ahead (tests 2nd), which
;;;                 triggers the (A) NR pre-check on the nbar
;;;   NBAR-NR (NR)  [=nbar] in cpool : fires on the reactivated nbar
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/nr-rule-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (unless (find-package :parsifal)
    (asdf:load-system :parsifal))
  (unless (find-package :glang-cl)
    (let* ((here  (or *load-truename* *compile-file-truename*))
           (root  (make-pathname
                   :defaults here
                   :directory (butlast (pathname-directory here) 2)))
           (glang (merge-pathnames "system/reference/glang-cl/" root)))
      (dolist (f '("package" "tokens" "pratt" "fixes"
                   "denotations" "compiler"))
        (load (merge-pathnames (format nil "~a.lisp" f) glang))))))

(in-package :parsifal)


(defun nr-rule-test (&optional verbose)
  (let ((results t))
    (flet ((check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%"
                           expected actual)))
               (setf results (and pass results))))
           (truthy (test-name actual)
             (let ((pass (and actual t)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass (format t "    expected non-NIL, got NIL~%")))
               (setf results (and pass results))))
           (reg (src)
             (eval (glang-cl::link (glang-cl::compile-rule src)))))

      (reset-rule-table)
      (reset-lexicon)

      ;; --- Lexicon: "dog" a noun, "." final punctuation.
      (dolist (w (list 'dog (intern "." :parsifal)))
        (setf (symbol-plist w) nil))
      (df dog feats (noun ns))
      (%df (list (intern "." :parsifal) 'feats '(finalpunc punc)))
      (expandsim 'dog)
      (expandsim (intern "." :parsifal))

      ;; --- Grammar.
      (reg "{RULE INITIAL-RULE IN NOWHERE
            [t] -->
            Create a new s node.
            !(setq s {the current s}).
            Activate cpool,ss-start.}")
      (reg "{RULE MAKE-NBAR IN SS-START
            [=noun] -->
            Attach 1st to a new nbar node as noun.
            Drop c into the buffer.}")
      (reg "{RULE NBAR-UTT IN SS-START
            [=nbar] [=finalpunc] -->
            Attach 1st to c as nbar.
            Attach 2nd to c as finalpunc.
            The parse is finished.}")
      (reg "{NR RULE NBAR-NR IN CPOOL
            [=nbar] -->
            Label c nbar-fired.
            Drop c.
            Restore the buffer.}")

      ;; --- Parse "dog ." -- the NR rule must fire when NBAR-UTT looks
      ;; ahead past the freshly built nbar.
      (let ((ok (parse-sentence "dog ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds" ok t)
        (check "NR rule NBAR-NR fired (between MAKE-NBAR and NBAR-UTT)"
               (reverse (mapcar #'symbol-name *deriv*))
               '("INITIAL-RULE" "MAKE-NBAR" "NBAR-NR" "NBAR-UTT"))
        (truthy "the nbar was reactivated and labelled by the NR rule"
                ;; the nbar node carries the NR rule's label
                (member 'nbar-fired
                        (let ((nb (daughter 'nbar c)))
                          (and nb (fe nb)))))))

    (format t "~&nr-rule-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(nr-rule-test t)
