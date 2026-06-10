;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-sentence-test.lisp
;;;
;;; Full-sentence integration against the REAL gram1.l grammar (not a
;;; toy): compile Marcus's verbatim rules with glang-cl, LINK them into
;;; :parsifal, register them, build a matching lexicon, and parse a
;;; sentence end to end through the runtime's wait-and-see loop.
;;;
;;;   read-sentence -> parse-sentence -> parse-loop
;;;     -> linked gram1.l rules firing against the runtime
;;;
;;; This is the first time the linked grammar and the runtime primitives
;;; run together. The current reach is a single-NP *utterance* (gram1's
;;; INITIAL-RULE + NP-UTTERANCE): a pronoun or other ready-made NP plus
;;; final punctuation. NP *construction* (det+noun -> NP) additionally
;;; needs NR-rule firing in the loop (NBAR-COMPLETE / NP-COMPLETE are NR
;;; rules), which set* does not yet trigger -- see system/core/runtime/
;;; parse-loop.lisp's SET* note. So this proves: the real INITIAL-RULE
;;; (with its `!(setq s {the current s})' escape), LINK, the rule index,
;;; the buffer/driver, and an utterance-level gram1 rule all work
;;; together.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-sentence-test.lisp

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


(defun register-linked-rules (srcs)
  "Compile each glang-cl rule SRC, LINK it into :parsifal, and EVAL it so
   RULE-INDEX registers it with data symbols (slots, features, packets)
   that are EQ to the runtime's."
  (dolist (src srcs)
    (eval (glang-cl::link (glang-cl::compile-rule src)))))


(defun gram1-sentence-test (&optional verbose)
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
               (setf results (and pass results)))))

      (reset-rule-table)
      (reset-lexicon)

      ;; --- Lexicon (in :parsifal, so its features are EQ to the linked
      ;; grammar's): "it" is a pronoun NP, "." is final punctuation.
      (dolist (w (list 'it (intern "." :parsifal)))
        (setf (symbol-plist w) nil))
      (df it feats (np pronoun ns))
      (%df (list (intern "." :parsifal) 'feats '(finalpunc punc)))
      (expandsim 'it)
      (expandsim (intern "." :parsifal))

      ;; --- Grammar: Marcus's gram1.l INITIAL-RULE and NP-UTTERANCE,
      ;; verbatim (INITIAL-RULE includes the `!(setq s {the current s})'
      ;; Lisp/grammar escape).
      (register-linked-rules
       '("{RULE INITIAL-RULE IN NOWHERE
          [t] -->
          Create a new s node.
          !(setq s {the current s}).
          Activate cpool,ss-start.}"
         "{RULE NP-UTTERANCE IN SS-START
          [=np] [=finalpunc] -->
          Label c np-utterance.
          Attach 1st to c as np.
          Attach 2nd to c as finalpunc.
          The parse is finished.}"))

      ;; --- Parse "it ." end to end.
      (let ((ok (parse-sentence "it ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse-sentence succeeds on \"it .\"" ok t)
        (check "fired INITIAL-RULE then NP-UTTERANCE"
               (reverse (mapcar #'symbol-name *deriv*))
               '("INITIAL-RULE" "NP-UTTERANCE"))
        (truthy "s (root) was captured via !(setq s {the current s})"
                (and s (member 's (fe s))))
        (truthy "c is labelled np-utterance"
                (member 'np-utterance (fe c)))
        (truthy "the NP (\"it\") was attached to c"
                (daughters 'np c))
        (truthy "the final punctuation was attached to c"
                (daughters 'finalpunc c))))

    (format t "~&gram1-sentence-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-sentence-test t)
