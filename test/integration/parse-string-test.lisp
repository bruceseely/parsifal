;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/parse-string-test.lisp
;;;
;;; End-to-end pipeline test for Increment 4: a sentence STRING parsed
;;; all the way through the real machinery --
;;;
;;;   read-sentence  (tokenize -> morpho -> lexicon -> nodify*)
;;;     -> parse-string (reset + initial rule + parse-loop)
;;;       -> glang-cl-compiled grammar rules
;;;
;;; The grammar here is small and self-contained (two rules using only
;;; already-supported glang-cl denotations), and the lexicon is built
;;; programmatically with df. Full gram1.l declarative-sentence parsing
;;; additionally needs case.l and the NP/clause rules in gram2.l/gram3.l,
;;; neither yet ported -- so this proves the input+driver pipeline, not
;;; the full English grammar.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/parse-string-test.lisp

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


(defun register-rules (srcs)
  "Compile and EVAL each glang-cl rule source so RULE-INDEX registers it."
  (dolist (src srcs)
    (eval (glang-cl::compile-rule-form (glang-cl::parse-rule src)))))


(defun parse-string-test (&optional verbose)
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

      ;; --- Lexicon: "it" is an NP pronoun, "." is punctuation.
      ;; Use features defs.lisp exports (np/pronoun/ns/punc) so the
      ;; lexicon's symbols are EQ to the ones the glang-cl grammar names.
      (dolist (w (list 'it (intern "." :parsifal)))
        (setf (symbol-plist w) nil))
      (df it feats (np pronoun ns))
      (%df (list (intern "." :parsifal) 'feats '(punc)))
      (expandsim 'it)
      (expandsim (intern "." :parsifal))

      ;; --- Grammar: create the S, then grab an NP + final punctuation.
      (register-rules
       '("{RULE INITIAL-RULE IN NOWHERE
          [t] -->
          Create a new s node.
          Activate ss-start.}"
         "{RULE NP-UTT IN SS-START
          [=np] [=punc] -->
          Label c np-utt.
          Attach 1st to c as np.
          Attach 2nd to c as punc.
          The parse is finished.}"))

      ;; --- The reader turns the string into two word-nodes.
      ;; (Checked before parsing: set*/nextword drains *wstring* into
      ;; the buffer during the parse.)
      (check "read-sentence builds a two-node word string"
             (length (read-sentence "it ."))
             2)

      ;; --- Drive it from a string.
      (let ((ok (parse-sentence "it ."
                                :initial-rule (intern "INITIAL-RULE" :glang-cl))))
        (check "parse-sentence succeeds on \"it .\""
               ok
               t)
        (check "derivation fired INITIAL-RULE then NP-UTT"
               (reverse (mapcar #'symbol-name *deriv*))
               '("INITIAL-RULE" "NP-UTT"))
        (truthy "the NP was attached to c"
                (daughters 'np c))
        (truthy "the punctuation was attached to c"
                (daughters 'punc c))
        (truthy "c is labelled np-utt"
                (member (intern "NP-UTT" :glang-cl) (fe c)))))

    (format t "~&parse-string-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(parse-string-test t)
