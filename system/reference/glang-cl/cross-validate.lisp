;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/reference/glang-cl/cross-validate.lisp
;;;
;;; Cross-validation harness: feed each rule from a corpus file to BOTH
;;; the existing rule-frame parser (PARSIFAL::PARSE-RULE-TEXT, cl-yacc-
;;; based) and glang-cl's PARSE-RULE-HEADER (Pratt-based), then compare
;;; the headers they agree on: kind, name, priority, and packets.
;;;
;;; This is a smoke check that the two independently-built parsers see
;;; the same rule frames. If they disagree, that's evidence one of them
;;; has the rule grammar wrong. If they agree across the whole corpus,
;;; that's evidence the rule-frame parser is structurally faithful to
;;; Marcus's actual reader (the one in glang.l).

(in-package #:glang-cl)


(defun split-rules-by-braces (text)
  "Split TEXT into balanced top-level {...} chunks (the same chunker
   used by the rule-frame parser's regression tests). Each chunk is the
   substring from after the previous top-level `}' to and including the
   next top-level `}'."
  (let ((result '()) (start 0) (depth 0))
    (loop for i from 0 below (length text)
          for c = (char text i)
          do (cond
               ((char= c #\{) (incf depth))
               ((char= c #\})
                (decf depth)
                (when (zerop depth)
                  (push (subseq text start (1+ i)) result)
                  (setf start (1+ i))))))
    (nreverse result)))


(defun read-whole-file (path)
  (with-output-to-string (out)
    (with-open-file (in path :direction :input)
      (loop for line = (read-line in nil nil)
            while line do (write-line line out)))))


(defun normalize-symbol (sym)
  "Return SYM's name as an uppercase string. Used to compare a glang-cl
   symbol against the rule-frame parser's string output."
  (cond
    ((null sym) nil)
    ((symbolp sym) (string-upcase (symbol-name sym)))
    ((stringp sym) (string-upcase sym))
    (t (princ-to-string sym))))


(defun normalize-packets (lst)
  (mapcar #'normalize-symbol lst))


(defun compare-headers (ours theirs)
  "Return a list of (field expected-from-rfp actual-from-glang) for any
   field that disagrees. Empty list means full agreement on the four
   compared fields. OURS is the rule-frame parser's plist; THEIRS is
   PARSE-RULE-HEADER's plist."
  (let ((diffs '()))
    ;; kind
    (let ((a (getf ours :kind))
          (b (getf theirs :kind)))
      (unless (eq a b)
        (push (list :kind a b) diffs)))
    ;; name
    (let ((a (normalize-symbol (getf ours :name)))
          (b (normalize-symbol (getf theirs :name))))
      (unless (equal a b)
        (push (list :name a b) diffs)))
    ;; priority -- the rule-frame parser leaves it NIL when the rule
    ;; doesn't say PRIORITY:; glang-cl defaults to 10 in that case.
    ;; Treat NIL and 10 as equivalent for this comparison.
    (let ((a (or (getf ours :priority) 10))
          (b (or (getf theirs :priority) 10)))
      (unless (eql a b)
        (push (list :priority a b) diffs)))
    ;; packets (ordinary rules only)
    (when (member (getf ours :kind) '(:rule :as-rule :nr-rule))
      (let ((a (normalize-packets (getf ours :packets)))
            (b (normalize-packets (getf theirs :packets))))
        (unless (equal a b)
          (push (list :packets a b) diffs))))
    (nreverse diffs)))


(defun cross-validate-file (path &key verbose)
  "Run the cross-validation against PATH (a corpus file). Returns a
   plist summary."
  (let ((chunks (split-rules-by-braces (read-whole-file path)))
        (both-ok 0) (ours-only 0) (theirs-only 0) (mismatches 0)
        (both-failed 0) (failures '()))
    (loop for chunk in chunks for n from 1 do
      (let ((ours (handler-case (parsifal::parse-rule-text chunk)
                    (error () nil)))
            (theirs (handler-case (parse-rule-header chunk)
                      (error () nil))))
        (cond
          ((and ours theirs)
           (let* ((ours-rule (first ours))
                  (diffs (compare-headers ours-rule theirs)))
             (cond
               ((null diffs)
                (incf both-ok)
                (when verbose
                  (format t "~&  ~3d agree: ~12a ~a~%"
                          n (getf theirs :kind) (getf theirs :name))))
               (t
                (incf mismatches)
                (push (list :chunk n :diffs diffs :ours ours-rule
                            :theirs theirs)
                      failures)
                (format t "~&  ~3d DIFFER: ~a~%" n (getf theirs :name))
                (dolist (d diffs)
                  (format t "      ~10a expected ~s got ~s~%"
                          (first d) (second d) (third d)))))))
          (ours
           (incf ours-only)
           (push (list :chunk n :note "only rule-frame parser succeeded"
                       :ours (first ours))
                 failures)
           (format t "~&  ~3d glang-cl FAILED: ~a (rule-frame OK: ~a)~%"
                   n (getf (first ours) :kind) (getf (first ours) :name)))
          (theirs
           (incf theirs-only)
           (push (list :chunk n :note "only glang-cl succeeded"
                       :theirs theirs)
                 failures)
           (format t "~&  ~3d rule-frame FAILED: ~a (glang-cl OK: ~a)~%"
                   n (getf theirs :kind) (getf theirs :name)))
          (t
           (incf both-failed)
           (push (list :chunk n :note "both failed")
                 failures)
           (when verbose
             (format t "~&  ~3d both FAILED on chunk~%" n))))))
    (format t "~&~%--- summary ---~%")
    (format t "~&  chunks total:         ~d~%" (length chunks))
    (format t "~&  both agree:           ~d~%" both-ok)
    (format t "~&  field mismatches:     ~d~%" mismatches)
    (format t "~&  only rule-frame OK:   ~d~%" ours-only)
    (format t "~&  only glang-cl OK:     ~d~%" theirs-only)
    (format t "~&  both failed:         ~d   (non-rule chunks)~%" both-failed)
    (list :chunks (length chunks)
          :both-ok both-ok
          :mismatches mismatches
          :ours-only ours-only
          :theirs-only theirs-only
          :failures (nreverse failures))))
