;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/core/runtime/dictionary.lisp
;;;
;;; Loads the lexicon half of defs.l -- the hundreds of word definitions
;;; in `defs-dictionary.dict' -- into the parser's plists. The .dict file
;;; is Marcus's source verbatim (modulo six dropped terminal-noise forms;
;;; see its header), so it is read with a MacLISP-flavored readtable, not
;;; the standard CL one:
;;;
;;;   #   In the lexicon `#' is the marker-set separator (the "ok # great"
;;;       split SMQVAL reads, case-frame.lisp), i.e. the symbol |#|. CL
;;;       makes `#' a dispatching macro char, so we give it constituent
;;;       syntax here -- it then reads as |#|.
;;;   :   A bare `:' appears as a punctuation "word" (e.g. `(jlike - :)').
;;;       CL's token parser rejects a lone colon (package marker), so we
;;;       install a reader macro that returns the symbol |:|. Escaped
;;;       colons (`\:') and -- since the lexicon has no package-qualified
;;;       symbols -- everything else are unaffected.
;;;
;;; The definers themselves (df / df1 / df+ / jlike / abbrev) live in
;;; lexicon.lisp; this file only adds the no-op `comment' macro the data
;;; uses and the readtable + loader. It is the last runtime component, so
;;; every definer it calls is already in place.

(in-package :parsifal)


(defmacro comment (&rest body)
  "MacLISP's `comment' -- a no-op that swallows its unevaluated body.
   The dictionary has two (`(comment may is ambigous!!!)')."
  (declare (ignore body))
  nil)


(defvar *dictionary-readtable*
  (let ((rt (copy-readtable nil)))
    ;; `#' -> constituent, so a bare `#' reads as the symbol |#|.
    (set-syntax-from-char #\# #\a rt)
    ;; bare `:' -> the symbol |:| (escaped `\:' still reads as |:| via
    ;; the normal single-escape, bypassing this macro char).
    (set-macro-character #\:
                         (lambda (stream char)
                           (declare (ignore stream char))
                           '|:|)
                         nil rt)
    rt)
  "Readtable for reading defs-dictionary.dict: `#' is a constituent and a
   bare `:' reads as |:|.")

(defparameter *dictionary-file*
  (asdf:system-relative-pathname :parsifal
                                 "system/core/runtime/defs-dictionary.dict")
  "Path to the lexicon data file in the source tree. (Resolved via ASDF,
   not *load-truename*, since the fasl loads from ASDF's output cache.)")

(defun load-dictionary (&optional (file *dictionary-file*))
  "Read and evaluate the word definitions in FILE with the dictionary
   readtable installed, populating the lexicon plists. Returns FILE."
  (let ((*readtable* *dictionary-readtable*)
        (*package*   (find-package :parsifal)))
    (load file))
  file)


;;; Populate the lexicon when the system loads.
(eval-when (:load-toplevel :execute)
  (load-dictionary))
