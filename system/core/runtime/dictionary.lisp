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

(defparameter *supplement-file*
  (asdf:system-relative-pathname :parsifal
                                 "system/core/runtime/supplement.dict")
  "Path to the SUPPLEMENTARY lexicon -- common words absent from Marcus's
   verbatim defs-dictionary.dict (which stays pristine). Loaded right after
   it, so the supplement's `jlike' targets already exist.")

(defun load-supplement (&optional (file *supplement-file*))
  "Load the supplementary lexicon (see *supplement-file*) with the same
   readtable/package as the canonical dictionary. Add words Marcus omitted
   here rather than editing his source. Returns FILE."
  (load-dictionary file))


(defparameter *give-class-time-verbs* '(give tell deliver)
  "Give-class transfer verbs to receive a TIME case (see AUGMENT-GIVE-CLASS-TIME).
   Base verbs only; jlike descendants (persuade/say/promise/ask/buy/change/hope)
   inherit through *specregs* when expanded.")

(defun augment-give-class-time (&optional (verbs *give-class-time-verbs*))
  "OUR extension (NOT a Marcus port): give an optional, refillable TIME case to
   the give-class transfer verbs, which take temporal adjuncts in English
   (\"give Sue the book tomorrow\") but which Marcus entered with no TIME slot --
   so a bare temporal had nowhere to land and dropped. We splice `(time opt)'
   into each base verb's expanded `case-frame' via MODCASEF, right AFTER the
   dictionary loads; the jlike descendants (persuade/promise/ask/..., expanded
   lazily at parse time) then inherit the augmented frame via `case-frame' being
   a *specreg*. With VP-TIME-NP-TO-PP (*bare-time-rules*) turning a bare temporal
   into a `during'-PP, VP-PP now FILLS this TIME case (was: a dropped BAD object).

   defs-dictionary.dict stays PRISTINE and the case-frame engine ports are
   untouched -- this is a decoupled post-load pass, the lexical analogue of
   *grammar-extensions*. Deliberately TARGETED, not a universal TIME slot: these
   verbs carry no `for'/`to'/`on' case that a new TIME slot could compete with in
   ppcasegen, so no existing PP changes which case it fills.

   NB placement: `(time)' must go just BEFORE the clause-final SUBJECT case
   (agt), matching every native-TIME frame Marcus wrote (schedule: (neut (time)
   (loc) agt); pay). Appended AFTER agt (e.g. via MODCASEF, whose ordering is
   degenerate with *caseorder* NIL) the during-PP does not fit the slot and TIME
   drops -- verified. Marcus keeps the subject case last, so we splice before it.

   We edit the RAW, unexpanded `cf' (not the expanded `case-frame'), so we do NOT
   pre-expand the verb here: expansion is left lazy, EXPANDCF normalizes the added
   `(time)' to `(time opt)' at first lookup, and the raw `feats' stay intact
   (pre-expanding would consume them). Descendants still inherit: a jlike word
   copies its model's expanded `case-frame' (a *specreg*) at its own expansion,
   which by then reflects the augmented `cf'."
  (dolist (v verbs)
    (let ((cf (get v 'cf)))             ; RAW cf; leave expansion lazy
      (when (and cf (not (find 'time cf :key (lambda (c) (if (consp c) (car c) c)))))
        ;; splice (time) in just before the clause-final subject case:
        (setf (get v 'cf) (append (butlast cf) '((time)) (last cf)))))))


;;; Populate the lexicon when the system loads: Marcus's dictionary first,
;;; then our supplement (whose jlike targets it provides), then our lexical
;;; extensions (give-class TIME case).
(eval-when (:load-toplevel :execute)
  (load-dictionary)
  (load-supplement)
  (augment-give-class-time))
