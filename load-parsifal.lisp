;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; load-parsifal.lisp
;;;
;;; One-step loader for an interactive session. From a fresh Lisp:
;;;
;;;     (load "/path/to/parsifal/load-parsifal.lisp")
;;;
;;; and you are ready to parse -- no need to remember the quickload +
;;; grammar-file + register-grammar sequence. It may be LOADed by absolute
;;; path from any directory: it makes the repo ASDF-discoverable itself (no
;;; ~/quicklisp/local-projects symlink needed) and leaves the repo root as
;;; the current directory, so the relative paths used elsewhere in the
;;; README resolve afterwards. After this returns:
;;;
;;;     (in-package :parsifal)
;;;     (parse-sentence "the boy scheduled the meeting ."
;;;                     :initial-rule (intern "INITIAL-RULE" :parsifal))
;;;
;;; Why this is needed: `(ql:quickload :parsifal)' loads only the runtime
;;; (the ASDF system). The GRAMMAR -- the rule groups, *full-grammar*, and
;;; `load-full-grammar' -- lives in system/grammar/clause-grammar.lisp, which
;;; is deliberately NOT an ASDF component, so it must be loaded separately and
;;; then REGISTERED into the (image-state) rule table. Re-run this loader (or
;;; just `(pa::load-full-grammar)') after anything that clears that table.

(let* ((here (or *load-truename* *load-pathname*
                 (error "load-parsifal.lisp must be LOADed, not pasted.")))
       (root (make-pathname :directory (pathname-directory here)
                            :name nil :type nil :defaults here)))

  ;; Make the repo discoverable to ASDF from wherever it lives, so this works
  ;; without a ~/quicklisp/local-projects symlink.
  (pushnew root asdf:*central-registry* :test #'equal)

  ;; Make the repo the current directory, for Lisp and for the OS. These are
  ;; two independent settings: `uiop:chdir' moves the OS process cwd, while
  ;; relative pathnames handed to `load' / `probe-file' / `compile-file'
  ;; resolve against *default-pathname-defaults*. Setting only the former
  ;; leaves the relative loads this README documents -- e.g.
  ;;   (load "system/reference/glang-cl/package.lisp")
  ;; -- still failing with "file does not exist". ROOT already ends in a
  ;; directory component, so no trailing-slash fixup is needed here.
  (uiop:chdir root)
  (setf *default-pathname-defaults* root)

  ;; 1. Runtime (the ASDF system). Prefer Quicklisp (pulls cl-lex/yacc), fall
  ;;    back to plain ASDF if Quicklisp isn't present.
  (if (find-package :quicklisp)
      (funcall (read-from-string "ql:quickload") :parsifal :silent t)
      (asdf:load-system :parsifal))

  ;; 2. Grammar (NOT an ASDF component -- see header).
  (load (merge-pathnames "system/grammar/clause-grammar.lisp" root))

  ;; 3. Register *full-grammar* into the rule table.
  (funcall (read-from-string "parsifal::load-full-grammar"))

  (format t "~&;; Parsifal loaded: runtime + full grammar registered.~%~
               ;; Parse with (in-package :parsifal) then~%~
               ;;   (parse-sentence \"...\" ~
               :initial-rule (intern \"INITIAL-RULE\" :parsifal))~%~
               ;; Re-run (pa::load-full-grammar) if the rule table is cleared.~%"))
