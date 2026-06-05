;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/reference/glang-cl/compiler.lisp
;;;
;;; The post-parse rule expander. Marcus's `rule' macro (glang.l lines
;;; 566-599) transforms the intermediate form produced by BUILD-RULE
;;; into the final compiled-Lisp triple. We do the same here, but as a
;;; function returning data rather than a macro that defunes things --
;;; the goal of this port is to PRODUCE Marcus's form, not to RUN it
;;; (the runtime primitives RULE-INDEX, ACTIVATE, etc. live in the
;;; missing parse.l).

(in-package #:glang-cl)


(defun compile-rule-form (intermediate)
  "Given the BUILD-RULE intermediate
       (rule NAME (TYPE PRIORITY PACKETS (INDEXF . PFEATS)) PAT ACTION)
   return Marcus's emitted-Lisp form
       (progn 'compile
              (rule-index 'TYPE 'PACKETS 'INDEXF
                          '(PRIORITY ::pat-of-NAME NAME ::act-of-NAME))
              (defun ::pat-of-NAME () PAT)
              (defun ::act-of-NAME () ACTION)
              (featindexify ...))."
  (destructuring-bind (rule-tag name index-info pat-body act-body)
      intermediate
    (declare (ignore rule-tag))
    (destructuring-bind (type priority packets feature-info) index-info
      (let* ((indexf (car feature-info))
             (pfeats (cdr feature-info))
             (pat-name (intern (format nil "::PAT-OF-~A" name) :glang-cl))
             (act-name (intern (format nil "::ACT-OF-~A" name) :glang-cl)))
        (list 'progn
              ''compile
              (list 'rule-index
                    (list 'quote type)
                    (list 'quote packets)
                    (list 'quote indexf)
                    (list 'quote (list priority pat-name name act-name)))
              (list 'defun pat-name '() pat-body)
              (list 'defun act-name '() act-body)
              (list 'featindexify
                    (when pfeats (list 'quote (reverse pfeats)))))))))


(defun compile-rule (string)
  "Tokenize, Pratt-parse, and expand STRING (a single `{...}' rule)
   into Marcus's emitted-Lisp form. Returns the form as data; does not
   evaluate it."
  (compile-rule-form (parse-rule string)))
