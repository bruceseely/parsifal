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


(defun compile-crule-form (intermediate)
  "Given the BUILD-CRULE intermediate
       (crule NAME TYPE NODES BODY)
   return Marcus's emitted-Lisp form (glang.l 601-610's `crule' macro):
       (progn 'compile
              (crule-index 'TYPE '(NODE-SPEC ::crule-of-NAME NAME))
              (defun ::crule-of-NAME () BODY))
   where NODE-SPEC is the lone node-type for a creation crule, or
   (UPPER . LOWER) for an attachment crule -- exactly the shape the
   runtime CRULE-INDEX files (case-frame.lisp)."
  (destructuring-bind (crule-tag name type nodes body) intermediate
    (declare (ignore crule-tag))
    (let ((fn-name   (intern (format nil "::CRULE-OF-~A" name) :glang-cl))
          (node-spec (if (cdr nodes)
                         (cons (car nodes) (cadr nodes))
                         (car nodes))))
      (list 'progn
            ''compile
            (list 'crule-index
                  (list 'quote type)
                  (list 'quote (list node-spec fn-name name)))
            (list 'defun fn-name '() body)))))

(defun compile-rule-form (intermediate)
  "Given the BUILD-RULE intermediate
       (rule NAME (TYPE PRIORITY PACKETS (INDEXF . PFEATS)) PAT ACTION)
   return Marcus's emitted-Lisp form
       (progn 'compile
              (rule-index 'TYPE 'PACKETS 'INDEXF
                          '(PRIORITY ::pat-of-NAME NAME ::act-of-NAME))
              (defun ::pat-of-NAME () PAT)
              (defun ::act-of-NAME () ACTION)
              (featindexify ...)).
   Dispatches to COMPILE-CRULE-FORM for a `(crule ...)' intermediate."
  (when (eq (car intermediate) 'crule)
    (return-from compile-rule-form (compile-crule-form intermediate)))
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


;;; -------------------------------------------------------------------
;;; Linking a compiled rule to the :parsifal runtime
;;;
;;; COMPILE-RULE produces a portable data form, but its rule-local *data*
;;; symbols -- slot names (OBJ, SUBJ), register keys (CASEFRAME, DOW),
;;; features, packet names -- are interned in :glang-cl, where the
;;; tokenizer puts everything that isn't an exported :parsifal symbol.
;;; The runtime compares those by EQ against :parsifal symbols (e.g.
;;; `(member gfunc '(subj obj))' in FILLSLOT, `(getr 'caseframe node)'),
;;; so a :glang-cl `obj' or `caseframe' silently misses.
;;;
;;; LINK re-homes every symbol into :parsifal by name. Runtime functions
;;; and specials are already exported from :parsifal, so they re-resolve
;;; to themselves; CL symbols re-resolve to CL (`:parsifal' `:use's
;;; `:cl'); the data symbols become their :parsifal counterparts. After
;;; LINK the form is EVAL-able against the runtime.
;;; -------------------------------------------------------------------

(defun link (form)
  "Re-intern every symbol in FORM into :parsifal by name, returning a
   copy ready to run against the parsifal runtime. Keywords and NIL are
   left untouched."
  (labels ((tr (x)
             (cond ((null x) nil)
                   ((keywordp x) x)
                   ((symbolp x) (intern (symbol-name x) :parsifal))
                   ((consp x) (cons (tr (car x)) (tr (cdr x))))
                   (t x))))
    (tr form)))
