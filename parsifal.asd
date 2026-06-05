;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes -*-

(defpackage :parsifal
  (:use #:cl #:cl-user #:uiop)
  (:nicknames :pa))

(in-package :asdf-user)


(defsystem "parsifal"
  :description "Common Lisp port of Mitchell Marcus's PARSIFAL wait-and-see parser"
  :version "0.0.1"
  :depends-on (:cl-lex :yacc)
  :components ((:module "system"
                :components ((:module "core"
                              :components ((:module "rule-processing"
                                            :serial t
                                            :components ((:file "rule-lexer")
                                                         (:file "rule-parser")))))))
               (:module "test"
                :serial t
                :depends-on ("system")
                :components ((:file "test-all")
                             (:file "rule-lexer-test")
                             (:file "rule-parser-test")))))


;;; The glang-cl Pratt-style port of Marcus's grammar-language parser
;;; lives under system/reference/glang-cl/ and is not loaded as part
;;; of this system. It is loaded manually (or via its own asdf system
;;; once we add one) during cross-validation work.
