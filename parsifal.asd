;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes -*-

(defpackage :parsifal
  (:use #:cl #:cl-user #:uiop)
  (:nicknames :pa)
  (:export
   ;; --- Runtime functions / macros emitted by glang-cl ----------------
   ;; (Each one corresponds to a function or macro in Marcus's parse.l;
   ;; many are not yet implemented, but the symbol must be exported now
   ;; so the symbol identity is shared between :glang-cl emissions and
   ;; the :parsifal runtime.)
   #:activate         #:deactivate
   #:addf1            #:remf1
   #:attach           #:attach1
   #:drop             #:insert-node
   #:bufrestore
   #:insert-index-pos #:remove-index-pos
   #:fast-is          #:testindices         #:featindexify
   #:fe               #:setfe
   #:is               #:is-not-all-of
   #:is-none-of       #:is-any-of
   #:transfer         #:liftr
   #:setr             #:getr
   #:flags            #:setflags            #:setup**
   #:clear-current-s  #:activatenode
   #:attach-monitor   #:create-monitor
   #:for              #:consprop
   #:rule-index
   ;; Emitted but not yet implemented (pre-declared symbols)
   #:find-node        #:find-node1
   #:father-node      #:node-above
   #:binding          #:io
   #:current-s        #:wh-comp
   #:newnode          #:newcf
   #:makenode         #:makesym
   #:set*             #:setup*
   #:word             #:s-type
   #:head             #:root-of
   #:nid              #:node-id
   #:daughters        #:daughter
   #:alt-attach       #:alt-fillslot
   #:setup-current-s
   ;; --- Special variables --------------------------------------------
   #:*activepackets*  #:*activerule*
   #:*activenodestak* #:*nextrule*
   #:*bufpntr*        #:*bufpntrstak*       #:*bufmax*
   #:*buffer*         #:*buffer-gc*
   #:*current-s*      #:*wh-comp*           #:*rset
   #:*parsecomplete*
   #:*1stfeat*        #:*2ndfeat*           #:*3rdfeat*
   #:*1stfvec*        #:*2ndfvec*           #:*3rdfvec*
   #:*int-index*      #:*nr-types*          #:*as-types*
   #:*index-to-fvec-alist*                  #:*findex-counter*
   ;; Plain-symbol specials Marcus declared (preserved verbatim because
   ;; glang-cl emits them directly into rule bodies)
   #:s #:c #:nth
   #:|1ST| #:|2ND| #:|3RD|))

(in-package :asdf-user)


(defsystem "parsifal"
  :description "Common Lisp port of Mitchell Marcus's PARSIFAL wait-and-see parser"
  :version "0.0.1"
  :depends-on (:cl-lex :yacc)
  :components ((:module "system"
                :components ((:module "core"
                              :components ((:module "runtime"
                                            :serial t
                                            :components ((:file "declr")
                                                         (:file "primitives")
                                                         (:file "buffer-ops")
                                                         (:file "node-ops")))
                                           (:module "rule-processing"
                                            :serial t
                                            :components ((:file "rule-lexer")
                                                         (:file "rule-parser")))))))
               (:module "test"
                :serial t
                :depends-on ("system")
                :components ((:file "test-all")
                             (:file "rule-lexer-test")
                             (:file "rule-parser-test")
                             (:file "declr-test")
                             (:file "primitives-test")
                             (:file "buffer-ops-test")
                             (:file "node-ops-test")))))


;;; The glang-cl Pratt-style port of Marcus's grammar-language parser
;;; lives under system/reference/glang-cl/ and is not loaded as part
;;; of this system. It is loaded manually (or via its own asdf system
;;; once we add one) during cross-validation work.
