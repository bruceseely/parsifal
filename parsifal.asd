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
   #:say              #:say-it
   #:explodec         #:exploden            #:implode
   #:maknam           #:readlist            #:ascii
   #:getchar          #:flatc
   ;; --- Lexicon loader (com.l) ---------------------------------------
   #:df               #:df1                 #:df+
   #:jlike            #:abbrev
   #:get-string       #:put-string          #:reset-lexicon
   #:lexicon-words    #:lexicon-noise-p
   #:buildword        #:buildnumber         #:buildirregword
   #:expandsim        #:expanddef           #:expandm
   #:add-redunds      #:expandcf            #:modcasef
   #:mod-features     #:lowcaseify
   #:*wstring-tree*   #:*wstring-list*      #:*numbers*
   #:*always-expand*  #:*caseorder*
   ;; --- Morphology (com.l) -------------------------------------------
   #:morpho           #:origcase            #:try
   #:ends-in          #:strip-if            #:strip-if-any
   #:sta              #:modfe
   #:*vowels*         #:*consos*            #:*liquids*
   #:*noend*          #:*endpuncs*          #:*puncs*
   #:*rt*             #:*word*              #:*wrd*
   #:*modfelist*      #:*nextmorph*
   ;; --- Sentence input + parse driver (com.l / parse.l) --------------
   ;; (`tokenize' is intentionally NOT exported -- glang-cl defines its
   ;; own; and the driver is `parse-sentence', not `parse-string', for
   ;; the same reason.)
   #:nodify           #:nodify*             #:wordify
   #:read-sentence    #:reset-parser-state  #:parse-sentence
   #:*ordercounter*
   ;; --- Case frames (case.l) -----------------------------------------
   #:case-frame       #:associate-cf        #:assoc-node
   #:putc             #:getc                #:clearcf
   #:closeframe       #:openframe           #:openchek
   #:maxunls          #:minunls             #:open-obj-cases
   #:semcall          #:hypo-slots          #:objs-needed
   #:crule-index      #:reset-crules
   #:*create-rules*   #:*attach-rules*      #:fnode #:snode
   #:smqval           #:smqchek             #:smarkers
   #:maxsmqval        #:fit-of              #:fit-of-1
   #:cases            #:subjcases           #:objcases
   #:ppcases          #:subjcasegen         #:objcasegen
   #:ppcasegen        #:consolidate-frame   #:filter-out-filled
   #:bind-slots
   #:need-slots       #:set-objs-needed     #:fits
   #:fits*            #:fillslot            #:fillcase
   #:fillmod          #:fillpred            #:fillspec
   #:finalize-frame   #:passivize-cf        #:passivize-cf1
   #:prefer           #:domf                #:dom-cf
   #:plus             #:times
   #:redund           #:*redund-table*      #:*specregs* #:refillables
   #:rule-index       #:rem-index           #:testrules
   #:reset-rule-table #:parse-loop          #:act-of-rule
   #:fetchrules       #:rules-packets
   #:*rule-index*     #:*deriv*
   ;; Rule-type tags emitted by glang-cl (data, not functions);
   ;; exported so the symbol identity is shared between the
   ;; rule-index call site and TESTRULES.
   #:normal           #:as                  #:nr
   #:noindexf
   ;; Emitted but not yet implemented (pre-declared symbols)
   #:find-node        #:find-node1
   #:father-node      #:node-above
   #:binding          #:io
   #:current-s        #:wh-comp
   #:newnode          #:newcf
   #:makenode         #:makesym
   #:set*             #:setup*             #:nextword
   #:last*            #:buffer-gc
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
   #:*parsecomplete*  #:*it*
   #:*1stfeat*        #:*2ndfeat*           #:*3rdfeat*
   #:*1stfvec*        #:*2ndfvec*           #:*3rdfvec*
   #:*int-index*      #:*nr-types*          #:*as-types*
   #:*index-to-fvec-alist*                  #:*findex-counter*
   #:*wstring*        #:*nodelist*          #:*sentence-types*
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
                                                         (:file "macros2")
                                                         (:file "maclisp-chars")
                                                         (:file "primitives")
                                                         (:file "buffer-ops")
                                                         (:file "node-ops")
                                                         (:file "parse-loop")
                                                         (:file "defs")
                                                         (:file "lexicon")
                                                         (:file "morpho")
                                                         (:file "input")
                                                         (:file "case-frame")
                                                         (:file "util")
                                                         (:file "dictionary")
                                                         (:static-file "defs-dictionary.dict")
                                                         (:static-file "supplement.dict")))
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
                             (:file "macros2-test")
                             (:file "maclisp-chars-test")
                             (:file "primitives-test")
                             (:file "buffer-ops-test")
                             (:file "node-ops-test")
                             (:file "parse-loop-test")
                             (:file "lexicon-test")
                             (:file "morpho-test")
                             (:file "case-frame-test")
                             (:file "util-test")
                             (:file "dictionary-test")))))


;;; The glang-cl Pratt-style port of Marcus's grammar-language parser
;;; lives under system/reference/glang-cl/ and is not loaded as part
;;; of this system. It is loaded manually (or via its own asdf system
;;; once we add one) during cross-validation work.
