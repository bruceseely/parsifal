;;;This file intended to be loaded within a declare in each parser file
(fasl "/usr/mitch/lisp/util/macros1")
(fasl "/usr/mitch/lisp/parsifal/macros2")
;(fasl "/usr/mitch/lisp/util/defmac")
(setsyntax '\# 2)

(declare 
  (macros t))
  (special :activepackets :activerule :activenodestak :nextrule :bufpntr 
	   :bufpntrstak :bufmax s c :current-s :wh-comp *rset
	   :parsecomplete 1st 2nd 3rd nth :1stfeat :2ndfeat :3rdfeat
	   :int-index :nr-types :as-types :1stfvec :2ndfvec :3rdfvec
	   ###started word-entry-switch testnum tree? ttysw bottom-tty
	   :noshow-defaults pswait abbr-temp endpuncs poport piport :deriv
	   :pstatus :breaksw :ruletrap :sent-list 
	   :showrule :noshowr :noshowr-defaults
	   :sent :osent prinlength prinlevel
	   :carefulsw :index-to-fvec-alist ctrace :crtrace :psych-sw tyo tyi
	   :nodelist :sentence-types :ttemp2 rule-unfunc rule-func :buffer-gc
	   top-tty :displayflag
	   :ttemp :ttemp1 :ptime :gctime :realtime :endtime :gram-stats 
	   :ptrace :ntrace :wstring :savewstring
	   hypoth-frames certain-frame openframe pred old-frames 
	   :findex-counter :nodecountsw :word-entry-switch)
 ;(fixnum :bufpntr :bufmax  :findex-counter :nodecountsw)
 ;(notype (set* fixnum) (testrules notype fixnum) (fetchrules notype fixnum)
 ;       (setup* notype fixnum) (remove-index-pos fixnum)
 ;       (insert-index-pos fixnum notype))
; (array* (notype :buffer 1))
  (*lexpr ptree union meet cat semcall)


(comment macro definitions)
;;the cdr of a node is a fixnum which contains its flags.
(defmacro flags (node) `(cdr ,node))

(defmacro setflags (node val) `(rplacd ,node ,val))

(defmacro setup** (nname nfeat nfvec)
          ;;clear appropriate feature vector.
   `(progn
     (do ((findex ,nfeat (cdr findex))) 
	 ((null findex))
	 (store (,nfvec (car findex)) 0.))
          ;;signal that nfvec is in inconsistent state.
          ;;(Just in case this routine interrupted out of)
          (setq ,nfeat 'changing)
          ;;set apppropriate positions and save indices.
          (do ((f (fe node) (cdr f))
               (result) (temp))
              ((null f) (setq ,nfeat result))
              (if* (setq temp (get (car f) ':findex))
                  (store (,nfvec temp) 1)
                  (setq result (cons temp result))))
          (setq ,nname node)))

(defmacro clear-current-s nil `(if* :current-s (setq :current-s nil :wh-comp nil)))

(defmacro activatenode (node)
	  `(progn (if* c (push (cons c :activepackets) :activenodestak))
		 (setq :activepackets nil)
		 (clear-current-s)
		 (setq c ,node)))

(defmacro setfe (node features) `(set (car ,node) ,features)) 

(defmacro fe (node) `(symeval (car ,node)))

(defmacro setr (reg value node) `(putprop (car ,node) ,value ,reg))  

(defmacro getr (register node)
          `(get (car ,node) ,register))

