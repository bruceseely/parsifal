 (declare
  (special :glangsyn :lispsyn :eof :token :left :drbp :fun :isfun :defbp 
	   :rulename :gtest :semi-flag :rulename :nodevarlist :nodevars 
	   :indexf :pfeats)
  (macros t))
(declare (include declr))

(eval-when (compile) (setsyntax '\[ 'vcharacter) (setsyntax '\] 'vcharacter))
;;;The translator from Parsifal's grammar language into LISP.
;;;Top down operator precedence parser for Parsifal's grammar language
;;;The parser itself is adapted from PRATT;CGOL >
;;;This parser converts the grammar language into LISP

(setq :defbp 25.) 

;;;The parser's main loop
(comment the main loop of the translator)
;;Note that advance flushes "the" "an" and "a" from the input
(setq :eof (ncons nil))

(defun advance nil 
       (cond ((eq (setq :token (read :eof)) :eof)
	      (throw 'lederr-bug :bug))
             ((memq :token '(the an a)) (advance))
	     (:token))) 

(defun verify (den) (cond (den (progn (advance) den)))) 

(defun associate (rbp :left) 
       (declare (fixnum rbp))
       (cond ((lessp rbp (or (and (atom :token) (get :token ':lbp)) 0.))
	      (associate rbp
			 (funcall (or (verify (get :token ':led))
					  (lederr)))))
	     (:left))) 

(defun :parse (rbp) 
       (declare (fixnum rbp))
       (associate rbp
		  (cond ((and (atom :token) (get :token ':nud))
			 (funcall (prog1 (get :token ':nud) (advance))))
			((prog1 :token (advance))))))


(defun lederr nil 
       (if* (boundp ':rulename) 
           (say |Error in| $ :rulename))
       (say error \- $ :token does not take a left argument\.)
       (say |:LEFT is|)
       (sprinter :left)
       (say |Following context:|)
       (do ((i 25. (1- i)) (ch (tyipeek nil) (tyipeek nil)))
           ((or (zerop i) (= ch 175)))
           (declare (fixnum ch))
           (tyo (tyi)))
       (throw 'lederr-bug :bug)) 

(defprop \} -1. :lbp) 

;;reader hackery
(comment reader hackery for the translator)
(defun glang-setup nil 
       (ifnot (boundp ':lispsyn) (setq :lispsyn readtable))
       (setsyntax '\{ 'vmacro 'glang-read)
       (for (readtable (setq :glangsyn (makereadtable t)))
	    (mapc '(lambda (x) (setsyntax x 'vsingle-character-symbol)) 
	    ;;make single char atoms
		  (explodec '|[]\{}()';,.=|))			
		;Note that . isn't even decimal point, any more (ughh!)
	    ;(setsyntax '|.| 197056. nil)
	    (setsyntax '\% 'vsplicing-macro 'glang-\%-macro)
	    (setsyntax '\! 'vmacro 'glang-!-macro))t) 

(defun glang-\%-macro nil 
       (do ((ch (ascii (tyipeek nil)) (ascii (tyipeek nil))))
	   ((eq ch '\%) (tyi t) nil)
	   (readch t))) 

(defun glang-!-macro nil ((lambda (readtable) (read t)) :lispsyn)) 

(defun glang-read nil 
  (for (ibase 10. readtable :glangsyn 
	      uctolcstate (status uctolc))
       (prog1 
	(for (temp) 
	     (sstatus uctolc t)
	     ;;total kludge = dangerous, awaiting liszt fill 2/9/82
	     (catch (advance) eofm)
	     (setq temp (catch (ncons (:parse -1.)) :bug))
	     (cond ((atom temp)
		    (for (readtable :lispsyn base 10.)
			 (cond ((eq temp 'eof-bug)
				(warn 1. |EOF encountered in grammar rule.|))
			       ((eq temp 'lederr-bug)
				(finish-grule)
				(break 'lederr-bug t)))))
		   ((car temp))))
	(funcall 'sstatus (list 'uctolc uctolcstate)))))
  ;%

;;utilities for defining translation functions
(comment definitional utilities)

(defun right nil (:parse :drbp)) 

(defun rightlist nil (:parselist :drbp '|,|)) 

(defun deffix macro (l) 
       (prog (dentype :isfun :fun denname :isfun :drbp) 
	     (setq l (cdr l))
	     (setq denname (cat (setq dentype (car l))
				'\-
				(setq :fun (caddr l))))
	     (setq :isfun (cadr l) l (cdddr l))
	     (setq :drbp (cadr l))
	     (return (list 'progn
			   '(quote compile)
			   (list 'defprop :fun  denname dentype)
			   (list 'defun denname nil (eval (caddr l)))
			   (if* (car l)
			       (list 'defprop
				     :fun
				     (car l)
				     ':lbp)))))) 

(defmacro nilfix (func code) `(deffix :nud isn ,func nil nil ,code)) 

(defmacro prefix (func rbp code) `(deffix :nud isp ,func nil ,rbp ,code)) 

(defmacro suffix (func :lbp code) `(deffix :led iss ,func ,:lbp nil ,code)) 

(defmacro infix (func bp code) `(deffix :led isi ,func ,bp ,bp ,code)) 

(defmacro infixr (func bp code) `(deffix :led isi ,func ,bp ,(1- bp) ,code)) 

(defmacro infixd (func :lbp rbp code) `(deffix :led isi ,func ,:lbp ,rbp ,code)) 
(defmacro infixm (func bp code) `(deffix :led ism ,func ,bp ,bp ,code)) 

(defmacro delim (symbol) `(defprop ,symbol 0. :lbp)) 

(eval-when (compile load eval)
  (defun denfunc (code) 
       (sublis (subst :drbp
		      ':drbp
		      '((right :parse :drbp)
                        (token eattoken)
			(rightlist :parselist :drbp '|,|)))
	       (subst :drbp ':drbp code))))

	  

(defmacro eattoken nil '(prog1 :token (advance)))

(defmacro istoken (tok) `(if* (eq :token ,tok) (advance) t))

(defmacro nottoken (char) `(not (eq :token ,char))) 
; (defmacro kwote (foo) `(list 'quote ,foo)) already in opus 38
(defun qlistfy (foo) (cond ((atom foo) (list 'quote (list foo))) (foo)))
(defmacro name-to-index (name) `(cdr (assq ,name '((1st . 0) (2nd . 1) (3rd . 2)))))
(defun kwote-if-atom (foo) (cond ((atom foo) (list 'quote foo)) (foo)))

(eval-when (load compile eval)
  (defun denfun fexpr (l)
        (cons 'buildfun (denfunc l))))

(eval-when (load compile eval)
  (defun buildfun fexpr (l)
       (do ((pattail l (cdr pattail))
            (result) (optsw) (next))
           ((null pattail) (nreverse result))
           (cond ((eq (setq next (car pattail)) '\]) 
                  (setq optsw nil))
                 ((eq optsw 'ignore)
                  (if* (eq next '\\)
                      (pop pattail)
                      (push (eval (car pattail)) result)))
                 ((eq next '\[) (setq optsw t))
                 ((eq next '$)
		  (pop pattail)
		  (push (eval (car pattail)) result))
                 ((eq next ':left) (push :left result))
                 ((eq next ':token) (push :token result))
                 ((atom next)
		  (cond (optsw (ifnot (istoken next) (setq optsw 'ignore)))
			((check next))))
                 ((push (eval next) result))))))
                                    


(eval-when (load compile eval)
  (defun isf (lispfunc) 
       (if* (atom lispfunc) (setq lispfunc (list 'quote lispfunc)))
       (append (list :isfun lispfunc :drbp)
	       (cond ((eq :isfun 'ism)
		      (list (list 'quote :fun))))))) 

 

(eval-when (load compile eval)
  (defun isn  (fcn rb) (list fcn)))  

(eval-when (load compile eval)
  (defun iss (fcn rb) (list fcn :left))  )

(eval-when (load compile eval)
  (defun isp  (fcn rb) (list fcn (:parse rb))))  

(eval-when (load compile eval)
  (defun isi  (fcn rb) (list fcn :left (:parse rb))))  

(eval-when (load compile eval)
  (defun ism (fcn rb cont) (cons fcn (cons :left (:parselist rb cont)))))  

(defun check (del)
       (cond ((or (and (atom del) (eq :token del)) 
		  (and (not (atom del)) (memq :token del)))
	      (advance))
	     ((warn 0. missing $ del inserted before $ :token))))
	  

(defun :parselist (rb cont)
       (cons (:parse rb)
	     (cond ((eq :token cont) (progn (advance) (:parselist rb cont))))))

(defun getvarlist nil
       (cond ((not (equal :token '\;))
	      (cons (prog2 nil :token (advance))
		    (cond ((equal :token '|,|)
			   (progn (advance) (getvarlist))))))))
;;utility functions
;;utility function for displaying property lists of denotations
(defun den? (x) 
       (say plist of $ x :)
       (dp x)
       (if* (get x ':nud)
	   (say plist of $ (cat ':nud- x))
	   (dp (cat ':nud- x)))
       (if* (get x ':led)
	   (say plist of $ (cat ':led- x))
	   (dp (cat ':led- x))))


(defun glang-to-lisp fexpr (l)
       (apply 'uread l)
       (uwrite)
       (do ((^q t) (^r t) (^w t) (eof (ncons nil)) (temp))
           ((eq (setq temp (read eof)) eof)
            (apply 'ufile (list (car l) 'lisp)))
           (print temp)))

(defun gt fexpr (l)
       (if* (cdr l) (apply 'uread (cdr l)))
       (for (^q t)
            (do ((i (eval (car l)) (1- i)))
		((zerop i))
                (finish-grule)))            
       (for (^q t)(setq :gtest (read '($$$))))
       (cond ((equal :gtest '($$$)) 
              (say |End of test file.|))
             (t (cursorpos 'c)
                (sprinter :gtest))))

(defun finish-grule nil (do ((ch (tyi) (tyi))) ((= ch 175))))


(comment rule form translating denotations)
;;;The following functions are the denotations which define Parsifal's grammar
;;language.

;;Note that the atoms "right" and "rightlist"  are macro-expanded
;;by DENFUNC.

;;;the Rule translator
(prefix rule 1 '(build-rule 'normal))
(infix rule 1 '(build-rule :left))

(defun build-rule (type)
       (for (:rulename (eattoken) pat nil)
            (list 'rule
                  :rulename 
                  (list (prog2 (ifnot (memq type '(nr as normal)) 
                                      (warn 1. $ type not legal type 
                                            for rule $ :rulename))
                               type)
                        (cond ((istoken 'priority:) (eattoken))
                              ((numberp :token) (eattoken))
                              (10.)) ;priority defaults to 10.
                        (prog2 (check 'in)(getvarlist));packets
                        ;;The do returns a list of all pattern features
                        ;;headed by :indexf, and setq's pat to the pattern
                        (do ((:nodevarlist
                              (cond ((eq type 'normal)
				     '((1st 0 ((* . 1st)))
				       (2nd 1 ((* . 2nd)))
				       (3rd 2 ((* . 3rd)))))
				    ('((nth :int-index ((* . nth)))))))
                             (:nodevars) (result) (:indexf) (:pfeats))
			    ((and (nottoken '\[) (nottoken 't))
			     (ifnot result (warn 1. no pattern for $ :rulename ?))
			     (setq pat
                                   (cond ((null (cdr result)) (car result))
                                         ((cons 'and (nreverse result)))))
			     (cons (or :indexf 'noindexf) :pfeats))
			    (setq result (cons (:parse 0) result))))
                  pat;pattern
                  (prog2 (check '(--> --->))
                         (:parse 0)))))

;;;the pattern element builder
(prefix \[ 0 '(build-pattern-element))

(or (boundp ':psych-sw) (setq :psych-sw nil))

(defun build-pattern-element nil
       (prog (starsubst patbody set* :semi-flag)
             (setq :semi-flag 'inpat)
             (cond ((istoken '**)
                    (setq starsubst (ncons (cons '* (eattoken)))
                          :nodevars nil))
                   (t (setq :nodevars (pop :nodevarlist))
                      (ifnot :nodevars
                             (warn 1. pattern of $ :rulename
                                    has unidentified element))
                      (ifnot (cond (:psych-sw (eq (car :nodevars) 'nth))
                                   ((memq (car :nodevars) '(nth 1st))))
			     (setq set*
				   (list 'or
					 (car :nodevars)
					 (list 'set* (cadr :nodevars)))))
                      (setq starsubst (caddr :nodevars))))
             (istoken '\;)
             (setq patbody (:parse 0))
             (check '\])
             (return (sublis starsubst
                             (cond  (set* (list 'and set* patbody))
                                    (patbody))))))

;;Case rule form

(infix crule 0. (denfun 'crule token :left
                        (buildfun token [ over token ])
                        right))

(prefix upper 0. (denfunc '(progn (istoken 'node) 'fnode)))

(prefix lower 0. (denfunc '(progn (istoken 'node) 'snode)))

;;;special IS predicate for use in patterns.

(prefix \= 10 '(build-=))

(defun build-= nil
       (cond (:nodevars
              ;;:nodevars is nil if the surrounding pattern element doesn't 
              ;;refer to the buffer.
              (for (vars (getvarlist))
                   (setq :pfeats (union2 (reverse vars) :pfeats))
                   (if* (memq (car :nodevars) '(1st nth))
                       (setq :indexf (pickindex vars)))
                  (list 'fast-is (cadr :nodevars) vars)))
             (t (list 'is '* (kwote (getvarlist))))))


;;pickindex uses the heuristic that the later features in a pattern are
;; the most obscure.  It picks the last feature that hasn't been used before
;;as an :indexf or else the last feature.

(defun pickindex (flist)
       (do ((fs flist (cdr fs)) (oldfs (get ':gram-stats 'indexfs))
                                (old) (new))
           ((null fs) (consprop ':gram-stats (or new old) 'indexfs)
                      (or new old))
           (cond ((memq (car fs) oldfs) (setq old (car fs)))
                 ((setq new (car fs))))))


(comment grammar function denotations)
;;;Bracketing  operators
;;"( )" as prefix scopes, while a(1,2,3)  translates into LISP as (a 1 2 3)
(prefix |(| 0. (denfunc '(prog1 right (check '|)|))))

(infixd |(| 30. 0.
	(denfunc
	 '(for (temp right)
	   (check '|)|)
	   (cond ((atom temp) (list :left temp))
		 ((eq (car temp) 'list-build) (cons :left (cdr temp)))
		 ((list :left temp))))))

(delim |)|) 
(delim \,)

;;Matching single quotes  quote
(prefix \' 0. (denfun 'quote right |'|))
(delim \') 
 

;;;control operators
;;Note that ; has higher precedence than if, and thus can
;;be used to prognify the action of an if statement.

(infixm \;
	3
	(isf '(cond ((and (boundp ':semi-flag) (eq :semi-flag 'inpat)) 'and) ('progn))))

(infix \.
       1
       (denfunc '(cons 'progn (cons :left (:parselist-unless :drbp '\. '\})))))

(defun :parselist-unless (rb cont stop)
       (cond ((eq :token stop) nil)
             (t (cons (:parse rb)
                      (cond ((istoken cont) (:parselist-unless rb cont stop)))))))

(infixm \, 17. (isf 'list-build))
(defmacro list-build (a &rest b) `(quote (,a ,@b)))

;;IF... THEN... (ELSE ....) (ANDTHEN)
;;	The nearest IF takes an ELSE clause, and ANDTHEN ends the nearest IF.


(prefix if 2 
        (denfunc '(for
                   (ifclause (list 'cond (buildfun right then right))
			     elseclause 
			     (if* (istoken 'else) right))
                   (if* elseclause (setq ifclause
                                        (append ifclause
                                        (cond ((and (not (atom elseclause))
						    (eq (car elseclause)
							'cond))
					       (cdr elseclause))
					      ((ncons (ncons elseclause)))))))
		       (cond ((istoken 'andthen) (list 'prog2 ifclause right))
			     (ifclause)))))

;;;Parser control operations
(prefix deactivate 10. (denfun 'deactivate (kwote (getvarlist))))
(prefix activate 10. (denfun 'activate (kwote (getvarlist))))    
(prefix run 10. (denfun 'setq ':nextrule (kwote (eattoken)) next))
(prefix parse 10. (denfun is finished 'setq ':parsecomplete 't))
(prefix restore 10. (denfun buffer 'bufrestore))

;;;Logic operators
(infixm and 5. (isf 'and))
(infixm or 4. (isf 'or))
(infix isn  6. (denfun \' t true that 'not right))

;;;Operations on nodes.
(nilfix last (denfun ':last))
(prefix attach 10. 
        (denfunc '(for (dn right fn (cond ((istoken 'to) right) 
                                        ('c))
                        funct (buildfun as 'quote (eattoken)))
                       (cond ((eq dn 'c) (list 'attach1 dn fn funct))
                             ((list 'attach fn dn funct))))))

(prefix drop 10. 
        (denfun 'drop c [ into buffer ] [ before (name-to-index token) \\ 0 ]))

(prefix insert 10. 
        (denfun 'insert-node right [ into buffer ] [ before (name-to-index token) \\ 0 ]))


(prefix label 10. (denfun 'addf1 right [ with ]  (qlistfy right)))
(prefix remove 10.
        (denfun [ features ] [ feature ] 'remf1 (qlistfy right) from right))

 (infix is 10.
       (denfunc '(cond ((memq :token '(none not any greater less equal))
			:left)
		       ((buildfun 'is [ labelled ] :left (qlistfy right))))))

(infix not 10.
       (denfun [ any 'is-none-of \\ (progn (istoken 'all) 'is-not-all-of) ]
               [ of ] :left (qlistfy right)))
(infix none 10. (denfun of 'is-none-of :left (qlistfy right)))
(infix any 10. (denfun of 'is-any-of :left (qlistfy right)))




(prefix transfer 10. (denfun 'transfer [ feature ] [ features ] (qlistfy right) from right to right))

(prefix lift 10. (denfun 'liftr (kwote token) register from right to right))
(prefix features 10. (denfun  'fe [ of ] right))
(prefix meet 10. (denfun 'intersectq2 of right and right))
(prefix word 10. (denfun 'wordify \'  (kwote token) \'))
(prefix create 10. (denfun [ new ]  'newnode (kwote token) 
                           node [ labelled (qlistfy right) \\ nil ]))
(prefix new 10. (denfunc
                 '(cond ((istoken 'case) (buildfun frame 'newcf ''normal))
                        ((istoken 'mod) (buildfun case frame 'newcf ''mod))
                        (t (buildfun 'newnode (kwote token) node
                                     [ labelled (qlistfy right) \\ nil ])))))

;;Rob Milne's disambiguation function

;;caseframe stuff
(prefix make 10. (denfun 'disambig-type right (kwote token)))

(prefix case 10. (denfun frame 'getr ''caseframe of right))
(prefix associate 10. (denfun 'associate-cf right with right))
(infix fills 10. (denfun 'fillslot :left (kwote token) slot
                         [ of cf of ] right))
(infix fits 10. (denfun 'fits :left (kwote token) slot
                        [ of cf of ] right))
(prefix finalize 10. (denfun 'finalize-frame cf of right))

;;number of object functions
(prefix lowest 17.
        (denfun 'minunls possible number of objects of right [ is ] ))

(prefix greatest 17.
        (denfun 'maxunls possible number of objects of right [ is ] ))
(prefix number 10. (denfun 'need-slots of objects of right will be right))

(prefix semantics 10. (denfun 'prefer prefers right right better than right))
(infix filling 11. (denfun 'fit-of :left (kwote right) slot of right))
(prefix prepositional 10. (denfun 'pgof phrase of right and right))


;;numeric comparisions
(infix greater 10.(denfun '> :left than right))
(infix less 10. (denfun '< :left than right))
(infix equal 10. (denfun '= :left to right))


;;there function
(prefix there 10.
        (denfunc '(progn (check 'is)
                         (cond ((istoken 'not) (list 'null right))
                               ((list 'setq ':it right))))))


(nilfix it '':it)

;;properties (for the moment) and tree walking functions

;;above now should be obsolete
(infixr above 20. (denfun 'node-above  (kwote-if-atom :left) right))
(prefix node 19. (denfun above 'father-node right))
(infixr of 20. (denfun 'find-node (kwote-if-atom :left) right))
(prefix indirect 19. (denfun object 'io of right))
(nilfix current (denfun 'current-s s))
(nilfix wh-comp (denfun 'wh-comp))
(prefix binding 18. (denfun 'binding of right))
(infix register 18.  (denfun 'getr (kwote-if-atom :left) of right))
(prefix set 9.
        (denfunc '(for (prop (prog1 token (istoken 'register) (check 'of))

			     storeform nil
                        node (prog1  (:parse 1.)  (check 'to))
                        val right)
                       (setq storeform (get prop ':storeform))
                       (cond (storeform (sublis (list (cons 'node node)
						      (cons 'val val))
						storeform))
			     ((list 'setr (kwote prop) val node))))))

(defprop features (setfe node val) :storeform)


;;The rule macro - expands the output of the GLANG parser
(comment the rule expanding macro)
(defun rule macro (x) (:rule x))

(defun :rule (x)
;;;Rules are expected to be of form
;;;(rule <name>
;;;	 (<type> <priority> <packets> (<indexf> . <pattern-features>))
;;;	 patternbody actionbody)
;;;This macro returns
;;;(PROGN 'COMPILE
;;;	  (RULE-INDEX <type> <packets> <indexf>
;;;		      (<priority> <patname> <rulename> <actionname>))
;;;	  (DEFUN patname NIL <patternbody>)
;;;	  (DEFUN actname NIL (prog nil <actionbody>))
;;;       (FINDEXIFY <pattern-features>))
       (prog (name pat-name act-name indexl)
             (setq name (cadr x) indexl (caddr x) x (cdddr x))
             (return 
              (list 'progn
                    '(quote compile)
                    (list 'rule-index
			  (list 'quote (car indexl));type
			  (list 'quote (caddr indexl));packets
			  (list 'quote (car (cadddr indexl)));indexf
			  (list 'quote
				(list (cadr indexl) ;priority
				      (setq pat-name (cat '::pat-of- name))
				      name
				      (setq act-name (cat '::act-of- name)))))
                    (list 'defun pat-name nil (car x))
                    (list 'defun act-name nil (cadr x))
                    (list 'featindexify
			  (if* (cdr (cadddr indexl))
				       (list 'quote
					     (reverse (cdr (cadddr indexl))))))))))

;;the case rule macro.
(defun crule macro (x)
       (for (name (cadr x) funname (cat '::crule-of- (cadr x))
                  type (caddr x) nodes (cadddr x)
                  body (car (cddddr x)))
            (list 'progn
                  ''compile
                  (list 'crule-index
			(list 'quote type)
			(list 'quote
			      (list (cond ((null (cdr nodes)) (car nodes))
					  ((cons (car nodes) (cadr nodes))))
				    funname
				    name)))
                  (list 'defun funname nil body))))

