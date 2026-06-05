(declare
 (special :noharddef :nodef :savenewdefsw :wrd :ordercounter poport
 more
  :newdeffile :cr :comma :space :altmode :rubout :consos :vowels :numbers
  :endpuncs :liquids :puncs :noend :rt :specregs :modfelist :nextmorph :word
  :always-expand :caseorder))
(declare (include declr.l))
;(version)
(setq :nodef nil :noharddef nil :savenewdefsw t) 
(declare (setq ibase 8))

(defun sentin nil 
       (prog (word char :wrd :nextmorph ^w :ordercounter) 
             (declare (fixnum :space :cr :rubout :altmode :comma))
	     (setq :osent :sent :sent nil)
	      (prin1 '>)
	newwrd
             (if* (setq word 
		       (do ((temp word (cdr temp))
			    (temp1 :nextmorph (cdr temp1))
			    (result))
			   ((or (null temp) (null temp1))
			    (nreverse result))
			   (push (car temp) result)))
		 (setq :nextmorph nil)
		 (go word))
	char (cond ((or (= (setq char (tyi t)) :space)
                        (= char :nl))
		    (go word))
                   ;;;rubout rubs out 1 char within word.  
                   ;;;After word break, rubs out a word
		   ((= char :rubout)
		    (cond (word (cursorpos 'x)
				(setq word (cdr word)))
			  (:sent (setq :sent (cdr :sent))
				(cursorpos (car (cursorpos)) 0.)
				(cursorpos '\])
				(cursorpos 'u)
                                (say-sent nil))))
                   ;;;$c--restart input.  $s--repeat last s. 
                   ;;;$<rubout>--reprint current input
		   ((= char :altmode)
		    (cond((eq (lowcaseify (setq char (tyi t))) 'c)
                          (setq :sent nil word nil)
                          (go clean))
			  ((eq (lowcaseify char) 's)
			   (setq :sent :osent)
                           (mapc 'print2 (origcase :sent))
                           (terpri)
                           (setq :ordercounter 0)
                           (return (nodify* :sent)))
			  ((= char :rubout) (go clean))
			  ((princ '?) (go clean))))
                   ;;;flush commas if preceded and followed by number.
                   ((and (= char :comma)
			 word
                         (numberp (readlist word))
                         (memq (ascii (tyipeek nil t)) :numbers)))
                   ;;;Hack for PA.  Breaks up 2:30 into 2 : 30
                   ((and (eq (ascii char) ':)
                         (numberp (readlist word))
                         (memq (ascii (tyipeek nil t)) :numbers))
                    (setq word (cons char word))
                    (go word))
		   ((setq word (cons char word))))
	     (go char)
	clean(say-sent word)
	     (go char)
	word (cond ((null word) (go char))
		   ((not (morpho word))
		    (cond ((memq :wrd :wstring-list))
			  ((define? word) (go word))
			  (t (setq word nil) (go clean)))))
	     (setq :sent (cons :wrd :sent))
             (putprop :wrd
                      (do ((w word (cdr w)) (i (length :nextmorph) (1- i)))
			  ((zerop i)  (maknam (reverse w))))
                      'origword)
             (cond ((if* (memq :wrd :endpuncs)
			(setq :sent (nreverse :sent))
			(setq :ordercounter 0)
			(return (nodify* :sent))))
		   ((go newwrd)))))

             

 

(defun morpho (:word) 
       (prog (:rt first secnd third :modfelist) 
             (setq :word (mapcar 'lowcaseify :word))
             (setq :rt :word)
             (cond ((expandsim (setq :wrd (implode (reverse :word)))) (return t))
                    ((strip-if-any '((n \' t) (\' s) (\' d) (\' m)
                                     (\' l l) (\') (\,) (\.) (\?) (\!)
                                     (\:) (\;) (\-)))
                     (if* (expandsim (setq :wrd (implode (reverse :word))))
                         (return t))))
	     (setq :modfelist '(nil nil))
	     (cond
	      ((ends-in '(s))
	       (modfe '(npl pres  v3s) '(ns tnsless v-3s))
	       (cond ((try) (return t)) ((go esend))))
	      ((ends-in '(l y))  (go lyend))
	      ((ends-in '(i n g))
	       (modfe '(ing part adj) '(tnsless past pres)))
	      ((ends-in '(e d))
	       (modfe '(past ed) '(tnsless pres)))
	      ((ends-in '(e n))
	       (modfe '(en part adj) '(tnsless past pres)))
	      ((ends-in '(e r))
	       (modfe '(comp) nil))
	      ((ends-in '(e s t))
	       (modfe '(supr) nil))
              ((and (memq (caddr :rt) :numbers)
		    (or (ends-in '(s t))
			(ends-in '(n d))
			(ends-in '(r d))
			(ends-in '(t h))))
	       (modfe '(ord) nil)))
	     (and (try) (return t))
	     (setq first (car :rt) secnd (cadr :rt))
	     (cond ((memq first :vowels)
		    (cond ((eq first 'i)
			   (setq :rt (cons 'y (cdr :rt)))
			   (go test))
			  ((eq first 'y) (go test))
			  ((not (eq first 'e)) (go adde))
			  ((eq secnd 'e) (go test))
			  ((cond ((try) (return t)) ((go adde))))))
		   ((eq first 'h)
		    (or (eq secnd 't) (go test))
		    (cond ((try) (return t)) ((go adde))))
		   ((eq first secnd)
		    (and (memq first :liquids) (and (try) (return t)))
		    (setq :rt (cdr :rt))
		    (go test))
		   ((memq secnd :vowels)
		    (or (memq (cdddr :rt) :vowels) (go adde))
		    (and (memq first :noend) (go adde))
		    (go test))
		   ((memq first :liquids)
		    (and (eq first 'l)
			 (eq secnd 'r)
			 (go test))
		    (go adde))
		   ((memq first :noend) (go adde))
		   ((go test)))
	adde (setq :rt (cons 'e :rt))
	     (go test)
	esend(setq first (car :rt) secnd (cadr :rt) third (caddr :rt))
	     (or (eq first 'e) (go test))
	     (cond ((eq secnd 'i)
		    (setq :rt (cons 'y (cddr :rt)))
		    (go test))
		   ((eq secnd 'h)
		    (or (eq third 't) (go ecut))
		    (go test))
		   ((eq secnd 'x) (go ecut))
		   ((memq secnd '(s z))
		    (and (eq third secnd) (go ecut))
		    (go test))
		   ((eq secnd 'v)
		    (setq :rt (cons 'f (cddr :rt)))
		    (and (memq (cadr :rt) :vowels)
			 (not (memq (caddr :rt) :vowels))
			 (setq :rt (cons 'e :rt)))
		    (go test))
		   ((return nil)))
	ecut (setq :rt (cdr :rt))
	     (go test)
	lyend(modfe '(adv manner) '(adj ns npl ngstart))
	     (and (eq (car :rt) 'i)
		  (setq :rt (cons 'y (cdr :rt)))
		  (go test))
	     (and (try) (return t))
	     (setq :rt (append '(e l) :rt))
	     (go test)
	test (and (try) (return t))))

(defun define? (word)
       (prog (temp)
             (say |Sorry, I don't know the word|
		  $ :wrd |.|)
             (drain piport)
	     (if* :nodef (return nil))
	againjl
	     (say |Tell me a word like|
		  $ :wrd
		  |(or type n if none): |)
	     (cond ((memq (setq temp (read)) '(N no NO n No)))
		   ((not (expandsim temp))
		    (say |Oops! I don't know that one either.|)
		    (go againjl))
		   (t (apply 'jlike (list :wrd temp))
		      (if* :savenewdefsw
                          (errset
			   (let ((more nil) (poport nil)
				 (defport (outfile :newdeffile 'append)))
				(setq poport defport)
				(terpri)
				(print (list 'jlike :wrd temp))
				(close defport))))
		      (say-sent word)
		      (return t)))
	     (return nil)  ;for now
;;;	     (if* :nohardDEF  (return nil))
;;;	     (SAY |Define| $ :wrd |? Y or N -|)
;;;	     (COND ((eq (READ) 'Y)
;;;		    (break |For defining| T)
;;;		    (SAY |Thanks - Go on-|)
;;;		    (SAY-SENT word)
;;;		    (return t))
;;;		   (T  (return nil)))
             ))

(defun origcase (stuff)
       (cond ((atom stuff) (get stuff 'origword))
             (t (mapcar 'origcase stuff))))

(defun lowcaseify (char)
       ;(declare (fixnum char))
       (ascii (cond ((greaterp 91. char 64.) (+ char 32.))
                    (char))))

(defun ends-in (l)
       (for (temp (sta l)) (if* temp (setq :rt (cdr temp)))))


(defun strip-if-any (l)
       (do ((nxt (car l) (car tail))
            (tail (cdr l) (cdr tail)))
           ((null tail) nil)
           (if* (strip-if nxt) (return t))))

(defun strip-if (l)
       (for (temp (sta l))
	    (if* temp
		(setq :nextmorph (reverse (car temp)))
		(setq :rt (setq :word (cdr temp))))))

                           
            

(ifnot (boundp ':wstring-tree)
       (setq :wstring-list nil)
       (setq :wstring-tree (ncons nil)))

(defun get-string (string tree)
       (cond ((null tree) nil)
	     ((get-string (cdr string) (get tree (car string))))
	     ((car tree) (cons (car tree) string))))

(defun put-string (string tree word)
       (for (temp nil)
	    (cond ((null string) (rplaca tree word))
		  ((setq temp
			 (or (get tree (car string))
			     (putprop tree (ncons nil) (car string))))
		   (put-string (cdr string) temp word)))))


(defun df+ fexpr (l)
       (prog (wlist canonword wlist1)
	     (setq wlist (mapcar 'lowcaseify (exploden (car l))))
	     (setq canonword (implode wlist))
	     (setq wlist1 (readlist (append '(|(|) wlist '(|)|))))
	     (put-string wlist1 :wstring-tree canonword)
	     (setq :wstring-list (union1 wlist1 :wstring-list))
	     (putprop canonword canonword 'origword)
	     (apply 'df (cons canonword (cdr l)))))


(defun say-sent (word) 
       (prints '>)
       (mapc '(lambda (x) (print2 x)) (reverse (origcase :sent)))
       (princ '\ )
       (if* word (princ (maknam (reverse word)))))
 

(defun wordify (word)
       (putprop word word 'origword)
        (expandsim word)
        (for (:ordercounter 1000) (nodify word)))

(defun nodify (word) 
       (prog (node) 
	     (setplist (setq node (makesym 'word))
		       (list 'daughters
			     'word
			     'word
			     word
                             'inputpos
                             (setq :ordercounter (1+ :ordercounter))
                             'markers
                             (get word 'markers)))
             (set node (get word 'features))
             (mapc '(lambda (regtype)
			    (for (temp (get word regtype))
				 (if* temp (putprop node temp regtype))))
		   :specregs)
	     (return (cons node 0))))

(defun nodify* (wstring)
       (do ((wtail wstring (cdr wtail))
	    (result nil (cons (car wtail) result)) (temp))
	   ((null wtail) (mapcar 'nodify (nreverse result)))
	   (if* (setq temp (get-string wtail :wstring-tree))
	       (expandsim (car temp))
	       (setq wtail temp)))) 

(defun sta (l) 
       ;;note that sta returns nil if the root is the same length as the ending
       (do ((rtail :rt (cdr rtail))
            (ltail (reverse l) (cdr ltail))
            (tail nil (push (car ltail) tail)))
	   ((null ltail) (if* rtail (cons tail rtail)))
	   (ifnot (eq (car rtail) (car ltail)) (return nil))))

(defun modfe (a b) 
       (setq :modfelist (list (union1 (car :modfelist) a)
			     (union1 (cadr :modfelist) b))))

(defun buildword (word feat markers root) 
       (expandsim root)
       (putprop word feat 'features)
       (putprop word markers 'markers)
       (putprop word root 'root)
       (mapc '(lambda (regtype)
			    (for (temp (get root regtype))
				 (if* temp (putprop word temp regtype))))
		   :specregs)) 

(defun buildnumber (numword number) 
       (declare (fixnum number))
       (buildword numword
		  (cons (cond ((< number 10.) 'ones)
                              ((zerop (\\ number 10.)) 'tens)
                              ((= number 100.) '*hundred)
                              ((< number 100.) '99s)
                              ((< number 1000.) '999s))
                        (cons (cond ((= number 1.) 'ns)
			      ('npl))
			(add-redunds '(1234num num adj))))
		  nil
		  nil)
       (putprop numword number 'quant)) 

(defun buildirregword (word) 
       (prog (irregl root fe) 
	     (setq irregl (get word 'irreg))
	     (expandsim (setq root (car irregl)))
	     (buildword word
			(setq fe (mod (get root 'features)
				      (cdr irregl)))
			(get root 'markers)
			root)
	     (return word))) 

(defun df fexpr  (l) 
       (do ((word (car l)) (plist (cdr l) (cddr plist)))
	   ((null plist) word)
	   (putprop word (cadr plist) (car plist))))
(defun df1 fexpr (l) nil) 
 

(defun jlike fexpr (l) 
       (prog (nword) 
	     (cond
	      ((atom (car l))
	       (putprop (setq nword (car l)) (cadr l) 'jlike))
	      (t (putprop (setq nword (caar l))
			  (caadr l)
			  'jlike)
		 (mapc '(lambda (newirreg oirreg) 
				(putprop newirreg
					 (cons (caar l)
					       (cdr (get oirreg
							 'irreg)))
					 'irreg))
		       (cdar l)
		       (cdadr l))))
	     (if* (cddr l) (putprop nword (cddr l) 'except))
	     (return nword))) 



(defun expanddef (word)
       (prog (temp)
             (if* (and (get word 'features)
		      (not (get word 'feats)))
		 (return t))
	     (if* (setq temp (get word 'markers))
		 (putprop word (add-redunds (expandm temp)) 'markers))
             (if* (setq temp (get word 'feats))
                 (for (features
                       (cons (implode (cons '* (explodec word)))
			     (add-redunds (expandm temp))))
		      (putprop word features 'features)
                      (for (poses (meet1 features :parts-of-speech))
                           (ifnot poses
				  (if* :carefulsw
                                      (warn 0. $ word
                                            |is not a known part of speech.|)))
                           (putprop word (car poses) 'type))))
             (if* (setq temp (get word 'cf))
                       (putprop word (expandcf (expandm temp)) 'case-frame))
             (remprop word 'feats)
             (remprop word 'cf)
             (if* (setq temp (get word 'neut))
		 (putprop word (expandm temp) 'neut))
             (if* (setq temp (get word 'preps))
                 (putprop word (cons nil temp) 'preps))))


(defun expandsim (word) 
       (prog (simw temp)
	     (cond ((and :always-expand
			 (or (and (setq simw (get word 'jlike)) (expandsim simw))
			     (and (get word 'irreg) 
				  (buildirregword word) 
				  (return t))
			     (and (get word 'root) (return nil)))))
		   ((get word 'feats) (expanddef word) (return t))
                   ((get word 'features) (return t))
		   ((get word 'irreg)
		    (buildirregword word)
		    (return t))
                   ((and (setq simw (get word 'jlike))
			  (expandsim simw)))
                   ((and (memq (getchar word (flatc word)) :numbers)
			 (setq temp 
			       (for (ibase 10.) (readlist (explodec word))))
			 (numberp temp)
			 (buildnumber word temp)
			 (return t)))
		     ((return nil)))
             (putprop word t 'expanded)
	     (putprop word
		      (cons (implode (cons '* (explodec word)))
			    (setminus (get simw 'features)
				      (ncons (implode (cons '* (explodec simw))))))
		      'features)
	     (putprop word (get simw 'markers) 'markers)
             (mapc '(lambda (regtype)
			    (for (temp (get simw regtype))
				 (if* temp (putprop word temp regtype))))
		   :specregs)
	     (do
	      ((plist (get word 'except) (cddr plist))
	       (prop)(value))
	      ((null plist))
	      (cond
	       ((eq (setq prop (car plist)) 'feats)
		(putprop word
			 (cond ((eq (cadr (setq value (cadr plist)))
				    'all)
				(car value))
			       ((setminus (union (car value)
						 (get word
						      'features))
					  (cadr value))))
			 'features))
             ((eq prop 'cf)
	      (putprop word
		       (expandcf (expandm (cadr plist)))
		       'case-frame))
	       ((eq prop 'cases)
		(putprop word
			 (modcasef (get simw 'case-frame)
				   (car (setq value (cadr plist)))
				   (cadr value))
			 'case-frame))
	       ((putprop word (cadr plist) prop))))
	     (return t))) 

(defun modcasef (basecf addcs remcs) 
       (prog (result) 
	     (mapc 
	      '(lambda (case) 
		       (do nil
			   ((after (caar addcs) (car case) :caseorder))
			   (setq result (cons (car addcs) result) 
				 addcs (cdr addcs)))
		       (or (memq (car case) remcs)
			   (setq result (cons case result))))
	      basecf)
	     (return (append (nreverse result) addcs)))) 

(defun after (case1 case2 standard) 
       (not (memq case2 (memq case1 standard)))) 


(defun redund fexpr (l)
       (do ((rtail (listfy (cadr l)) (cdr rtail)))
                    ((null rtail) (car l))
                    (putprop (car rtail)
			     (union1 (ncons (car l))
				     (get (car rtail) ':redunds))
			     ':redunds)))

(defun listfy (a) (cond ((atom a) (ncons a)) (a)))

 

(defun add-redunds (feats)
       (do ((fset feats)
            (newfs feats
                   (do ((fs newfs (cdr fs)) (result))
                       ((null fs) result)
                       (mapc
                        '(lambda (f)
                           (ifnot (memq f fset)
				  (push f result)
				  (push f fset)))
                         (get (car fs) ':redunds)))))
           ((null newfs) fset)))

(defun expandcf (cf)
       (mapcar '(lambda (case)
                        (cond ((null case) case)
                              ((atom case) (list case 'oblig))
                              ((null (cdr case)) (list (car case) 'opt))
                              (case)))
               cf))

(defun expandm (prop)
       (cond ((and (atom prop) (get prop ':abbrev)))
             (prop)))
(defun abbrev fexpr (l) (putprop (car l) (cadr l) ':abbrev))

                        
                                      
(defun mod (a b) 
       (if* (and (memq 'ed=en a) (memq 'ed (car b)))
	      (setq a (append a '(en part))))
       (union (setminus a (cadr b)) (car b))) 

(defun try nil 
       (prog (x root features)
	     (expandsim (setq root (implode (reverse :rt))))
	     (cond ((setq features
			  (or (get root 'features)
			      (and (setq x (get root 'irreg))
				   (setq root (car x))
				   (mod (get 'features x)
					(cdr x)))))
		    (buildword :wrd
			       (mod features :modfelist)
			       (get root 'markers)
			       root))
		   ((return nil)))
	     (return t))) 

(setq :nl 10.)
(setq :comma 44.) 
(setq :space 32.)
(setq :altmode 27. :rubout 127.) 

(setq :consos '(b c d f g h j k l m n p q r s t v w x z)) 

(setq :vowels '(a e i o u y)) 

(setq :numbers '(\0 \1 \2 \3 \4 \5 \6 \7 \8 \9)) 

(setq :endpuncs '(\. ? !)) 

(setq :liquids '(l r s z v)) 

(setq :puncs '(\. \, ? : \; ! -)) 

(setq :noend '(c g s j v z)) 

(defun remnodes (num) 
       (do i 1. (1+ i) (> i num) (remnode i))
       (or (> (get 'node 'gennum) num)
	   (putprop 'node 0. 'gennum))) 

(defun remnode (num) 
       (prog (node) 
	     (reclaim (plist (setq node
				   (implode (append '(n o d e)
						     (explodec num)))))
		      nil)
	     (makunbound node)
	     (remob node))) 

