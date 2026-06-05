(declare (special nindnt nsw fsw casesw funcsw  scroll wipeclean 
		  dividescreen linel line-count user-top-level 
		  poport piport 
		  ntab ftab make-film? super-film?))

(declare (include declr))
;(version)
;;Parser interface and utility functions - none of which are necessary to
;run the parser
                  
(defun parsifal nil 
       (prog ;;Nothing will be local here later
              (hypoth-frames certain-frame
	      also-filled openframe) 
	     (ifnot (boundp '###started)
		    (setq ###started nil)
		    (say Parsifal Version 1.0))
             (do ((ctemp ))		
                 ;;do forever
                 nil
		 (cond  ((and (not scroll) (not wipeclean))
			 (cursorpos '\])
			 (cond (dividescreen
				(cursorpos 'd)
				(do ((i 1 (1+ i)))
				    ((= i linel))
				    (princ '|_|))
				(cursorpos 'u)
				(tyo 13.)))))
			(patom "# ")
			(drain)
			(and scroll
			     (setq line-count 1))
		 (setq ctemp (read))
		 (newline-stuff)
                 (cond ((eq ctemp 'p)(parse))
		       ((eq ctemp 'tree) (ptree))
		       ((eq ctemp 'ctree) (ptree s 0. t) )
		       ((eq ctemp 'ftree) (ptree s 0. nil t nil nil) )
		       ((eq ctemp 'ntree) (ptree s 0. nil nil t) )
		       ((eq ctemp 'ltree) (ptree s 0. nil t t) )
		       ((eq ctemp 'cases) (short-ctree1 s))
		       ((eq ctemp 'time) (parsetime))
                       ((eq ctemp '?) (introduce-parsifal))
		       ((eq ctemp 'lisp) (setq user-top-level nil)
			(return nil))
		       (t (prin1 (eval ctemp))
			  (terpri))))))

(defun introduce-parsifal nil
       (process |cat /usr/mitch/lisp/parsifal/parsifal.help|))


(defun :ptoplevel nil (sstatus toplevel '(parsifal)))

(defun runfast nil 
       (setq :ptrace nil 
	     :ntrace nil 
             ctrace nil
             :crtrace nil
             :pstatus nil
             word-entry-switch nil
	     :carefulsw nil)
       (*rset nil)
       (sstatus translink on)) 

 (defun ptrace nil 
       (setq :ptrace t
             :ntrace t :pstatus nil)
       (*rset t))

(defun ctrace (foo)(setq :crtrace (setq ctrace foo)))

(defun trace-on fexpr (l)
       (if* (apply 'ruletrap l)
	   (setq :displayflag nil)
	   (setq rule-func '(and top-tty (setq :displayflag t)))
	   t))

(defun trace-off nil (setq :displayflag nil))

(defun deriv nil
       (cursorpos 'c)
       (for (base 10.)
	    (say |Total rules executed:| $ (length :deriv))
	    (terpri))
       (do  ((l (reverse :deriv) (cdr l))
	     (sw t (not sw)))
	    ((null l))
	    (princ (car l))
	    (princ '-->)
	    (cond ((or sw  ttysw) (terpri))
		  ((cursorpos (car (cursorpos)) 25.)))))


(defun switches nil 
       (say |The following switches have been set:|)
       (mapc '(lambda (sw) (say-it  '|     | sw (eval sw)))
             '(:ptrace :ntrace :crtrace ctrace :pstatus *rset
                      nouuo :buffer-gc :carefulsw :always-expand
                      :breaksw :ruletrap :word-entry-switch
                      :displayflag :psych-sw))t)

(defun pstate nil
       (cond ((boundp ':activenodestak)
	      (nstak? 100.)
	      (terpri)
	      (bprint)
	      (terpri)
	      (wstring)
	      (terpri))
	     ((say |Not parsing....\|))))

(declare (*lexpr say-it1))

(defun say1 macro (l)
       (rplaca l 'say-it1)
       (do ((saylist) (spliceflag) (saytemp (cdr l) (cdr saytemp)))
	   ((null saytemp) (rplacd l (reverse saylist)))
	   (cond ((memq (car saytemp) '($$ $))
                  (cond ((eq (car saytemp) '$$)
                         (setq saylist (cons ''$$$splice$$$ saylist))))
		  (setq saytemp (cdr saytemp))
		  (setq saylist (cons (car saytemp) saylist)))
		 (t (setq saylist (cons (list 'quote (car saytemp)) saylist))))))


(defun say-it1 saylist
       (terpri)
       (do ((n  1 (1+ n)) (splice))
           ((> n saylist))
           (cond ((eq (arg n) '$$$splice$$$) (setq splice t))
                 (splice (setq splice nil) (mapc 'print2 (arg n)))
		 ((eq (arg n) '$$up) (cursorpos 'u))
                 ((print2 (arg n))))))

(defun parser-underscore (switch)
  (cond ((not (memq (getenv 'TERM) '(|2621| |2624| hp2621 2621a)))nil)
	((eq switch 'highlight)
	 (ncons (implode '(27. & d B))))
	((eq switch 'HIGHLIGHT)
	 (ncons (implode '(27. & d  C))))
	(switch (ncons (implode '(27. & d D))))
	((ncons (implode '(27. & d @))))))

(defun bprint nil
       (for (bmax+1 (1+ :bufmax) prinlength 5)
	    (say1 |	|
		  $$ (parser-underscore 'highlight)
		  |The Buffer|  
		  $$ (parser-underscore nil) 
		  |  (Pointer stack: | 
		  $ (mapcar '1+  (cons :bufpntr :bufpntrstak)) \))
            (do ((i 0 (1+ i)))
		((> i :bufmax))
		;(if* (= i :bufpntr) (prints '|	=====|))
		(say1 $ (1+ i)
		      $ (cond ((plusp (logand (flags (:buffer i)) 1)) '*)
			      (':))
		      |	| 
		      $$(cond ((= (1+ i) :bufpntr)
			       (parser-underscore t)))
		      $$ (nid (:buffer i))
		      $$ (cond ((= (1- i) :bufpntr)
				(parser-underscore nil)))))
	    (if* (:buffer bmax+1)
		(prints '| (There 's something in position $ bmax+1) |)
		(node-id (:buffer bmax+1)))))

(defun nstak? (depth)
       (for (base 10.)
	    (say1 $$up
		  |	|
		  $$(parser-underscore 'highlight)
		  |The Active Node Stack|
		  $$(parser-underscore nil)
		  |(| 
		  $ (length :activenodestak) |deep) |
		  $$ (cond (:showrule 
			    (list 
			     '|Next rule:|
			     (car (parser-underscore 'HIGHLIGHT))
			     (car :activerule)
			    (car (parser-underscore nil))
			     ))
			   (nil))))
       (if* (< depth (length :activenodestak)) (say |		.......|))
       (do ((stail (reverse (firstn depth :activenodestak)) (cdr stail)))
           ((null stail))
           (say1 |	| $$ (nid1 (caar stail)) \\ $ (cdar stail))
           (printkids (caar stail) (or (caadr stail) c)))
       (say1 |C:	| $$ (nid1	 c) \\ $ :activepackets)
                  (printkids c nil))



(defun wstring nil
       (princ '|Yet unseen words: |)
       (prphrase (nodes-to-words :wstring)))

(defun printkids (node activekid)
       (do ((sons-tail (cdr (getr 'daughters node)) (cddr sons-tail)))
	   ((null sons-tail) t)
	   (mapc
             '(lambda (dn)
                      (say |		| $ (car sons-tail) :
                           $ (cond ((eq dn activekid)
                                    (cond (make-film?  '||) ('\|)))
				   ((is-any-of dn '(trace *))
				    (cons '|trace of: | (phrasify (binding dn))))
                                   ((phrasify dn)))))
             (reverse (cadr sons-tail)))))

(defun stree (node) 
       (cursorpos 'c)
       (setq nsw t)
       (tree node 0.)
       (setq nsw nil)) 

(defun defeat fexpr (l) (remprop (car l) 'features)) 

(defun prule fexpr (l)
       (sprinter (get (car l) ':indexinfo))
       (funcall 'pp (ncons (cat '::pat-of- (car l))))
       (funcall 'pp (ncons (cat '::act-of- (car l)))))

(defun findex (num)
       (do ((i 0 (1+ i))
            (flist (reverse (get ':gram-stats 'flist)) (cdr flist)))
           ((null flist) (say |You lose!!|))
           (if* (= i num) (cond ((= (get (car flist) ':findex) num)
                                (return (car flist)))
                               (t (say flist thinks $ (car flist) but it has
                                       index $ (get (car flist) ':findex)))))))

(defun trace-in macro (l)
       (rplaca l 'trace)
       (rplacd l
	       (ncons (list (cadr l)
			    'wherein
			    (cat '::act-of- (caddr l))
			    'break
			    (cadddr l)))))

(defun psuspend (sname)
       (progn (remedit) (makunbound '###started)
              (suspend (cat '|:pdump | (status userid)
                            '|;ts | sname
                            '|\
|))
              (^g)))

(comment pbug nil
       (prog (^r ^w)
	     (setq ^r t ^w t)
	     (cond ((uprobe pbug mail dsk mitch)
		    (uappend pbug mail dsk mitch))
		   ((uwrite dsk mitch)))
	     (terpri)
	     (date)
	     (prphrase :sent)
	     (ufile pbug mail dsk mitch)))

(comment cando nil
       (prog (^r ^w)
	     (setq ^r t ^w t)
	     (cond ((uprobe parser wins dsk mitch)
		    (uappend parser wins dsk mitch))
		   ((uwrite dsk mitch)))
	     (terpri)
	     (date)
	     (prphrase (phrasify s))
	     (ufile parser wins dsk mitch)
	     (cando-long)))


(declare (special ttysw))
(comment cando-long nil
       (prog (^r ttysw)
	     (setq ^r t ttysw t)
	     (cond ((uprobe parser succes dsk mitch)
		    (uappend parser succes dsk mitch))
		   ((uwrite dsk mitch)))
	     (terpri)
	     (date)
	     (prphrase (phrasify s))
	     (short-ctree1 s)
	     (ufile parser succes dsk mitch)))

(defun date nil
       (for (base 10. *nopoint t date (status date))
	    (print2 (cat (cadr date) '\\ (caddr date) '\\ (car date) '\:))))


(defun sent-list nil
       (terpri)
       (mapc 'print2 :sent-list) t)

 
(setq tree? nil)

(defun test argnum
       (prog (n file)
             (setq n (arg 1))
             (setq file (cond ((eq argnum 2) (arg 2))
                              ( '(test > dsk mitch))))
             (apply 'uread file)
             (say |From file| $ file)
	     (runfast)
	     (nouuo nil)
	     (nexts n)
	     (setq testnum n)
	loop ((lambda (^q) (parse)) t)
	     (setq testnum (1+ testnum))
             (if* tree? (ptree s 0 t) (say $ :sent) (tyi))
	     (say |Sentence: | $ :sent)
	     (parsetime)
	     (go loop)))

  

(defun ptree n 
       (prog (casesw funcsw nsw fsw)
             (if* openframe (closeframe openframe))
	     (cursorpos 'c)
	     (setq casesw (default-arg 3. nil) 
		   funcsw (default-arg 4. nil) 
		   nsw (default-arg 5. nil)
                   fsw (default-arg 6. t))
	     (tree (default-arg 1. s) (default-arg 2. 0.)))) 


(defun parsetime nil 
       (prog (base)
	     (setq base 10.)
	     (say parsing time: $
		  (quotient (setq :ttemp (\\ (- :ptime :gctime) 1000.))
			    1000.0)
		  sec\.)
	     (or (zerop :gctime)
		 (say gctime: $ (quotient (\\ :gctime 1000.) 1000.0)
		      sec\.))
	     (say |          | in $
		  (quotient (fix (*$ :realtime 1000.0)) 1000.0) sec\.
		  realtime)
	     (say time\\word: $
		  (quotient (\\ :ttemp (length :sent)) 1000.0) sec\.)
	     (say time\\rule: $
		  (quotient (\\ :ttemp (length :deriv)) 1000.0) sec\.
		  for $ (length :deriv) rules executed\.)
	     (and :ptrace
		  (say |     | with tracing on))
	     (and *rset
		  (say |     | with *rset on))
	     (ifnot (status translink) (say |     | 
					    with (status translink nil)))
	     (return t)))

(defun time-per-word nil 
       (parsetime)
       (terpri)
       (terpri)
       (do ((nword :savewstring (cdr nword)) (tpwtemp) (base 10.))
           ((null nword)
            (say |One # = |
		 $
		 (quotient (\\ (fix (quotient (quotient (- :ptime :gctime)
							(length :sent))
					      25.))
			       100.)
			   10.0)
		 |msec.| )
            (terpri))
           (terpri)
           (princ (getr 'word (car nword)))
           (cursorpos (car (cursorpos)) 10.)
           (princ (quotient (\\ (setq tpwtemp
				      (-(or (getr ':time (cadr nword)) :endtime)
					 (getr ':time (car nword))))
				1000.)
			    1000.0))
           (cursorpos (car (cursorpos)) 20.)
           (do ((i 0 (1+ i)))
               ((= i
		   (fix (quotient (* tpwtemp 25.)
				  (quotient (- :ptime :gctime) (length :sent))))))
               (princ '#))))


;;tree printing functions
(defun tprint (indent list) 
       (cond (ttysw (space indent))
	     ((cursorpos (car (cursorpos)) indent)))
       (do ((fset (progn (princ '\() (princ (car list)) (cdr list)) (cdr fset))) 
           ((null fset) (princ '\)) (terpri-hack))
           (if* (> (+ (flatsize (car fset)) 3.)
		     (- (linel t)
			(cond (ttysw (chrct t)) ((cdr (cursorpos))))))
		  (terpri-hack)
		  (space indent))
           (princ '\ )
           (princ (car fset))))
 
(putd 'chrct (getd 'charcnt))
(defun spacegen (n) (do i 0. (1+ i) (> i n) (princ '\ ))) 



(defun space (indent) 
       (prog (pos)
             (setq pos (- (linel t) (chrct t)))
tryagin      (cond (ttysw (spacegen  (- indent pos)))
		   (t (cond ((> (cdr (cursorpos))  (+ pos indent))
			     (terpri-hack)
			     (go tryagin))
			    ((cursorpos (car (cursorpos))
					(+ pos indent))))))))

(setq nsw nil casesw nil funcsw nil fsw t ttysw nil)

(defun tree (node level) 
       (prog (nindnt findnt) 
	     (setq findnt
		   (+ (setq nindnt (* level (+ ntab ftab))) ftab))
	     (and nsw (princ (car node)) (space 3.))
	     (cond (fsw (tprint (+ nindnt 5.) (fe node)))
                   ((not (atom (getr 'daughters node))) (terpri)))
	     (cond
	      ((is-any-of node '(trace *))
               (tprint (+ nindnt 8.) (phrasify (binding node))))
              ((and casesw (getc 'pred node))
               (space findnt)
               (princ '|PRED :    |) 
               (princ (getc 'pred node))
               (terpri-hack)
               (space findnt)
               (ifnot (atom (cadr (getc 'spec node)))
		   (princ '|SPEC :|) (tree (cadr (getc 'spec node)) (1+ level)))
	       (mapc 
		'(lambda (case) 
			 (space findnt)
			 (princ (car case))
			 (princ '|:	MARKER:  |)
                         (princ (caddr case))
                         (terpri-hack)
			 (cond  ((dominates? (cadr case) node)
                                (circ-print (cadr case)))
			       (t (tree (cadr case) (1+ level)))))
                 		(reverse (getc 'cases node)))
               (if* (getc 'mods node)
                     (space findnt)
                     (princ '|MODS:	|)
                     (mapc '(lambda (mod) (tree mod (1+ level))) (getc 'mods node))))
              ((eq (getr 'daughters node) 'word)
               (cond (fsw (space (+ nindnt 8.)))
                     (t (space 2.)))
	       (princ (origcase (getr 'word node)))
	       (terpri-hack))
              ((do ((sons-tail (cdr (getr 'daughters node)) (cddr sons-tail)))
                   ((null sons-tail) t)
			 (space findnt)
			 (and funcsw
			      (princ (car sons-tail))
			      (princ ':)
			      (and nsw (terpri-hack)))
			 (mapc 
			  (function (lambda (dn) 
					    (tree dn (1+ level))))
			  (reverse (cadr sons-tail))))))) )



(defun short-ctree1 (node) (cursorpos 'c)(short-ctree node 0))

(defun short-ctree (node level) 
       (prog (nindnt findnt) 
	     (setq findnt
		   (+ (setq nindnt (* level (+ ntab ftab))) ftab))
	     (cond
	      ((is-any-of node '(trace *))
               (tprint (+ nindnt 8.) (phrasify (binding node))))
              ((getc 'pred node)
	       (terpri-hack)
               (space findnt)
               (princ '|PRED :    |) 
               (princ (getc 'pred node))
               (terpri-hack)
	       (mapc 
		'(lambda (case) 
			 (space findnt)
			 (princ (car case))
			 (princ '|:   (MARKER:  |)
                         (princ (caddr case))
			 (princ '\))
                         (terpri-hack)
			 (cond  ((dominates? (cadr case) node)
                                (circ-print (cadr case)))
			       (t (short-ctree (cadr case) (1+ level)))))
                 		(getc 'cases node))
               (if* (getc 'mods node)
                     (space findnt)
                     (princ '|MODS:	|)
                     (mapc '(lambda (mod) (short-ctree mod (1+ level))) (getc 'mods node))))
              ((eq (getr 'daughters node) 'word)
	       (space 2.)
	       (princ (origcase (getr 'word node)))
	       (terpri-hack))
	      (t (space (+ 8. nindnt)) (prphrase (phrasify node)) (terpri-hack)))))



;;this hack is to avoid strange lossages due to funny IO problems in LISP
;;interacting with cursorposes immediately following terpris
(defun terpri-hack nil (terpri) (tyo 32.))

(defun dominates? (node1 node2)
       (do ((fn (getr 'father node2) (getr 'father fn)))
           ((null fn))
           (if* (equal fn node1) (return t))))

(defun circ-print (node)
       (space (+ nindnt ftab ntab))
       (and nsw (princ (car node)) (space 3.))
       (tprint (+ nindnt 9.) (cons '##father-of-self## (fe node)))) 

;;exit functions
(comment dump nil
       (sprinter nil)
       (uwrite)
       ((lambda (^r ^q)
                (say output on $ (status dow) \, $ (status date) \.)
                (terpri)
                (mapc '(lambda (sent) (apply 'say-it sent)) :sent-list)) 
        t t)
       (setq :sent-list nil)
       (ufile dump >))

(defun adieu nil
       (remedit)
       (dump)
       (valret '|u|))


;;functions to print the phrase under a node
(defun phrasify(node)
       (mapcar '(lambda (x) (origcase (cdr x)))
               (sortcar (collectw node)
                        '(lambda (x y) (< x y)))))

(defun collectw (node)
       (cond ((getr 'word node)
              (ncons (cons (getr 'inputpos node) (getr 'word node))))
             (t  (mapcan '(lambda (funcset)
                                  (and 
                                   (not (atom funcset))
                                   (mapcan 'collectw funcset)))
                         (cdr (getr 'daughters node))))))

(defun prphrase (phrase)
       (mapc 'print2 phrase) t)

(defun nodes-to-words (nodestring)
       (mapcan '(lambda (node) (phrasify node)) nodestring))

(defun cfprint (node)
       (openchek node)
       (terpri)
       (terpri)
       (prphrase (phrasify node))
       (say \ \ \ pred : $ pred)
       (mapc '(lambda (case)
                      (say \ \ \  $ (car case) :) 
                      (prphrase (phrasify (cadr case))))
             (cadr certain-frame))
       t)

;;movie functions

(setq pswait .5)


(defun psset-int nil
  '(progn
    (for (poport bottom-tty)
       (princ '|pswait <-- |)
       (drain poport)
       (setq pswait (read)))))

(setsyntax 16. 'macro 'psset-int)


(setq no-removeds nil)

(setq make-film? nil super-film? nil)

(defun switch-display nil
  '(pdisplay (not :displayflag)))

(setsyntax 7. 'macro 'switch-display)

(defun pdisplay (flag)
       (setq :displayflag flag)
       (cond (flag (cursorpos 'c)
		   (splitscreen 6.)
		   ;(sstatus ttyint '\ 'switch-tyo)
		   )
	     (t (splitscreen nil) (cursorpos 'c) ;(sstatus ttyint '\ nil)
		)))

(comment defun switch-tyo (foo bar) 
       (nointerrupt nil)
       (cond ((eq poport bottom-tty) (setq poport top-tty))
             ((eq poport top-tty) (setq poport bottom-tty)))
       (terpri)
       (print '##))
       

(declare (special poport ttysw :noshow))
(setq :noshow nil)
(setq :noshow-defaults '(npool np-complete nbar-complete build-name))
(setq :noshowr nil)
(setq :noshowr-defaults '(np-complete nbar-complete))

(defun noshow fexpr (x)
       (cond ((null x) :noshow)
             ((eq (car x) 't) (setq :noshow :noshow-defaults))
             ((eq (car x) nil) (setq :noshow nil))
             (t (setq :noshow (append x :noshow)))))

(defun noshowr fexpr (x)
       (cond ((null x) :noshowr)
             ((eq (car x) 't) (setq :noshowr :noshowr-defaults))
             ((eq (car x) nil) (setq :noshowr nil))
             (t (setq :noshowr (append x :noshowr)))))
(setq :showrule nil)


(defun display-trace argnum
  (do-display))

(defun do-display nil
       (prog (poport ttysw)
	     (setq ttysw make-film?)
	     (if* (or (meet :activepackets :noshow)
		     (memq (car :activerule) :noshowr))
		 (return t))
	     (setq poport top-tty)
	     (cursorpos 'c)
	     ;(say |About to run:| $ (car :activerule))
	     ;(clearuntil 2.)
	     (for (:showrule t) (nstak? 1.))
	     (clearuntil 10.)
	     (bprint)
	     (clearuntil 15.)
	     (wstring)
	     (do-display-post)))

(defun do-display-post nil
  (cond (make-film? (if* (and super-film? :activerule)
			(say |________\$$$$$$$$$$$$$$$$|
			     $
			     (car :activerule)))
		    (prints '|_________________________\.block 20\|))
	((eq pswait 'record) (record))
	((and pswait (fixp pswait)) 
	      (listback (concat "sleep " pswait)))
	(t ;pswait is nil
	   (drain piport) 
	   (let ((char (ascii (tyi))))
		(cond ((eq char 'b)
		       (let ((poport (cond (top-tty bottom-tty))))
			    (drain poport)
			    (break)
			    (display-trace)))
		      ((eq char 'p)
		       (princ "pswait <-- ")
		       (setq pswait (read)))
		      ((eq char 'r)
		       (apply 'edit (ncons (car :activerule)))
		       (clearsplit)
		       (display-trace)))))))

(defun clearuntil (line)
       (cond (make-film? (terpri))
             (t (clearloop (- line (car (cursorpos)))))))

(defun clearloop (lines)
       (declare (fixnum lines))
       (do ((i 1 (1+ i)))
	   ((ge i lines) (terpri))
	   (declare (fixnum i))
           (terpri)))
              
(defun super-movie fexpr (file) (for (super-film? t) (apply 'movie file)))
(defun super-movie1 fexpr (file) (for (super-film? t) (apply 'movie1 file)))

(defun movie fexpr (file)
       (prog (^r make-film? abbrev-temp mport)
             (pdisplay t)
             (setq mport (apply 'outfile file))
             (setq make-film? t ^r t)
             (setq abbrev-temp (status abbrev))
             (sstatus abbrev 1)
	     (let ((poport mport))
		  (princ '|
.xgp\
.font 0 25fg\
.nofill\
.ss\
.sinch 8.5,6.0,11.0,9.0,1.0\
.php1\
.he1\
.he2\
\
.sp 2\
|)
             (say |Not showing rules in packets:| $ :noshow)
	     (errset (parse))
             (prints '|\.end\|))
             (close mport)
             (sstatus abbreviate abbr-temp)
             (pdisplay nil)))

(comment movie1 fexpr (file)
       (prog (^r make-film? abbrev-temp tty-linel)
             (pdisplay t)
             (uwrite dsk mitch)
             (setq make-film? t ^r t)
             (setq abbrev-temp (status abbrev))
             (sstatus abbrev 3)
             (setq tty-linel (linel top-tty))
             (linel top-tty 55.)
             (princ '|
.xgp\
.font 0 40fg\
.nofill\
.ss\
.sinch 8.5,7.5,11.0,9.0,0.5\
.php1\
.he1\
.he2\
\
.sp 2\
|)
             (say |Not showing rules in packets:| $ :noshow)
             (errset (parse))
             (prints '|\.end\|)
             (apply 'ufile 
                    (cond ((not file) '(movie > dsk mitch))
                          (file)))
             (sstatus abbreviate abbrev-temp)
             (linel top-tty tty-linel)
             (pdisplay nil)))

(comment trace-movie nil
       (prog (^r ^w )
             (ptrace)
             (uappend movie > dsk mitch)
             (ioc rw)
             (tyo 14)
             (parse)
             (ufile movie > dsk mitch)))
 

(comment nexts (n) 
       (do ((i 0.) (^q t) (temp nil) (:sent nil))
	   ((= i n)
            (maknam (nreverse :sent)))
	   (if* (memq (car (setq temp (cons  (ascii (tyi)) temp))) endpuncs)
                  (setq i (1+ i))
                  (setq :sent temp
                        temp nil))))

(comment nextts (n)
       (uread test)
       (say $ (nexts n)))

(defun sys:int-serv (dummy) 
  (for (poport (cond (top-tty bottom-tty)))
       (patom '"Interrupt:  " )
       (drain poport)
       (break)))

(or (getd '$old-gcafter)
  (putd '$old-gcafter (getd 'gcafter)))

(defun gcafter fexpr (x)
  (let ((poport (cond (top-tty bottom-tty))))
       (apply '$old-gcafter x)))

