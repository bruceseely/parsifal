;;lisp utilities not in (or variant from) PA;UTIL > used by MITCH
(declare (setsyntax '\# 2))
(declare (macros t))
(setq fasload nil)
(*rset t)
(setq defun t)
(prints '|****   *RSET switch is on.    ****|)
(prints '|****   DEFUN setq'd to T      ****|)


(fasload split fasl dsk liblsp)
(defun chrct (tty) (- (linel tty) (charpos tty)))
;;;^P -- loads PARSIFAL
(sstatus ttyint '\ '(lambda (f char) (nointerrupt nil)  (floadparsfl)))
;;;^^ -- shows parser state
(sstatus ttyint '\ '(lambda (f char) (nointerrupt nil)  (pstate)))
;;;^@ -- split parser display
(sstatus ttyint 0.
	 '(lambda (f char)
		  (nointerrupt nil)
		  (pdisplay (not :displayflag))
		  (princ '*)))
;;;^A -- record screen
(sstatus ttyint '\ '(lambda (f char) (nointerrupt nil)  (record))))
;;;^t -- trace
(sstatus ttyint 20. 'trace-interupt)
 
(setq *rset-trap '*rset-trap-handler)

(sstatus macro \" 'readstring)
(prints '|****   String hacker enabled.    ****|)

;;LEDIT setup
(setq ledit-named-forms '(defun setq def jlike rule crule macrodef redund
                           prefix suffix infix infixm nilfix))
(putprop 'sprint (get 'sprinter 'autoload) 'autoload)
(putprop 'step '(step fasl dsk liblsp) 'autoload)
(putprop 'bt '(debug fasl dsk liblsp) 'autoload)
(putprop 'debug '(debug fasl dsk liblsp) 'autoload)
(setq warnsw t)




;;Parisfal loading functions
(defun loadparsfl nil
       (cursorpos 'c)
       (alloc '(list (70000 150000 .2)
		     symbol
		     (4000 4000. 400)
		     fixnum
		     (5000 15000 .2)))
       (princ 'loading\ parsifal)
       (terpri)
       (loadfuns pfiles)) 

;;loadfuns expects a list of files to load.
;;load-defaults is a global variable that can be set to the default list>

(defun floadparsfl nil
       (clear-input t)
       (nointerrupt nil)
       (prints '|Load PARSIFAL? Type space if Yes--|)
       (progn (cond ((eq (tyi) 40)
		     (loadparsfl)
                     (cursorpos 'c)
		     (switches)
		     (sstatus toplevel '(parsifal)))
		    (t (prints '|Aborted.|)))))

(declare (*fexpr fload cload))

(defun unglang fexpr (x)
       (ifnot (get 'glang-to-lisp 'fsubr) (fload @glang))
       (glang-setup)
       (apply 'glang-to-lisp x))

(defun parser! nil
       (fasload @com fasl dsk mitch)
       (fasload @parse)
       (fasload @case)
       (fasload ^rule)
       (ifnot (getl 'glang-setup '(subr expr))
              (defun glang-setup nil nil))
       (cload @defs)
       (cload @gram1 lisp dsk mitch)
       (setq :parsifal-as-subroutine t))



(defun loadfun*  (fnames loaders)
       (mapc '(lambda (loader file)
		      (or (eq loader 'n)
			  (apply (cdr (assq loader
					    '((f . fasload)
					      (d . fload)
                                              (r . refasl)
                                              (x . fload*)
					      (c . cload))))
				 (list file))))
	     loaders fnames)
       (prints '|All files loaded|))


(defun ##retrace? (form)
       (if* (and (eq (car form) 'defun)
                (get 'trace 'fsubr)
                (memq (cadr form) (trace)))
           (push (cadr form) ##retrace-list)))

(defun ##retrace! nil
       (if* ##retrace-list
	   (apply 'trace ##retrace-list)
	   (say |Retraced because editted:| $ ##retrace-list))
       (setq ##retrace-list nil))

(setq ##retrace-list nil)
(setq ##ledit-pre-eval-func '##retrace?)
(setq ##ledit-post-teco-func '##retrace!)


;; use ddt tty interrupt for bottom of screen since more reliable
(pagel tyo 10000)

;;; default --MORE-- handler
(endpagefn tyo 'pagepause)


(defun pagepause (tty)
       ;;; function to pause at bottom of page
       ;;; tty is output tty which is interrupted
       ;;; like oldio (sstatus pagep)
       ;;; output proceeds only if a <sp> or <cr> typed
       ;;; anything else aborts type-out
       ;;; <sp> rolls typeout
       ;;; <cr> clears screen and continues at top
       ;;; NOTE: (1) characters typed are NOT gobbled
       ;;;       (2) ctl-G or ctl-S interrupts can be used
       (clear-input t)
       (princ '--more-- tty)
       (nointerrupt nil)
       ((lambda (char)
                (cond ((= 32. char) (cursorpos 'top tty)(cursorpos 'l tty))
                      ((= 13. char)(cursorpos 'c tty))
                      (t (princ '|FLUSHED|)
                         (cursorpos 'top tty)
                         (cursorpos 'l tty)
                         (prog (*rset) (error)))))
        (tyipeek)))	

(defun dp1 (l)
       (do ((pl (plist l) (cddr pl)))
           ((null pl))
		 (terpri)
		 (print (car pl))
                 (sprint (cadr pl) (- (linel t) 12.) 0)))

(prints '|utilities in xutil > loaded.|)

