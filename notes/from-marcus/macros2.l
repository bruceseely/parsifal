(declare (macros t))
(*lexpr say-it warner)
(defun say macro (l)
    (progn
       (rplaca l 'say-it)
       (do ((saylist) (saytemp (cdr l) (cdr saytemp)))
	   ((null saytemp) (rplacd l (reverse saylist)))
	   (cond ((eq (car saytemp) '$)
		  (setq saytemp (cdr saytemp))
		  (setq saylist (cons (car saytemp) saylist)))
		 (t (setq saylist (cons `(quote ,(car saytemp)) saylist)))))))

(defun meet macro (s) (rplaca s 'intersectq))

(defun make-eq-set macro (s) (rplaca s 'setifyq))

(defun make-equal-set macro (s) (rplaca s 'setify))

(defun set-union macro (s) (rplaca s 'union))

(defun set-difference macro (s) (rplaca s 'setminus))

(defun set-intersection macro (s) (rplaca s 'intersect))

(defmacro meet1 (a b)
	  `(intersectq2 ,a ,b))

(defmacro meet2 (a b)
	  `(intersect2 ,a ,b))

(defmacro union1 (a b)
	  `(unionq2 ,a ,b))

(defmacro setminus1 (a b)
	  `(setminusq2 ,a ,b))

(defun warn macro (l)
   (progn
       (rplaca l 'warner)
       (do ((warnlist) (warntemp (cddr l) (cdr warntemp)))
	   ((null warntemp)
	    (rplacd l (cons (cadr l) (reverse warnlist))))
	   (cond ((eq (car warntemp) '$)
		  (setq warntemp (cdr warntemp))
		  (setq warnlist (cons (car warntemp) warnlist)))
		 (t (setq warnlist
			  (cons `(quote ,(car warntemp))
				warnlist)))))))

(defun cat macro (x) (rplaca x 'concat))

(defmacro logand (&rest frobs) `(boole 1 ,@frobs))
(defmacro logor (&rest frobs) `(boole 7 ,@frobs))

(defmacro default-arg (num var) 
  `(cond ((< (arg) ,num) ,var) ((arg ,num))))

