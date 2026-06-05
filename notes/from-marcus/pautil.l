;;; -*-lisp-*-
(comment required files)
(declare (setsyntax '\# 2))
(declare (sstatus macro \~ '(lambda () ((lambda (\~) (eval \~) \~) (read)))))
(setq \~ nil)						; ~ is a NO-OP to the interpreter.

;; Expressions preceded by ~ are read and digested by the compiler, as well as being
;; compiled and left defined in the file.  Use ~ before expressions that will be needed at
;; at compile-time as well as load-time.


~
(defun requiredf narg
       ;; Arguments are files that are required to be loaded.
       ;; REQUIREDF will halt and load them if the current version is not present.
       ;; Currency is determined using *VERSION*.
       (do ((files (listify narg) (cdr files))
            (f))
           ((null files))
           (setq f (mergef (car files) '((* *) * fasl)))
           ;; if file not  yet in according to version, then load
           (or (equal (mergef '((* *) * t) f)(mergef '((* *) * t)(get '*version* (cadr f))))
               (progn (or (status feature noldmsg)
			  (progn (terpri)(princ '|\;Loading required file |)(prin1 f)))
                      (load f)))
           ;;if has version, then compare, otherwise assume ok
           (cond ((and (get '*version* (cadr f))
                       (not (equal (get '*version* (cadr f))
                                   (probef (mergef '((* *) * >) f)))))
                  (terpri tyo)(princ '|\;Required file latest version not compiled |  tyo)
                  (prin1 f tyo)
                  (break alt-p-to-proceed)))))

~(requiredf '((dsk liblsp) macrod)		  ; So macros defined herein will run.
	    '((dsk parsfl) %cntrl))			  ; Contains some macros.

(comment version numbers)

;;***********************************************************************
;; VERSION NUMBER --
;;   Put (VERSION) in each of the files whose version you wish to record.
;;    This puts the full file name (including version number) on the
;;    plist of *VERSION* under the indicator which is the first file name.
;;************************************************************************

(macroevaldef version (call)
       ;; puts the version number (the second filename) of the current file on
       ;; property of *VERSION* which is the first file name.
       ;; Works for both interpreted and compiled versions.
       ((lambda (filename)
                ;; assume file name in newio format
                `(defprop *version* ,filename ,(cadr filename)))
	(truename infile)))

(defun version? (file)
       ;; returns the present version number of given file
       (caddr (get '*version* file)))

(defun check-versions ()
       ;; runs through all the version numbers on *VERSION* and
       ;; makes sure they correspond to the highest version number on dsk.
       ;; It returns list of file names that have higher version numbers on
       ;; dsk than in present image.
       (do ((plist (cdr (plist '*version*)) (cddr plist))
            (filename)(hits))
           ((null plist) hits)
           (setq filename (car plist))
           (or (equal filename (probef (mergef '((* *) * >) filename)))
               (push filename hits))))

(defun bump-version (v)
       ;; V is a version number (not a fixnum).
       ;; Returns a version number (not a fixnum), which is 1+ V.
       ((lambda (base ibase *nopoint)
                (implode (exploden (1+ (readlist (exploden v))))))
        10. 10. t))

(version)						; record version of this file

(comment list manipulation)
;;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;;
;;;		List Hacking Utilities
;;;
;;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(defun firstn (n list)
       ;; returns a list comprising the first N elements of LIST.
       (do ((l list (cdr l)) (output) (i (cond((minusp n) 0)(n)) (1- i)))
           ((or (zerop i) (null l)) (nreverse output))
           (declare (fixnum i))
           (setq output (cons (car l) output))))


;;;	The following macros extract the Nth field of a list.

(macrodef 1st (l) (car l))

(macrodef 2nd (l) (cadr l))

(macrodef 3rd (l) (caddr l))

(macrodef 4th (l) (cadddr l))

(macrodef 5th (l) (car (cddddr l)))

(comment file manipulation)


(defun readfile fexpr (file)
       ;; Fix to use newio file specs
 ((lambda (eof)
   (apply 'uread file)
   (setq ^q t)
   (terpri)
   (princ (status uread))
   (do ((exp (read eof) (read eof)))
       ((eq exp eof))
       (#prin1 (eval exp)))
   '*)
  (list nil)))


(defun writefile (sexpr filspc)
       ((lambda (fil prinlevel prinlength)
             (prog2
              ((lambda (outfiles ^r ^w) (#print sexpr))	;;grinds Sexpr.
	       (cons fil outfiles) t t)
              (truename fil)
              (close fil)))
	(open (mergef filspc defaultf) 'out) nil nil))

(comment printing utilities)

(defun newline ()
       ;; puts cursor on a blank line.
       (or (zerop (charpos t)) (terpri)))

(defun newpage ()
       ;; prints a formfeed.
       (tyo 12.))

(defun space (n)
       ;; Leaves N blank lines after the current line.
       (declare (fixnum n i))
       (newline)
       (do i n (1- i) (zerop i) (terpri)))

(defun printc (string)
       ;; "PRINT" using PRINC instead of PRIN1.
       (terpri) (princ string) (tyo 32.))

(defun chrct (tty)
       (- (linel tty) (charpos tty)))


(comment numerical logical and string operators)

;;;	Numerical Operators


(defun ge (a b)
       ;; greater-than-or-equal-to predicate for numbers
       (signp ge (difference a b)))

(defun le (a b)
       (signp le (difference a b)))

(defun round (x)
       ;;rounds off the number X to the nearest integer.
       (fix (plus 0.5 x)))



;;;	String Oriented Utilities.

(defun concatenate args
       ;;args are atoms; returns interned atom formed by concatenation.
       (implode (mapcan 'exploden (delq nil (listify args)))))

(defun stringify args
       ;; Like concatenate, except does NOT intern atom.
       (maknam (mapcan 'exploden (delq nil (listify args)))))


;;;	Miscellany

(macrodef kwote (x) `(quote ,x))

(macrodef listp (x) (eq (typep x) 'list))

(macrodef logand frobs (boole 1 . frobs))

