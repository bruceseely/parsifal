(declare (include declr))
;;This file greatly cleaned up and commented by Kurt Van Lehn.

;;;****************************************************************
;;;
;;;		the Case Frame data type
;;;
;;;  Instances of case frames live on the CASE-FRAME prop of their head.
;;;  There are only instances of case frames since the constant 
;;;  information lives on the lexical entry of the head. Each cf
;;;  is a gensym with the following properties:
;;;	CASE-FRAME	NORMAL or MOD, to indicate caseframe type
;;;     ASSOC-NODE	The node this is a cf for.
;;;	SPEC		The aux or q-det of the head.
;;;	CASES		The filled <slot>s of the cf.
;;;	PRED		The root of the word that heads the cf
;;;	HYPO-SLOTS	A list of <slotframe>s showing the different
;;;			ways to use the rest of the NPs in filling 
;;;			the cf's slots.
;;;	OBJS-NEEDED	The number of obligatory slots left unfilled.
;;;  CASES, PRED, and HYPO-SLOTS are cached in variables of the same name
;;;  by the functions openframe, closeframe and openchek. The relevant 
;;;  properties on the pred are
;;;	CASE-FRAME	An ordered list of <case>s. This prop is set up
;;;			by morphology from the CF prop when the pred is first
;;;			used. CF's are in the lexicon.
;;;	PREPS		An alist of prepositions and the cases they mark.
;;;	<casename>	The semantic markers for the case, in two sets,
;;;			separated by #. First set is OK, second is great.
;;;  The relevant registers on the nodes are
;;;	MARKERS		The semantic marker set for the node.
;;;	CASE-FRAME	The cf for this node, if any.
;;;  
;;;  The internal data structures are as follows:
;;;	<hypoframe>	::= ( <case> <open-cases> <full-slots> )
;;;			::= ( <case> )
;;;	<slotframe>	::= ( <open-cases> <full-slots> )
;;;	<open-cases>	::= ( ... <case> ... )
;;;	<full-slots>	::= ( ... <slot> ... )
;;;	<case>		::= ( <name> <optionality> )
;;;	<slot>		::= ( <case> <node> <role> )
;;;	<role>		::= one of SUBJ, OBJ or PP.
;;;  There is a small hack -- nil is consed onto the front of <open-cases> when
;;;  no more slots may be filled by nodes with the syntactic <role> of OBJ. 
;;;
;;;****************************************************************


;;; creation and access utilities -- all of these can be called externally
;;; to this file
(comment creation and access utilities)
(defun newcf (type)
       (clearcf)
       (for (cf (makesym 'casefr))
            (setq openframe cf)
            (putprop cf type 'case-frame)
            cf))

(defun case-frame (node) (getr 'caseframe node))

(defun associate-cf (cf node)
       (setr 'caseframe cf node)
       (ifnot (get cf 'assoc-node)
	      (putprop cf node 'assoc-node)
	      (semcall 'newframe cf node (get cf 'case-frame)))) 

(defun assoc-node (cf) (get cf 'assoc-node))

(defun maxunls (node)
       (openchek node) 
       (do ((sfs hypo-slots (cdr sfs)) (i 0.))
	   ((null sfs) i)
	   (setq i (max i (open-obj-cases (car sfs))))))


(defun minunls (node)
       (openchek node)
       (do ((sfs hypo-slots (cdr sfs)) (i 64.))
	   ((null sfs) i)
	   (setq i (min i (open-obj-cases (car sfs))))))



;;;****************************************************************
;;;
;;;	the major case frame functions
;;;
;;;	NEED-SLOTS -- tells the cf to expect n objs. 
;;;	FITS  -- see if the  node will fits somewhere in the cf
;;;	FILLSLOT -- stick the node into the cf somewhere
;;;	FINALIZE-FRAME -- check that all the necessary cases are filled
;;;	PASSIVIZE-CF -- make cases that an obj could be obligatory. 
;;;
;;;****************************************************************
(comment the major case frame functions)



(defun need-slots (node n) 
;;; called from the grammar
       (set-objs-needed n node)
       (consolidate-frame))

(defun set-objs-needed (n node)
       (openchek node)
       (setq objs-needed n)
       (if* hypo-slots
	      (do ((sfrs hypo-slots (cdr sfrs)) (result))
		  ((null sfrs)
		   (setq hypo-slots result))
		  (or (< (open-obj-cases (car sfrs)) n)
		      (setq result (cons (car sfrs) result))))))

(defun open-obj-cases (slotfr) 
;;; also called from MAXUNLS and MINUNLS
          (cond ((or (null (car slotfr))
                     (null (caar slotfr))) 0.)
		((eq (caaar slotfr) '*obj) 2.)
		(1.)))

(defun fits (lower gfunc upper)
       (cond ((and (eq gfunc 'pp) (eq (getr 'type upper) 's))
              (cond ((getr 'caseframe lower) (fits* upper 'subj lower))
		    (t (associate-cf (newcf 'dummy-mod) '(dummy-node))
		       (fillslot (find-node 'prep lower) 'pred '(dummy-node))
		       (fillslot (find-node 'np lower) 'obj '(dummy-node))
		       (fits* upper 'subj '(dummy-node)))))
             ((eq gfunc 'pp)
	      (fits* (find-node 'np lower)
		     (word (find-node 'prep lower))
		     upper))
             ((fits* lower gfunc upper))))

(defun fits* (lower gfunc upper)
      (openchek upper)
      (do ((hyposet (cases upper nil gfunc) (cdr hyposet)))
          ;;; since mode is nil, cases just returns a list of <case>s. 
          ((null hyposet) nil)
          (if* (smqchek (car hyposet) upper lower) (return t))))


(defun fillslot (lower gfunc upper)
       (cond ((memq gfunc '(subj obj)) (fillcase lower gfunc upper))
             ((and (eq gfunc 'pp) (eq (getr 'type upper) 's))
              (pp-cf-check lower)
              (fillmod lower upper)
              (fillcase upper 'subj lower))
             ((eq gfunc 'pp)
              (fillcase (find-node 'np lower)
			(word (find-node 'prep lower))
			upper))
             ((eq gfunc 'pred) (fillpred lower upper))
             ((eq gfunc 'mod) (fillmod lower upper))
             ((eq gfunc 'spec) (fillspec lower upper))))

(defun fillmod (lower upper)
       (openchek upper)
       (consprop openframe lower 'mods)
       (putprop (getr 'caseframe lower) openframe 'mod-of)
       (semcall 'mod lower openframe))

(defun fillcase (lower gfunc upper)
       (openchek upper)
       (do ((hypos (cases upper t gfunc) (cdr hypos)) (result) (thehypo))
	   ((null hypos) (setq hypo-slots result))
           ;;; keep all hypotheses whose markers agree
	   (if* (smqchek (car (setq thehypo (car hypos))) upper lower)
	       (push (list (cadr thehypo)
			   (cons (list (caar thehypo) lower gfunc)
				 (caddr thehypo)))
		     result)))
       (if* (and (plusp objs-needed) (eq gfunc 'obj))
	   (set-objs-needed (1- objs-needed) upper))
       (consolidate-frame)
       (if* ctrace
	   (closeframe openframe)
	   (terpri)
	   (print '|-----------|)
	   (say |New frames for| $ upper)
	   (dp1 (getr 'caseframe upper))
	   (terpri)
	   (print '|-----------|)))

(defun fillpred (lower upper)
       (openchek upper)
       (setq pred
	     (or (get (getr 'word lower) 'root) (getr 'word lower))
	     hypo-slots
	     (ncons (ncons (get pred 'case-frame))))
       (semcall 'head lower openframe))

(defun fillspec (lower upper)
       (openchek upper)
       (putprop openframe
		(list (cond ((is-any-of upper '(s vp)) 'aux)
			    ((is-any-of upper '(np)) 'q-det)
			    (t (warn 0 $ lower
                                     |can 't be a spec for| $ upper)))
		      lower)
		'spec)
       (semcall 'spec (get openframe 'spec) openframe))


(defun finalize-frame (node)
;;; Removes hypotheses that still have obligatory cases left open if node is an s node.
             (openchek node)
       (if* (is-any-of node '(s))
             (do ((hypos hypo-slots (cdr hypos))
		  (badf nil)
	          (result nil))
	         ((null hypos)
		  (cond (result (setq hypo-slots result))
			(t (warn 0. some oblig slots left unfilled in $ openframe -- finalize-frame\.\
 $ badf)
			   (break badf :carefulsw))))
        	 (do ((casetail (caar hypos) (cdr casetail))
                         (case))
                        ((null casetail)
		         (push (car hypos) result))
                        (cond  ((null (setq case (car casetail))))
			       ((eq 'oblig (cadr case))
				(cond ((and (eq (car case) 'agt)
					    (is c '(np-preposed))))
				      (t (push  (car hypos) badf)
                                         (return nil))))
			       ((atom (cadr case)))
			       ((eq 'opt\\oblig (caadr case))
				(push (car hypos) badf)
                                (return nil)))))
	     (consolidate-frame))
             (semcall 'finalize-the-frame openframe)
             (closeframe openframe))

(defun passivize-cf (node)
       (openchek node)
       (passivize-cf1 nil))

(defun passivize-cf1 (mode)
       (setq 
	hypo-slots
        (nconc (if* mode hypo-slots)
               (mapcan
                '(lambda (slotfr)
                         (mapcar
                          '(lambda (hypo)
				   (list (append (cadr hypo)
						 (ncons (list
							 (caar hypo)
							 'oblig)))
					 nil))
   			  (objcasegen slotfr t)))
	       hypo-slots))))


;;;****************************************************************
;;;
;;;		Hypothesis generation
;;;
;;;  These return a list of possible ways to fill the given cf.  The
;;;  list has the element of the form ( <case> . <slotframe> ) where
;;;  <slotframe> is what you would have left if you filled the <case>,
;;;  unless mode is nil, in which case it <slotframe> is nil.
;;;  These triples are called <hypoframe>s above. 
;;;
;;;****************************************************************
(comment hypothesis generation)

(defun cases (node mode gfunc)
        (openchek node)
        (cond ((eq gfunc 'obj) (objcases mode))
	      ((eq gfunc 'subj) (subjcases  mode))
	      (t (ppcases gfunc mode))))

(defun subjcases (mode)
       (mapcan '(lambda (fr) (subjcasegen fr mode)) hypo-slots))

(defun objcases (mode)
       (mapcan '(lambda (fr) (objcasegen fr mode)) hypo-slots))

(defun ppcases (gfunc mode)
       (mapcan '(lambda (fr) (ppcasegen fr gfunc mode)) hypo-slots))




(defun subjcasegen (slotfr mode)
;;; reads the open-cases in reverse order! Quits at the first oblig case, if
;;; any, since that has to be filled before any following optional ones(?).  It
;;; is also carefull to avoid that nil marker that objcasegen sometimes puts on.
;;; (car nil) = (cdr nil) = nil  nowadays. 
       (do ((open-cases (reverse (car slotfr)) (cdr open-cases)) (result) (next))
           ((or (eq (cadr next) 'oblig)
                (null (car open-cases))) result)
           (setq next (car open-cases))
           (push (cond (mode (list next (reverse (cdr open-cases)) (cadr slotfr)))
		       (next))
		 result)))

(defun objcasegen (slotfr mode)
;;; usually just returns first empty case. 
       (prog (result open-cases)
             (if* (null (car (setq open-cases (car slotfr)))) (return nil))
	     ;;; when nil is the car of <open-cases>, the sf can't take anymore objs
             (if* (eq (caar open-cases) '*obj)
		 ;;; (*obj dat) means the dat case can be an indirect object, so
		 ;;; pull it out and put it in front.
                 (do ((l (cdr open-cases) (cdr l))
		      (head nil (cons (car l) head))
		      (*objcase (cadar open-cases)))
		     ((or (null l)
			  (if* (eq (caar l) *objcase)
			      (push (cond
				     (mode (list (car l)
						 (append (nreverse
							  head)
							 (cdr l))
						 (cadr slotfr)))
				     ((car l)))
				    result)
			      t))
		      ;;whether found or not, flush the *obj entry
		      (setq open-cases (cdr open-cases)))))
             (ifnot (< (open-obj-cases (ncons open-cases)) objs-needed)
 		    (push (cond (mode (list (car open-cases)
					    (cons nil (cdr open-cases))
					    ;;; see comment on nil hack above
					    (cadr slotfr)))
				((car open-cases)))
			  result))
             (return result)))


(defun ppcasegen (slotfr prep mode)
;;; Fronts the cases marked by the pp, reading forward down open-cases.
;;; If the case is refillable (a global attribute of a casename!) then it is 
;;; not removed from the open-cases.
       (do ((open-cases (car slotfr) (cdr open-cases))
	    (csave nil (cons (car open-cases) csave))
            (result)
            (ppcases (or (get (get pred 'preps) prep)
	      		 (get prep 'cases-marked-by))))
           ((null open-cases) result)
           (if* (memq (caar open-cases) ppcases)
               (push (cond (mode
                            (list (car open-cases)
                                  (append  (reverse csave)
                                           (cond ((memq (caar open-cases)
                                                        refillables)
                                                  open-cases)
                                                 ((cdr open-cases))))
                                  (cadr slotfr)))
                           ((car open-cases)))
                     result))))



;;;****************************************************************
;;;
;;; 		CONSOLIDATE-FRAME
;;;  Updates CASES so that it remains the intersection
;;;  of the filled slots over the hypthetical slotsframes.  
;;;
;;;****************************************************************
(comment consolidate-frame)

(defun consolidate-frame nil 
       (cond
        ((null hypo-slots)
	 (warn 0. no consistent hypo-slot)
	 (break badfr :carefulsw))
        ((null (cdr hypo-slots))
         (bind-slots (filter-out-filled (car hypo-slots))))
        ((do ((pcercs (filter-out-filled (car hypo-slots))(cdr pcercs))
  	      (cert-slots))
	     ((null pcercs) (bind-slots cert-slots))
	     (do ((otherslotfrs (cdr hypo-slots) (cdr otherslotfrs))
		  (slot (car pcercs)))
		 ((null otherslotfrs) (push slot cert-slots))
		 (ifnot (member slot (cadar otherslotfrs))
	 	        (return nil)))))))

(defun filter-out-filled (hslotfr)
        (do ((hslots (cadr hslotfr) (cdr hslots)) 
	     (result)
	     (cases (get openframe 'cases)))
	    ((null hslots) result)
            (ifnot (member (car hslots) cases)
		   (push (car hslots) result))))

(defun bind-slots (cert-slots)
  (putprop openframe (append cert-slots (get openframe 'cases)) 'cases)
  (mapc
     (function (lambda (slot)(semcall 'case slot openframe)))
     cert-slots))


;;;****************************************************************
;;;
;;; 		SEMANTIC  MARKERS
;;;  since this is all going away soon, no comment
;;;
;;;****************************************************************
(comment semantic markers)

(defun smqval (case node) 
       (do ((marktail (or (get pred (car case))
                          (get (car case) 'markerset))
		      (cdr marktail))
	    (nmarkers (smarkers node))
	    (value 0.)
            (marker)
	    (#sw))
	   ((null marktail) -2.)
	   (cond ((eq (car marktail) 'all) (return 1.))
                 ((null nmarkers)
                  (if* :carefulsw 
                      (warn 0. |NO markers for| $ node \.))
                  (return 0))
                 ((memq (setq marker (car marktail)) nmarkers)
		  (return value))
		 ((eq marker '#)
		  (setq value (cond (#sw 0.) ((setq #sw t) 1.))))))) 

(defun smqchek (case upper node) (signp ge (smqval case node))) 


(defun smarkers (node)
  (cond ((is node '(pp))
         (getr 'markers (nead (find-node 'np node))))
        ((getr 'markers (head node)))
	((getr 'markers node)))) 

(defun maxsmqval (caseset node) 
       (apply 'max
	      (mapcar '(lambda (case) (smqval case node))
		      caseset)))
(defun fit-of (lower gfunc upper) (fit-of-1 lower (cases upper nil gfunc)))


(defun fit-of-1 (node caseset) (maxsmqval caseset node)) 

(setq much 2. somewhat 1. no 0.)

;;the following is the syntax/"smart semantics" interface

(setq super-clears t)

(defun super-smqval (case node frame)
       (prog (temp ^q)
             (if* (eq super-clears 'no-super) (return 0))
             (if* super-clears (cursorpos 'c))
	     (terpri)
             (print '|________________|)
             (say |How much do you like|  $ (phrasify node) |as| $ case |with|)
       (cfprint frame)
huh?   (say |
            0 - bad
            1 - ok
            2 - good
            --->|)
       (setq temp (read))
       (ifnot (and (numberp temp) (lessp -1 temp 4))
               (go huh?))
       (terpri)
       (print '|________________|)
       (return (cond ((= temp 0) -2)
                     ((= temp 1) 0)
                     ((= temp 2) 1)))))

(defun super-fit-of fexpr (l)
       (super-fit1-of (eval (car l)) (eval (cadr l)) (eval (car (last (cadr l))))))

(defun super-fit1-of (node caseset framenode)
       (apply 'max
              (mapcar '(lambda (case) (super-smqval case node framenode))
                      (do ((temp (fitspg1 node caseset t)))
                          (t (append (car temp) (cdr temp)))))))



;;; Cache functions
(comment cache functions)

(defun openchek (node)
       (for (cf (getr 'caseframe node))
	    (ifnot (equal openframe cf)
		    (if* openframe (closeframe openframe))
		    (openframe cf))))

(defun closeframe (cf) 
       (if* cf
       (putprop cf hypo-slots 'hypo-slots)
       (putprop cf pred 'pred)
       (putprop cf objs-needed 'objs-needed)))

(defun openframe (cf)
       (setq openframe cf)
       (cond (cf (setq hypo-slots (get cf 'hypo-slots)
		       pred       (get cf 'pred)
		       objs-needed  (get cf 'objs-needed)))
	     (t (clearcf))))

(defun clearcf nil
;;;also called from newcf
       (closeframe openframe)
       (setq hypo-slots '((nil nil)))
       (setq objs-needed 0)
       (setq pred nil))


;;;  miscelaneous stuff
(comment random stuff)

(defun prefer (value1 degree value2)
       (not (> degree (- value1 value2))))

(defun domf (node)
       (closeframe openframe)
       (do ((tnode (or (getr 'father node) (getr 'highercl node))
		   (or (getr 'father tnode) (getr 'highercl tnode))))
	   ((or (null tnode) (getr 'case-frame tnode)) tnode)))

(defun dom-cf (node)
       (do ((n (getr 'father node) (getr 'father n)))
           ((null n))
           (for (temp (getr 'caseframe n)) (if* temp (return temp)))))

(defun real-caseset (caseset sourcenode)
       (append (car caseset)
               (do ((possc-tail (poss-pg-cases sourcenode) (cdr possc-tail))
                    (result) (markers (cadr caseset)))
                   ((null possc-tail) result)
                   (and (meet (get (car possc-tail) 'genl-case-for) markers)
                        (setq result (cons (list (car possc-tail) 'genlc) result))))))


(defun putc (reg value node)
       (and (get (car node) 'caseframe)
	    (putprop (get (car node) 'case-frame) value reg)))


(defun getc (reg node)
       (get (or (get (car node) 'caseframe)
                '***total-dumm***y) reg))
              

(defun semcall l
       (ifnot (memq (get openframe 'case-frame) '(nil dummy-mod))
              (if* ctrace (apply 'say-it
				   (cons '|To semantics:| (node-w-feats (listify l)))))))

 (defun nodep (foo) (and (listp foo) (atom (car foo)) (numberp (cdr foo))))

(defun node-w-feats (list)
       (do ((l list (cdr l)) (result))
	   ((null l) (nreverse result))
	   (push (cond ((nodep (car l))
			(list (caar l) '\\ (fe (car l))))
		       ((car l)))
		 result)))




(defun pgof (prep np)
       (for (faknode (makesym 'dummypg))
	    (putprop faknode
		     (list nil 'np (ncons np) 'prep (ncons prep))
		     'daughters)
	    (ncons faknode))) 

(comment surface structure monitoring)

(defun crule-index (type indexinfo)
       (cond ((eq type 'attachment)
	      (for (ftype (caar indexinfo))
                   (for (typebucket (get ':attach-rules ftype))
                        (setq indexinfo (rplaca indexinfo (cdar indexinfo)))
                        (ifnot (member indexinfo typebucket)
                               (putprop ':attach-rules 
                                        (cons indexinfo typebucket)
                                        ftype)))))
	     ((eq type 'creation) (push indexinfo :create-rules))))

(defun create-monitor (type)
       (for (crule (assq type :create-rules))
	    (if* crule
		(if* :crtrace (say |Running create-rule| $ (caddr crule)))
		(funcall (cadr crule)))))

(defun attach-monitor (fn dn type)
            (for (attach-rules (get ':attach-rules (getr 'type fn))
                               crule nil)
                 (if* (if* attach-rules (setq crule (assoc type attach-rules)))
                     (if* :crtrace (say |Running attach-rule| $ (caddr crule)))
                     (setq fnode fn snode dn)
                     (funcall (cadr crule))))
       t)

