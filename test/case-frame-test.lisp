;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-

(in-package :parsifal)

;;; Tests for system/core/runtime/case-frame.lisp (Increment 1: data
;;; type, access, open/close caching) and the phrase-structure
;;; accessors head/word/root-of in node-ops.lisp.
;;;
;;; (case-frame-test)   -> t if all pass, nil otherwise
;;; (case-frame-test t) -> verbose
;;;
;;; Each block rebinds the case specials (openframe/hypo-slots/
;;; objs-needed/pred) so frames don't leak between cases.


(defun case-frame-test (&optional verbose)
  (let ((results t))
    (flet ((check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%"
                           expected actual)))
               (setf results (and pass results))))
           (truthy (test-name actual)
             (let ((pass (and actual t)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass (format t "    expected non-NIL, got NIL~%")))
               (setf results (and pass results)))))

      ;; --- newcf -----------------------------------------------------

      (let ((openframe nil) (hypo-slots '((nil nil))) (objs-needed 0) (pred nil))
        (let ((cf (newcf 'normal)))
          (truthy "newcf returns a symbol"   (symbolp cf))
          (check  "newcf sets the frame type" (get cf 'case-frame) 'normal)
          (check  "newcf makes it the open frame" openframe cf)))

      ;; --- associate-cf / case-frame / assoc-node --------------------

      (let ((openframe nil) (hypo-slots '((nil nil))) (objs-needed 0) (pred nil)
            (node (cons (gensym "N") 0)))
        (setf (symbol-plist (car node)) nil)
        (let ((cf (newcf 'normal)))
          (associate-cf cf node)
          (check "case-frame retrieves the node's frame" (case-frame node) cf)
          (check "assoc-node retrieves the frame's node" (assoc-node cf) node)))

      ;; --- open / close caching --------------------------------------

      (let ((openframe nil) (hypo-slots '((nil nil))) (objs-needed 0) (pred nil))
        (let ((cf (newcf 'normal)))
          (setq hypo-slots '((a b)) objs-needed 3 pred 'verb-pred)
          (closeframe cf)
          (check "closeframe caches hypo-slots"  (get cf 'hypo-slots)  '((a b)))
          (check "closeframe caches objs-needed" (get cf 'objs-needed) 3)
          (check "closeframe caches pred"        (get cf 'pred)        'verb-pred)
          (openframe nil)                       ; clear the specials
          (check "clearing zeroes objs-needed"   objs-needed 0)
          (openframe cf)                        ; reopen
          (check "openframe restores hypo-slots"  hypo-slots  '((a b)))
          (check "openframe restores objs-needed" objs-needed 3)
          (check "openframe restores pred"        pred        'verb-pred)))

      ;; --- open-obj-cases / maxunls / minunls ------------------------

      (check "open-obj-cases: empty slotframe -> 0"
             (open-obj-cases '(nil nil)) 0)
      (check "open-obj-cases: *obj case -> 2"
             (open-obj-cases '(((*obj opt)) nil)) 2)
      (check "open-obj-cases: other case -> 1"
             (open-obj-cases '(((agt oblig)) nil)) 1)

      (let ((openframe nil) (hypo-slots '((nil nil))) (objs-needed 0) (pred nil)
            (node (cons (gensym "N") 0)))
        (setf (symbol-plist (car node)) nil)
        (let ((cf (newcf 'normal)))
          (associate-cf cf node)
          (setf (get cf 'hypo-slots)
                '((((*obj opt)) nil) (((agt oblig)) nil)))
          (openframe nil)                       ; force openchek to reload
          (check "maxunls picks the max open object cases" (maxunls node) 2)
          (check "minunls picks the min open object cases" (minunls node) 1)))

      ;; --- putc / getc (frameless node = inert) ----------------------

      (let ((node (cons (gensym "N") 0)))
        (setf (symbol-plist (car node)) nil)
        (check "getc on a frameless node is NIL"
               (getc 'foo node) nil)
        (check "putc on a frameless node is a no-op"
               (putc 'foo 'bar node) nil))

      ;; --- head / word / root-of -------------------------------------

      (let ((noun-node (cons (gensym "NOUN") 0))
            (nbar-node (cons (gensym "NBAR") 0))
            (np-node   (cons (gensym "NP")   0)))
        (dolist (n (list noun-node nbar-node np-node))
          (setf (symbol-plist (car n)) nil (symbol-value (car n)) nil))
        (setfe noun-node '(noun))
        (setfe nbar-node '(nbar))
        (setfe np-node   '(np))
        (attach nbar-node noun-node 'noun)      ; noun is nbar's daughter
        (attach np-node   nbar-node 'nbar)      ; nbar is np's daughter
        (check "head of an NP walks down to the noun"
               (head np-node) noun-node))

      (let ((node (cons (gensym "N") 0)))
        (setf (symbol-plist (car node)) nil)
        (setr 'word 'cats node)
        (setf (get 'cats 'root) 'cat)
        (check "word returns the root of the node's word" (word node) 'cat))
      (setf (get 'dogs 'root) 'dog)
      (check "root-of returns a word's root" (root-of 'dogs) 'dog)
      (check "root-of of a rootless word is the word itself"
             (root-of 'qxzzy) 'qxzzy)

      ;; --- alt-fillslot (frameless s = inert) ------------------------

      (let ((s (cons (gensym "S") 0)))
        (setf (symbol-plist (car s)) nil)
        (check "alt-fillslot runs without error"
               (progn (alt-fillslot 'agt 'verb 'np) t) t))

      ;; --- creation crules / create-monitor --------------------------

      (let ((*create-rules* nil) (fired nil))
        (crule-index 'creation
                     (list 'foo (lambda () (setq fired 'yes)) 'foo-crule))
        (create-monitor 'foo)
        (check "create-monitor fires the matching creation crule" fired 'yes)
        (setq fired nil)
        (create-monitor 'other)
        (check "create-monitor ignores an unmatched type" fired nil))

      ;; ...and create-monitor is reached through newnode
      (let ((*create-rules* nil) (*nodelist* nil) (*activenodestak* nil)
            (*activepackets* nil) (c nil) (*current-s* nil) (*wh-comp* nil)
            (fired nil))
        (crule-index 'creation
                     (list 'baz (lambda () (setq fired (getr 'type c))) 'baz-crule))
        (newnode 'baz nil)
        (check "newnode fires create-monitor for the new node's type"
               fired 'baz))

      ;; --- attachment crules / attach-monitor ------------------------

      (let ((*attach-rules* (make-hash-table :test #'eq))
            (fnode nil) (snode nil) (fired nil)
            (father   (cons (gensym "F") 0))
            (daughter (cons (gensym "D") 0)))
        (setf (symbol-plist (car father)) nil
              (symbol-plist (car daughter)) nil)
        (setr 'type 'vp father)
        (crule-index 'attachment
                     (list (cons 'vp 'verb)
                           (lambda () (setq fired (list fnode snode)))
                           'vp-verb))
        (attach-monitor father daughter 'verb)
        (check "attach-monitor fires the matching attachment crule"
               fired (list father daughter))
        (check "attach-monitor binds fnode/snode for the crule body"
               (and (eq fnode father) (eq snode daughter)) t)
        (setq fired nil)
        (attach-monitor father daughter 'obj)   ; no crule for obj
        (check "attach-monitor ignores an unmatched attach type" fired nil))

      ;; ...and attach-monitor is reached through attach
      (let ((*attach-rules* (make-hash-table :test #'eq))
            (fnode nil) (snode nil) (fired nil)
            (father   (cons (gensym "F") 0))
            (daughter (cons (gensym "D") 0)))
        (setf (symbol-plist (car father)) nil
              (symbol-plist (car daughter)) nil)
        (setfe father nil) (setfe daughter nil)
        (setr 'type 'np father)
        (crule-index 'attachment
                     (list (cons 'np 'det) (lambda () (setq fired t)) 'np-det))
        (attach father daughter 'det)
        (check "attach fires attach-monitor -> the crule" fired t))

      ;; --- semantic-marker scoring (smqval / smqchek / maxsmqval) ----

      (let ((noun (cons (gensym "NOUN") 0))
            (nbar (cons (gensym "NBAR") 0))
            (np   (cons (gensym "NP")   0))
            (pred nil))
        (dolist (n (list noun nbar np))
          (setf (symbol-plist (car n)) nil (symbol-value (car n)) nil))
        (setfe noun '(noun)) (setfe nbar '(nbar)) (setfe np '(np))
        (setr 'markers '(anim) noun)
        (attach nbar noun 'noun)
        (attach np   nbar 'nbar)
        (setf (get 'agt   'markerset) (list 'hanim '|#| 'anim)   ; ok # great
              (get 'pat   'markerset) (list 'rock)
              (get 'thing 'markerset) (list 'all))
        (check "smarkers finds the phrase head's markers" (smarkers np) '(anim))
        (check "smqval: a great-set marker scores 1"  (smqval '(agt oblig) np) 1)
        (check "smqval: no matching marker scores -2" (smqval '(pat oblig) np) -2)
        (check "smqval: an `all' case scores 1"       (smqval '(thing oblig) np) 1)
        (check "smqchek: a fitting case is T"     (smqchek '(agt oblig) nil np) t)
        (check "smqchek: a non-fitting case is NIL" (smqchek '(pat oblig) nil np) nil)
        (check "maxsmqval picks the best fit"
               (maxsmqval '((agt oblig) (pat oblig)) np) 1))

      ;; --- hypothesis generation -------------------------------------

      (check "subjcasegen reads in reverse, stops after the oblig case"
             (subjcasegen '(((agt oblig) (loc opt)) nil) t)
             '(((agt oblig) nil nil) ((loc opt) ((agt oblig)) nil)))

      (let ((objs-needed 1))
        (check "objcasegen returns the first open case (with the nil hack)"
               (objcasegen '(((obj oblig) (loc opt)) nil) t)
               '(((obj oblig) (nil (loc opt)) nil))))

      (let ((pred nil) (refillables '(time)))
        (setf (get 'with 'cases-marked-by) '(instr))
        (check "ppcasegen fronts the preposition-marked case"
               (ppcasegen '(((agt oblig) (instr opt)) nil) 'with t)
               '(((instr opt) ((agt oblig)) nil))))

      ;; cases dispatches through openchek to the right generator
      (let ((openframe nil) (hypo-slots '((nil nil))) (objs-needed 1) (pred nil)
            (node (cons (gensym "N") 0)))
        (setf (symbol-plist (car node)) nil)
        (let ((cf (newcf 'normal)))
          (associate-cf cf node)
          (setf (get cf 'hypo-slots)  '((((obj oblig)) nil))
                (get cf 'objs-needed) 1)
          (openframe nil)                       ; force openchek to reload
          (check "cases dispatches obj -> objcases"
                 (cases node t 'obj)
                 '(((obj oblig) (nil) nil)))))

      ;; --- consolidate-frame -----------------------------------------

      (let ((openframe (make-symbol "CF"))
            (hypo-slots '((nil ((agt n1 subj)))))
            (objs-needed 0) (pred nil) (ctrace nil))
        (setf (get openframe 'cases) nil)
        (consolidate-frame)
        (check "consolidate-frame binds a single hypothesis's slots"
               (get openframe 'cases) '((agt n1 subj))))

      (let ((openframe (make-symbol "CF"))
            (hypo-slots '((nil ((agt n1 subj) (obj n2 obj)))
                          (nil ((agt n1 subj)))))
            (objs-needed 0) (pred nil) (ctrace nil))
        (setf (get openframe 'cases) nil)
        (consolidate-frame)
        (check "consolidate-frame keeps only slots certain in every hypothesis"
               (get openframe 'cases) '((agt n1 subj))))

      ;; --- major operations: a clause's case-frame lifecycle ---------

      (labels ((mk-np (marks)
                 (let ((np (cons (gensym "NP") 0)) (noun (cons (gensym "NOUN") 0)))
                   (dolist (x (list np noun))
                     (setf (symbol-plist (car x)) nil (symbol-value (car x)) nil))
                   (setfe np '(np)) (setfe noun '(noun))
                   (setr 'markers marks noun)
                   (attach np noun 'noun)
                   np))
               (s-node ()
                 (let ((n (cons (gensym "S") 0)))
                   (setf (symbol-plist (car n)) nil (symbol-value (car n)) nil)
                   (setfe n '(s)) n))
               (v-node (w)
                 (let ((v (cons (gensym "V") 0)))
                   (setf (symbol-plist (car v)) nil)
                   (setr 'word w v) v)))

        ;; fillpred -> fits -> fillslot(subj/obj) -> finalize-frame
        (let ((openframe nil) (hypo-slots '((nil nil))) (objs-needed 0)
              (pred nil) (ctrace nil) (c nil)
              (s    (s-node))
              (verb (v-node 'hit))
              (subj (mk-np '(anim)))
              (obj  (mk-np '(anim))))
          (setf (get 'hit 'case-frame) '((obj oblig) (agt oblig))
                (get 'agt 'markerset)  '(all)
                (get 'obj 'markerset)  '(all))
          (associate-cf (newcf 'normal) s)
          (fillpred verb s)
          (check "fillpred seeds hypo-slots from the verb's case-frame"
                 hypo-slots '((((obj oblig) (agt oblig)))))
          (check "fits: an np fits the clause as subject"
                 (fits subj 'subj s) t)
          (fillslot subj 'subj s)
          (truthy "filling the subject records the agt slot"
                  (member (list 'agt subj 'subj) (get openframe 'cases)
                          :test #'equal))
          (fillslot obj 'obj s)
          (finalize-frame s)
          (truthy "finalized frame keeps the agt (subject) slot"
                  (member (list 'agt subj 'subj) (get openframe 'cases)
                          :test #'equal))
          (truthy "finalized frame keeps the obj slot"
                  (member (list 'obj obj 'obj) (get openframe 'cases)
                          :test #'equal)))

        ;; fits returns NIL when no case's markers match
        (let ((openframe nil) (hypo-slots '((nil nil))) (objs-needed 0)
              (pred nil) (ctrace nil)
              (node (s-node)) (subj (mk-np '(rock))))
          (associate-cf (newcf 'normal) node)
          (setq hypo-slots '((((agt oblig)) nil)))
          (setf (get 'agt 'markerset) '(hanim))   ; rock is not hanim
          (check "fits is NIL when the node's markers don't fit any case"
                 (fits subj 'subj node) nil))

        ;; set-objs-needed prunes hypotheses lacking enough object slots
        (let ((openframe nil) (hypo-slots '((nil nil))) (objs-needed 0)
              (pred nil) (ctrace nil)
              (node (s-node)))
          (associate-cf (newcf 'normal) node)
          (setq hypo-slots '((((*obj dat)) nil) (((agt oblig)) nil)))
          (set-objs-needed 2 node)
          (check "set-objs-needed keeps only hypotheses with >= n object slots"
                 hypo-slots '((((*obj dat)) nil))))

        ;; passivize-cf adds an obligatory-object variant
        (let ((openframe nil) (hypo-slots '((nil nil))) (objs-needed 1)
              (pred nil) (ctrace nil)
              (node (s-node)))
          (associate-cf (newcf 'normal) node)
          (setq objs-needed 1 hypo-slots '((((obj oblig)) nil)))
          (passivize-cf node)
          (check "passivize-cf adds a variant with the object case obligatory"
                 hypo-slots '(((nil (obj oblig)) nil)))))

      ;; --- pgof ------------------------------------------------------

      (let* ((np   (cons (gensym "NP") 0))
             (prep 'to)
             (result (pgof prep np))
             (node result))
        (check "pgof returns a one-element daughter list"
               (length result) 1)
        (truthy "pgof's element is a symbol (the fake node head)"
                (symbolp (car node)))
        (check "pgof's node exposes the NP via the daughter accessor"
               (daughter 'np node) np)
        (check "pgof's node exposes the prep via the daughter accessor"
               (daughter 'prep node) prep))

      ;; --- prefer ----------------------------------------------------

      (check "prefer: T when value1 leads value2 by at least degree"
             (prefer 5 2 3) t)
      (check "prefer: NIL when the lead is smaller than degree"
             (prefer 5 2 4) nil))

    results))
