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
        (check "attach fires attach-monitor -> the crule" fired t)))

    results))
