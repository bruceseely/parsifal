;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-transitive-test.lisp
;;;
;;; A full *transitive* declarative clause parsed end to end against
;;; Marcus's verbatim gram1/gram3 rules: "the dog ate the bone .".
;;; Builds on the intransitive clause (gram1-clause-test) and adds object
;;; handling:
;;;
;;;   ... MAIN-VERB builds the VP ...
;;;   STARTNP / DETERMINER / NOUN / NBAR / NP-COMPLETE ...  -- the OBJECT NP
;;;   OBJECTS        ss-vp [=np]        -> attach the object NP to the VP
;;;   VP-NP          attachment crule   -> fill an obj slot of the VP's
;;;                                        case frame with the object NP
;;;   VP-DONE / S-DONE                  -> finish
;;;
;;; Result tree:
;;;   [S decl/major [NP the dog] [aux] [VP [verb ate] [NP the bone]]
;;;                 [finalpunc .]]
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-transitive-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (unless (find-package :parsifal)
    (asdf:load-system :parsifal))
  (unless (find-package :glang-cl)
    (let* ((here  (or *load-truename* *compile-file-truename*))
           (root  (make-pathname
                   :defaults here
                   :directory (butlast (pathname-directory here) 2)))
           (glang (merge-pathnames "system/reference/glang-cl/" root)))
      (dolist (f '("package" "tokens" "pratt" "fixes"
                   "denotations" "compiler"))
        (load (merge-pathnames (format nil "~a.lisp" f) glang))))))

(in-package :parsifal)


(defparameter *gram1-transitive-rules*
  '("{RULE INITIAL-RULE IN NOWHERE
      [t] --> Create a new s node. !(setq s {the current s}).
      Activate cpool,ss-start.}"
    "{AS RULE STARTNP IN CPOOL
      [=ngstart] --> Create a new np node.
      If 1st is det then activate parse-det else activate parse-qp-1.
      Activate npool.}"
    "{CREATION CRULE NP-START NP
      if c is none of propn-np, pron-np, name, complex-noun-np, trace, *, comp-np
        and 1st is not pronoun then associate a new case frame with c.}"
    "{RULE DETERMINER IN PARSE-DET
      [=det] --> Attach 1st to c as det. Label c det.
      Transfer the features indef, def, wh from 1st to c.
      Deactivate parse-det. Activate parse-qp-2.}"
    "{RULE DET-QUANT-DONE PRIORITY: 15 IN PARSE-QP-2
      [t] --> Deactivate parse-qp-2. Activate parse-adj.}"
    "{RULE ADJ IN PARSE-ADJ
      [t] --> If 1st is adj then attach 1st to c as adj
        else if 1st is punc then attach 1st to c as punc
          else deactivate parse-adj; activate parse-noun.}"
    "{RULE NOUN IN PARSE-NOUN
      [=noun] --> Attach 1st to a new nbar node as noun.
      Transfer the features massn, time, ns, npl, n1p, n2p, n3p from 1st to c.
      Drop c.}"
    "{RULE NBAR IN PARSE-NOUN
      [=nbar] --> Attach 1st to c as nbar.
      Transfer the features time, place, n1p, n2p, n3p from 1st to c.
      Deactivate npool, parse-noun. Drop c into the buffer. Restore the buffer.}"
    "{ATTACHMENT CRULE NP-NBAR NP OVER NBAR
      Associate the case frame of the upper node with the lower node.
      !'Something-or-other fills the spec slot of the upper node.
      If there is a case frame of upper and there is a noun of the lower node
        then it fills the pred slot of upper.}"
    "{NR RULE NBAR-COMPLETE IN CPOOL [=nbar] --> Activate cpool, nbar-complete.}"
    "{RULE NBAR-DONE PRIORITY: 15 IN NBAR-COMPLETE t --> Drop c. Restore the buffer.}"
    "{NR RULE NP-COMPLETE IN CPOOL
      [=np; * is none of modified, not-modifiable] -->
      If there is not a det of c and there is not a qp of c then
        if the nbar of c is npl then label c npl else
        if the noun of the nbar of c is propnoun then label c prop-np else
        if the nbar of c is massn, ns then label c massnp else
        if the nbar of c is time and the ord of the qp of c is general-ord
          then label c next-time-np else
        if the noun of the nbar of c is none of pseudopropnoun then label c bad.
      If there is a det of c or there is a qp of c then
        transfer the meet of (if there is a det of c then the features of it else ns,npl)
          and (if there is a qp of c then the features of it else ns,npl)
          from the nbar of c to c;
        if c is none of ns,npl then label c bad.
      Activate cpool, np-complete.}"
    "{RULE NP-DONE PRIORITY: 15 IN NP-COMPLETE
      t --> If there is a case frame of c then finalize the cf of c.
      Drop c. Restore the buffer.}"
    "{RULE MAJOR-DECL-S IN SS-START
      [=np] [=verb] --> Label c decl, major. Deactivate ss-start. Activate parse-subj.}"
    "{RULE UNMARKED-ORDER IN PARSE-SUBJ
      [=np] [=verb] --> Attach 1st to c as np. Deactivate parse-subj. Activate parse-aux.}"
    "{RULE STARTAUX IN PARSE-AUX
      [=verb] --> Create a new aux node.
      Transfer vspl, v1s, v+13s, vpl+2s, v-3s, v3s from 1st to c.
      Transfer pres, past, future, tnsless from 1st to c.
      Activate build-aux, cpool.}"
    "{RULE AUX-ATTACH IN PARSE-AUX
      [=aux] --> Attach 1st to c as aux. Activate parse-vp. Deactivate parse-aux.}"
    "{RULE AUX-COMPLETE PRIORITY: 15 IN BUILD-AUX [t] --> Drop c into the buffer.}"
    "{RULE MAIN-VERB IN PARSE-VP
      [=verb] -->
      Deactivate parse-vp.
      If c is major then activate ss-final else if c is sec then activate emb-s-final.
      Attach a new vp node to c as vp.
      Attach 1st to c as verb.
      Activate cpool.
      If there is a verb of c and it is passive then activate passive; run passive next.
      If it is inf-obj then
        if it is to-less-inf-obj then activate to-less-inf-comp andthen
        if it is to-be-less-inf-obj then activate to-be-less-inf-comp andthen
        if it is 2-obj-inf-obj then activate 2-obj-inf-comp
          else activate inf-comp;
          if it is subj-less-inf-obj then activate subj-less-inf-comp else
          if it is no-subj then activate no-subj.
      If it is that-obj then activate that-comp.
      If there is a wh-comp and it is not utilized then activate wh-vp else
      If the current s is major then activate ss-vp else
      Activate embedded-s-vp.}"
    "{ATTACHMENT CRULE VP-VERB VP OVER VERB
      Associate a new case frame with the upper node.
      Associate the case frame of the upper node with the s above upper.
      The aux of the s above upper fills the spec slot of upper.
      The lower node fills the pred slot of upper.
      If the lower node is none of no-subj, passive
        and there is a binding of the np of the s above upper
        then it fills the subj slot of upper.}"
    ;; object handling
    "{ATTACHMENT CRULE VP-NP VP OVER NP
      The lower node fills an obj slot of the upper.}"
    "{RULE OBJECTS IN SS-VP [=np] --> Attach 1st to c as np.}"
    "{RULE VP-DONE PRIORITY: 20 IN SS-VP [t] --> Drop c.}"
    "{RULE S-DONE IN SS-FINAL
      [=finalpunc] --> Attach 1st to c as finalpunc. Finalize the cf of c. Parse is finished.}"))


(defun gram1-transitive-test (&optional verbose)
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

      (reset-rule-table)
      (reset-lexicon)

      ;; --- Lexicon (in :parsifal so features are EQ to the linked rules).
      (dolist (w (list 'the 'dog 'ate 'bone (intern "." :parsifal)))
        (setf (symbol-plist w) nil))
      (df the feats (det ngstart def ns npl))
      (df dog feats (noun ns n3p) markers (anim hanim))
      (df bone feats (noun ns n3p) markers (physob inanim))
      ;; "ate": a past-tense transitive main verb -- object (neut) + agent.
      (df ate feats (verb mainverb past v-3s) cf (neut agt) markers (act))
      (%df (list (intern "." :parsifal) 'feats '(finalpunc punc)))
      (dolist (w (list 'the 'dog 'ate 'bone (intern "." :parsifal))) (expandsim w))

      ;; --- Grammar (compile -> link -> register).
      (dolist (src *gram1-transitive-rules*)
        (eval (glang-cl::link (glang-cl::compile-rule src))))

      ;; --- Parse "the dog ate the bone ." end to end.
      (let ((ok (parse-sentence "the dog ate the bone ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"the dog ate the bone .\"" ok t)

        ;; Two NPs built (subject + object), the clause layer, and OBJECTS.
        (check "transitive derivation (subject NP, clause, object NP, OBJECTS)"
               (reverse (mapcar #'symbol-name *deriv*))
               '("INITIAL-RULE" "STARTNP" "DETERMINER" "DET-QUANT-DONE"
                 "ADJ" "NOUN" "NBAR" "NP-COMPLETE" "NBAR-COMPLETE"
                 "NBAR-DONE" "NP-DONE" "MAJOR-DECL-S" "UNMARKED-ORDER"
                 "STARTAUX" "AUX-COMPLETE" "AUX-ATTACH" "MAIN-VERB"
                 "STARTNP" "DETERMINER" "DET-QUANT-DONE" "ADJ" "NOUN"
                 "NBAR" "OBJECTS" "NP-COMPLETE" "NBAR-COMPLETE"
                 "NBAR-DONE" "NP-DONE" "VP-DONE" "S-DONE"))

        ;; Tree: [S decl/major [NP subj] [aux] [VP [verb] [NP obj]] [finalpunc]].
        (truthy "S is a declarative major clause"
                (subsetp '(decl major s) (fe c)))
        (truthy "subject NP attached to S" (daughter 'np c))
        (let ((vp (daughter 'vp c)))
          (truthy "a VP was attached to S" vp)
          (truthy "the verb is under the VP" (and vp (daughter 'verb vp)))
          (truthy "the VP carries a case frame" (and vp (getr 'caseframe vp)))
          ;; the object NP is attached UNDER the VP (subject is under S).
          (let ((obj (and vp (daughter 'np vp))))
            (truthy "the object NP is attached to the VP" obj)
            (truthy "the object NP is \"the bone\""
                    (let* ((nbar (and obj (daughter 'nbar obj)))
                           (noun (and nbar (daughter 'noun nbar))))
                      (and noun (eq (getr 'word noun) 'bone))))))
        (truthy "final punctuation attached to S" (daughter 'finalpunc c))))

    (format t "~&gram1-transitive-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-transitive-test t)
