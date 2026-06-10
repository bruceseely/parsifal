;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/clause-grammar.lisp
;;;
;;; Shared rule library for the gram1/gram3 integration tests. Each rule
;;; is Marcus's verbatim grammar source (modulo whitespace); the tests
;;; compile -> LINK -> register the groups they need via REGISTER-GRAMMAR
;;; and supply their own (small) lexicon.
;;;
;;;   *np-rules*           gram1 INITIAL-RULE + the gram3 NP-construction
;;;                        rules (det / qp / adj / noun / nbar, the two NR
;;;                        completion rules, the NP-START + NP-NBAR crules)
;;;   *np-utterance-rule*  gram1 NP-UTTERANCE ([np][finalpunc] utterance)
;;;   *clause-rules*       gram1 declarative-clause layer (MAJOR-DECL-S ->
;;;                        subject / aux / main verb / VP-VERB crule ->
;;;                        VP-DONE / S-DONE)
;;;   *vp-np-rule*         gram1 VP-NP crule (object case-frame filling)
;;;
;;; LOAD this file from a test; it pulls in :parsifal and glang-cl.

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


(defparameter *np-rules*
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
      Drop c. Restore the buffer.}")
  "gram1 INITIAL-RULE + the gram3 NP-construction rules.")


(defparameter *np-utterance-rule*
  "{RULE NP-UTTERANCE IN SS-START
    [=np] [=finalpunc] -->
    Label c np-utterance.
    Attach 1st to c as np.
    Attach 2nd to c as finalpunc.
    The parse is finished.}"
  "gram1 NP-UTTERANCE: a single NP plus final punctuation is an utterance.")


(defparameter *clause-rules*
  '("{RULE MAJOR-DECL-S IN SS-START
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
    "{RULE OBJECTS IN SS-VP [=np] --> Attach 1st to c as np.}"
    "{RULE VP-DONE PRIORITY: 20 IN SS-VP [t] --> Drop c.}"
    "{RULE S-DONE IN SS-FINAL
      [=finalpunc] --> Attach 1st to c as finalpunc. Finalize the cf of c. Parse is finished.}")
  "gram1 declarative-clause layer: subject, aux, main verb (+ VP-VERB), finish.")


(defparameter *vp-np-rule*
  "{ATTACHMENT CRULE VP-NP VP OVER NP
    The lower node fills an obj slot of the upper.}"
  "gram1 VP-NP crule: fill the verb's object case slot with an attached NP.")


(defparameter *pronoun-rule*
  "{RULE PRONOUN IN npool
    [=pronoun] --> Attach 1st to c as pronoun.
    Label c pron-np, not-modifiable.
    Transfer ns, npl, n1p, n2p, n3p, wh from 1st to c.
    If 1st is relpron then label c relpron-np.
    If 1st is poss-pronoun then label c poss-np.
    Run np-done next.}"
  "gram5 PRONOUN: a pronoun becomes a pron-np (e.g. the imperative `you').")

(defparameter *imperative-rule*
  "{RULE IMPERATIVE IN SS-START
    [=tnsless] --> Label c imper, major.
    Insert the word 'you' into the buffer.
    Deactivate ss-start. Activate parse-subj.}"
  "gram1 IMPERATIVE: a clause-initial tnsless verb -> insert `you' subject.")

(defparameter *qp1-done-rule*
  "{RULE QUANT-DONE PRIORITY: 15 IN PARSE-QP-1
    [t] --> Deactivate parse-qp-1. Activate parse-adj.}"
  "gram3 QUANT-DONE: the no-determiner NP path (parse-qp-1 -> parse-adj).")

(defparameter *pp-rules*
  '("{RULE PP IN CPOOL
      [=prep] [=np] [** c; there is not a wh-comp or the wh-comp is utilized] -->
      Attach 1st to a new pp node as prep. Attach 2nd to c as np. Drop c.}"
    "{RULE PP-UNDER-VP-1 IN SS-VP
      [=pp] --> If 1st fits a pp slot of the cf of c then attach 1st to c as pp
        else run vp-done next.}"
    "{RULE PP-UNDER-S-1 IN SS-FINAL
      [=pp] --> If 1st fits a pp slot of the cf of c then attach 1st to c as pp
        else attach 1st to c as pp.}")
  "gram5 PP construction + attachment ([prep][np] -> pp; under VP or S).")


(defparameter *inf-complement-rules*
  '("{RULE TO-INFINITIVE PRIORITY: 7 IN PARSE-AUX
      [=*to, auxverb] [=tnsless] -->
      Label a new aux node inf. Attach 1st to c as to. Activate build-aux, cpool.}"
    "{RULE INF-S-START1 PRIORITY: 5 IN INF-COMP
      [=np] [=*to,auxverb] [=tnsless] -->
      Label a new s node sec, comp-s, inf-s. Attach 1st to c as np.
      Activate cpool, parse-aux.}"
    "{RULE OBJ-IN-EMBEDDED-S IN EMBEDDED-S-VP
      [=np] --> If 1st fits an obj slot of the cf of c then attach 1st to c as np
        else run embedded-vp-done next.}"
    "{RULE EMBEDDED-VP-DONE PRIORITY: 15 IN EMBEDDED-S-VP
      [t] --> Drop c. Activate embedded-s-final.}"
    "{RULE EMBEDDED-S-DONE PRIORITY: 20 IN EMBEDDED-S-FINAL
      [t] --> Finalize the cf of c. Drop c.}"
    "{RULE COMP-TO-NP IN CPOOL
      [=COMP-S] --> Attach 1st to a new NP node labelled comp-np, not-modifiable as s.
      Drop c into the buffer.}"
    "{ATTACHMENT CRULE NP-S NP OVER S
      If lower is inf-s then set the markers register of upper to !'(inf-comp).
      If lower is that-s then set the markers of upper to !'(that-comp).
      If lower is relative then lower fills a mod slot of the upper node
        else associate the case frame of lower with the upper node.}")
  "gram1/gram2 embedded infinitive-complement layer: build the embedded inf-S
   (TO-INFINITIVE, INF-S-START1, OBJ-IN-EMBEDDED-S, EMBEDDED-VP-DONE,
   EMBEDDED-S-DONE), then attach it to the matrix verb -- COMP-TO-NP wraps the
   finished comp-s in a comp-np and drops it to the buffer, where the clause
   layer's OBJECTS treats it as an object; the NP-S crule marks the comp-np
   inf-comp and associates the embedded case frame.")


(defparameter *raising-rules*
  '("{RULE PASSIVE-AUX IN BUILD-AUX
      [=*be] [=en] --> Attach 1st to c as passive. Label 2nd passive.}"
    "{RULE INSERT-TO-BE-1 IN TO-BE-LESS-INF-COMP
      [* is any of en, adj] -->
      Insert the word 'be' into the buffer before 1st.
      Insert the word 'to' into the buffer before 1st.}"
    "{RULE SEEMS IN NO-SUBJ
      [=*to] [=tnsless] --> Deactivate no-subj. Run passive next.}"
    "{RULE PASSIVE PRIORITY: 5 IN PASSIVE
      t --> Label the current s np-preposed.
      Create a new np node labelled trace, not-modifiable.
      Set the binding of c to the np of the current s.
      Drop c. Deactivate passive.}")
  "gram1/gram2 raising + passive layer. PASSIVE-AUX builds a passive aux
   ([be][en]); PASSIVE preposes the surface subject and leaves a bound trace
   in subject position; SEEMS (a no-subj raising verb) hands off to PASSIVE;
   INSERT-TO-BE-1 rewrites a bare predicate (`broken' -> `to be broken') so a
   to-be-less-inf-obj verb's complement parses as an embedded infinitive.
   Compose with *inf-complement-rules* for subject-raising complements.")


(defparameter *vp-np-full-rule*
  "{ATTACHMENT CRULE VP-NP VP OVER NP
    The lower node fills an obj slot of the upper.
    If the verb of the upper node is no-subj
        and the s above the upper node is not np-preposed
        then the np of the s above the upper node fills the subj slot of upper.
    If there is an s of lower and the np of it is delta then
        if the verb of the upper node is obj-binds-delta then
            set the binding of the np of the s of lower to the indirect object of upper
        else
       if the verb of the upper node is subj-binds-delta
           or
           the verb of the upper node is subj-less-inf-obj
           and it isn't true that
               there is a binding of the np of the s of the lower node
           then set the binding of the np of the s of lower
               to the np of the current s.
    If there is an s of lower and the np of it is delta then
        the np of the s of lower fills the subj slot of the s of lower;
        finalize the cf of the s of lower.}"
  "gram1 VP-NP, the COMPLETE Marcus crule (gram1:243-269): fills the verb's
   object slot, and -- for embedded clauses -- handles no-subj subject
   raising and DELTA-subject control binding (binds the embedded delta
   subject to the matrix subject for subj-less-inf-obj verbs like `want',
   then finalizes the embedded case frame). Use instead of the simpler
   *vp-np-rule* when delta/raising complements are in play; backward
   compatible (the extra clauses are guarded off for plain objects).")

(defparameter *delta-complement-rules*
  '("{RULE CREATE-DELTA-SUBJ-1 IN SUBJ-LESS-INF-COMP
      [=*to, auxverb] [=tnsless] -->
      Create an np node labelled trace, not-modifiable. Drop c into the buffer.}"
    "{RULE SUBJECT-IS-DELTA-DIAG PRIORITY: 15 IN EMBEDDED-S-FINAL
      [** c; the np of c is trace; the np of c is not delta;
            there is not a binding of the np of c] -->
      If there is a wh-comp and it is not utilized
          then set the binding of the np of c to wh-comp;
          the np of c fills the subj slot of the cf of c;
          label the wh-comp utilized
          else label the np of c delta.}"
    "{RULE DELTA-SUBJ-S-DONE PRIORITY: 15 IN EMBEDDED-S-FINAL
      [** C; THE NP OF C IS DELTA] -->
      Drop c into the buffer.}")
  "gram2 delta-subject (control) layer for subj-less-inf-obj verbs without an
   explicit embedded subject (\"the boy wants to go .\"): CREATE-DELTA-SUBJ-1
   drops a trace into the buffer to serve as the embedded subject;
   SUBJECT-IS-DELTA-DIAG labels that trace `delta' (when not wh-bound);
   DELTA-SUBJ-S-DONE drops the embedded S without finalizing its frame, so
   the *vp-np-full-rule* can later bind the delta to the matrix subject and
   finalize. Compose with *inf-complement-rules* + *vp-np-full-rule*.")


(defun register-grammar (&rest groups)
  "Compile, LINK, and register each rule in GROUPS. Each group is a rule
   source string or a list of them."
  (dolist (g groups)
    (dolist (src (if (listp g) g (list g)))
      (eval (glang-cl::link (glang-cl::compile-rule src))))))
