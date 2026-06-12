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
        else attach 1st to c as pp.}"
    "{RULE PP-UNDER-VP-2 IN EMBEDDED-S-VP
      [=pp] --> If 1st fits a pp slot of the cf of c then attach 1st to c as pp
        else run embedded-vp-done next.}"
    "{RULE PP-UNDER-S-2 IN EMBEDDED-S-FINAL
      [=pp] --> If 1st fits a pp slot of the cf of c then attach 1st to c as pp
        else run embedded-s-done next.}"
    "{ATTACHMENT CRULE VP-PP VP OVER PP
      The lower node fills a pp slot of the upper node.}")
  "gram5 PP construction + attachment ([prep][np] -> pp; under VP or S, in both
   matrix and EMBEDDED clauses -- PP-UNDER-VP-2/PP-UNDER-S-2 are the embedded
   variants). The VP-PP attachment crule fires when a pp is attached to a VP and
   fills the verb's prepositional case from the pp's object (via the prep word ->
   ppcasegen), so PP arguments and stranded wh objects fill their case slot.")


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


(defparameter *perfect-rules*
  '("{RULE PERFECTIVE IN BUILD-AUX
      [=*have] [=en] --> Attach 1st to c as perf. Label c perf.}")
  "gram1 perfect aux layer (gram1:112). PERFECTIVE builds the perfect aux
   ([have][en], e.g. `have been'/`have scheduled'): it attaches `have' under
   the aux as perf and labels the aux perf. Stacks under PASSIVE-AUX in
   BUILD-AUX to make a perfect-passive aux (`have been scheduled'); no
   priority conflict (PERFECTIVE needs *have, PASSIVE-AUX needs *be).")


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


(defparameter *two-object-inf-rules*
  '("{RULE CREATE-DELTA-SUBJ IN 2-OBJ-INF-COMP
      [=*to, auxverb] [=tnsless] -->
      Create an np node labelled trace, not-modifiable, delta.
      Drop c into the buffer.
      Deactivate 2-obj-inf-comp. Activate inf-comp.}")
  "gram2 control layer for 2-obj-inf-obj verbs -- the verbs that take an NP
   object plus an infinitive complement whose subject is a DELTA: `persuade'
   (\"the boy persuaded the girl to go .\") and `promise' (\"the boy promised
   the girl to go .\"). Both are 2-obj-inf-obj (redund-implied from
   obj-binds-delta / subj-binds-delta -> inf-obj), so MAIN-VERB activates
   2-obj-inf-comp. The first object (`the girl') is attached by the clause
   layer's OBJECTS; then, with [to][tnsless] in the buffer, CREATE-DELTA-SUBJ
   drops a trace already labelled `delta' for the embedded subject and hands
   off to inf-comp so INF-S-START1 builds the embedded inf-S. Unlike the
   subj-less (`want') case the trace is born `delta', so SUBJECT-IS-DELTA-DIAG
   is a no-op and only DELTA-SUBJ-S-DONE (both in *delta-complement-rules*) is
   needed. This rule is verb-agnostic; the control distinction lives entirely
   in *vp-np-full-rule*: its obj-binds-delta arm binds the delta to the verb's
   INDIRECT object (persuade -> object control, `the girl' goes), its
   subj-binds-delta arm to the matrix subject (promise -> subject control, `the
   boy' goes). Compose with *inf-complement-rules* + *delta-complement-rules* +
   *vp-np-full-rule*.")


(defparameter *that-complement-rules*
  '("{RULE THAT-S-START PRIORITY: 5 IN CPOOL
      [=comp, *that] [=np] [=verb] -->
      Label a new s node sec, comp-s, that-s.
      Attach 1st to c as comp. Attach 2nd to c as np.
      Activate cpool, parse-aux.}"
    "{RULE THAT-S-START-1 PRIORITY: 5 IN THAT-COMP
      [=np] [=verb] -->
      Label a new s node sec, comp-s, that-s.
      Attach 1st to c as np. Activate cpool, parse-aux.}"
    "{RULE THAT-DIAG-1 IN CPOOL
      [=*that; * is none of comp, det, pronoun] [=np] -->
      If there is not a det of 2nd
              and there is not a qp of 2nd
              and the nbar of 2nd is none of npl, massn
              and 2nd is not not-modifiable
       then   attach 1st to 2nd as det;
              label 1st det, ns
       else   if c is a nbar then label 1st pronoun, relpron
       else   label 1st comp.}"
    "{RULE THAT-DIAG-2 PRIORITY: 13 IN CPOOL
      [=*that; * is not pronoun] -->
      Label 1st pronoun.}")
  "gram2 that-clause complement layer for that-obj verbs (\"the boy believes
   that the lecture meets .\"). `that' is det\\comp-ambig in the lexicon, so
   THAT-DIAG-1 diagnoses it (det if it could start the following NP, relpron
   under an NP, else comp); THAT-S-START then builds the embedded
   sec/comp-s/that-s with `that' as comp and the following NP as subject;
   THAT-S-START-1 covers the dropped-comp case (that-comp packet). The
   committed comp-attachment layer (COMP-TO-NP marks the comp-np that-comp via
   NP-S, OBJECTS attaches it) needs no addition. THAT-DIAG-2 is the
   that-as-pronoun fallback. Compose with *inf-complement-rules*.")


(defparameter *wh-question-rules*
  '("{RULE WH-QUEST PRIORITY: 5 IN SS-START
      [=wh] [=verb] -->
      Label c major, quest, wh-quest.
      Attach 1st to c as whcomp.
      If 1st is a pp then label c pp-quest else
      If 1st is a np then label c np-quest.
      Deactivate ss-start. Activate parse-subj, wh-pool.}"
    "{ATTACHMENT CRULE S-WHCOMP S OVER WHCOMP
      Set the :wh-comp of the upper node to the lower node.}"
    "{RULE CREATE-WH-TRACE PRIORITY: 14 IN WH-POOL
      T -->
      Create a new np node labelled trace, not-modifiable.
      Set the binding of c to the wh-comp.
      Label the wh-comp utilized.
      Drop c into the buffer.}"
    "{RULE WH-RESOLVED-1 PRIORITY: 5 IN WH-POOL
      [** c; the wh-comp is utilized] -->
      Deactivate wh-pool.}")
  "gram1/gram2 wh-question layer (subject wh-questions, \"who broke the jar ?\").
   WH-QUEST fronts the wh-element into the whcomp and (via the S-WHCOMP crule)
   the S's :wh-comp register; with the subject position empty, CREATE-WH-TRACE
   (the wh-pool catch-all) drops a trace bound to the wh-comp into the subject
   slot and marks the wh-comp utilized; WH-RESOLVED-1 then shuts off wh-pool.
   The trace's binding (the wh-element) is what fills the verb's subject case.
   Needs *pronoun-rule* (the wh-word `who' becomes a wh pron-np first). Object
   wh-questions (aux-inversion, SUBJ-QUEST?, the wh-vp placement rules) are not
   included here.")


(defun find-wh-comp (node)
  "Walk up from NODE: stop at the first NP (returning NIL -- an island
   boundary, Marcus's complex-NP constraint) or the first S (returning that
   S's :wh-comp register). Hand port of gram2's find-wh-comp -- Marcus wrote it
   in Lisp (\"this IS really a piece of the interpreter\") inside the grammar
   file. \"The node above X\" is the FATHER (one step up the active-node stack /
   tree); FATHER-NODE handles the at-creation, not-yet-attached case."
  (cond ((or (null node) (member 'np (fe node))) nil)
        ((member 's (fe node)) (getr :wh-comp node))
        (t (find-wh-comp (father-node node)))))

(defparameter *long-distance-wh-rules*
  '("{CREATION CRULE S-CREATE S
      Set the :wh-comp of c to find-wh-comp (the node above c)}")
  "gram2 long-distance wh layer -- the wh-comp INHERITANCE mechanism that lets a
   wh-element questioned in a matrix clause bind a gap inside an embedded clause
   (\"Who did you say that Bill told?\": `who' is the object of the embedded
   `told', extracted across `say that'). S-CREATE is a CREATION crule: every
   time an S node is created it sets that S's :wh-comp to FIND-WH-COMP of the
   node above it -- i.e. it copies the nearest enclosing S's wh-comp down into
   the new clause (unless an NP intervenes, the complex-NP island constraint).
   So when THAT-S-START builds the embedded that-S, S-CREATE seeds it with the
   matrix wh-comp; the embedded MAIN-VERB then sees a pending wh-comp and
   activates wh-vp, and WH-WITH-END-NEXT / CREATE-WH-TRACE spend it on the
   embedded verb's object -- a single shared wh-comp node, so marking it
   utilized in the embedded clause consumes it for the whole sentence. Needs
   FIND-WH-COMP (above). Harmless where there is no matrix wh-comp (it just
   copies NIL); compose with *that-complement-rules* + *inf-complement-rules* +
   *wh-question-rules* + *inversion-rules* + *object-wh-rules*.")


(defparameter *inversion-rules*
  '("{RULE AUX-INVERSION IN PARSE-SUBJ
      [=auxverb] [=np] -->
      Attach 2nd to c as np.
      Deactivate parse-subj. Activate parse-aux.}"
    "{RULE DO-SUPPORT IN BUILD-AUX
      [=*do] [=tnsless] --> Attach 1st to c as do.}")
  "gram1 subject-aux inversion + do-support, shared by object wh-questions and
   yes-no questions. AUX-INVERSION (parse-subj) attaches the post-aux NP as the
   subject of an inverted clause; DO-SUPPORT (build-aux) consumes an inverted
   `do'/`did' into the aux. Compose with *object-wh-rules* or *yes-no-rules*.")

(defparameter *object-wh-rules*
  '("{RULE SUBJ-QUEST? PRIORITY: 5 IN PARSE-SUBJ
      [=verb]  [** c; * is np-quest] [=np] [t] -->
      If 1st is not auxverb or 3rd is not verb
         then create a new np node labelled trace, not-modifiable;
              set the binding of c to wh-comp;
              drop c;
              label wh-comp utilized
         else run aux-inversion next.}"
    "{RULE WH-WITH-END-NEXT PRIORITY: 15 IN wh-vp
      t -->
      If the greatest possible number of objects of c is equal to 0
              or
          it isn't true that the wh-comp fits an obj slot of the cf of the current s
          and the verb of c is not comp-obj
              then run too-many-nps next
              else run create-wh-trace next.}"
    "{RULE WH-RESOLVED PRIORITY: 5 IN wh-vp
      [** c; the wh-comp is utilized ] -->
      Deactivate wh-vp.
      If the current s is major
              then activate ss-vp
              else activate embedded-s-vp.}")
  "gram1/gram2 object wh-question layer (\"who did the boy see ?\"). Builds on
   *wh-question-rules* + *inversion-rules*: SUBJ-QUEST? diagnoses, after
   WH-QUEST, whether the verb-initial buffer is a subject question (verb
   directly follows -> create the subject trace here) or subject-aux inversion
   (it runs AUX-INVERSION, from *inversion-rules*, to attach the post-aux NP as
   subject; DO-SUPPORT consumes the inverted `do'/`did'). With the subject
   overt, the wh-comp's gap is in OBJECT position: MAIN-VERB activates wh-vp,
   WH-WITH-END-NEXT (the buffer is exhausted at the gap) runs CREATE-WH-TRACE to
   drop an object trace bound to the wh-comp, and WH-RESOLVED hands control back
   to ss-vp so OBJECTS attaches it. Compose with *wh-question-rules* +
   *inversion-rules* + *pronoun-rule*.")

(defparameter *ditransitive-wh-rules*
  '("{RULE WH-WITH-NP-NEXT IN WH-VP
      [=np] -->
      If the greatest possible number of objects of c is less than 2
          then if the current s is major then run objects next
                   else run obj-in-embedded-s next
          else
      The number of objects of c will be 2;
      If semantics prefers 1st filling an obj slot of c somewhat
          better than wh-comp filling an obj slot of c then
              run objects next else
      If semantics prefers the wh-comp filling an obj slot of c much
          better than 1st filling an obj slot of c then
          label the current s slightly-bad;
          run create-wh-trace next else
      If semantics prefers 1st filling an obj slot of c no
          better than the wh-comp filling an obj slot of c then
          label the current s slightly-bad;
          run objects next else
          label the current s very-bad;
          run create-wh-trace next.}"
    "{RULE WH-WITH-NP-PP-NEXT PRIORITY: 7 IN WH-VP
      [=np] [=prep] -->
      If the greatest possible number of objects of c is greater than 1
          and a prepositional phrase of 2nd and the wh-comp fits a pp slot of c
          or
          the greatest possible number of objects of c is equal to 1
          and a prepositional phrase of 2nd and the wh-comp fits a pp slot of the current s
          then run objects next else
      If the greatest possible number of objects of c is greater than 1
          then run wh-with-np-next next else
      Run too-many-nps next.}"
    "{RULE WH-WITH-PP-NEXT PRIORITY: 5 IN WH-VP
      [=prep] [=np] -->
      If a prepositional phrase of 1st and 2nd fits a pp slot of c
          then run pp next else
      If it isn't true that
          a prepositional phrase of 1st and the wh-comp fits a pp slot of c
          then if the greatest possible number of objects of c is greater than 0
              then run create-wh-trace next
              else run too-many-nps next
          else
      If the lowest possible number of objects of c is greater than 0
          then run create-wh-trace next else
          run wh-pp-build next}"
    "{RULE TOO-MANY-NPS PRIORITY: 15 IN WP-VP
      [=np]
      [** c; the greatest possible number of objects of c is less than 2] -->
      if there is not a whcomp of the current s
          then run wh-resolved next
          else !(warn 1 too-many-nps loses).}")
  "gram2 ditransitive wh-vp placement (\"What did Bob give Sue?\"). When the
   wh-gap could land in either of a 2-object verb's slots, WH-WITH-NP-NEXT
   decides -- given an NP right after the verb -- whether to spend the wh-comp on
   that slot or attach the NP and keep the wh-comp for a later slot. It uses the
   `semantics prefers X filling an obj slot ... better than Y' comparisons
   (smqval over the verb's object markersets): if the overt NP fits an object
   slot at least as well as the wh-comp, attach the NP (run objects) and let
   WH-WITH-END-NEXT spend the wh-comp on the remaining slot; otherwise create the
   wh-trace now. WH-WITH-NP-PP-NEXT (priority 7, more specific) is the variant
   for when an NP is followed by a PREP -- e.g. \"What did Bob send Sue on
   friday?\", where after the verb the buffer is [Sue][on ...]. It first asks
   whether the wh-comp belongs IN that PP (does [2nd-prep + wh-comp] fit a pp
   slot? -- pgof + `fits a pp slot'); if so it attaches the NP and lets the
   wh-comp go to the PP. Otherwise the PP is a separate adjunct: with two object
   slots it delegates to WH-WITH-NP-NEXT (so the NP competes for an object slot
   and the adjunct PP attaches on its own via *pp-rules*), else it overflows to
   TOO-MANY-NPS. WH-WITH-PP-NEXT (priority 5, the most specific) is the variant
   for when a PREP comes first -- an in-situ PP after the verb, e.g. \"What did
   Bob change to friday?\", where after the verb the buffer is [to][friday]. If
   that PP is a plain pp that fits one of the verb's pp slots ([1st-prep + 2nd-NP]
   fits) it just builds it (run pp), leaving the wh-comp to fall to a verb object
   later; if instead the wh-comp itself is the prep's object (stranded-with-trace
   case) it routes to WH-PP-BUILD; the middle branches spend the wh-comp on a
   verb object (CREATE-WH-TRACE) or overflow. TOO-MANY-NPS is the overflow guard.
   Needs the verb's pp slots (e.g. change: time/loc via `to'; give: neut + dat).
   Compose with *object-wh-rules* (for WH-WITH-END-NEXT / WH-RESOLVED) +
   *wh-question-rules* + *inversion-rules*; the PP variants also need *pp-rules*
   (PP attach + VP-PP case fill) and *wh-pp-rules* (WH-PP-BUILD).
   (PP *fronting* -- pied-piped \"To whom did Bob give it?\" -- is a separate,
   not-yet-built construction: nothing assembles a clause-initial wh-PP for
   WH-QUEST's pp-quest branch to front.)")

(defparameter *there-rules*
  "{RULE THERE priority: 5 IN BUILD-AUX
    [=*be] [=np]
    [** c; the noun of the nbar of the binding of the np of the current s is *there] -->
    Label the current s existential.
    Attach 2nd to the current s as np.}"
  "gram1 existential `there' (\"is there a meeting ?\"). After a yes-no opener
   inverts the auxiliary `is'/`be' over the subject `there' (a pseudopropnoun
   NP whose noun is *there), THERE fires in build-aux when the next token is an
   NP: it relabels the clause `existential' and attaches that NP (the logical
   subject, e.g. `a meeting') as a second np of the S. The copula `be' then also
   serves as the clause's main verb. Compose with *yes-no-rules* + *inversion-rules*
   + *qp1-done-rule* (the bare `there' NP has no determiner).
   NB: the full canonical \"Is there a meeting scheduled for friday?\" further
   needs a passive-participle reduced relative (\"a meeting scheduled ...\"),
   which is a separate construction not covered here.")

(defparameter *yes-no-rules*
  "{RULE YES-NO-Q IN SS-START
    [=auxverb] [=np] -->
    Label c quest, ynquest, major.
    Deactivate ss-start. Activate parse-subj.}"
  "gram1 yes-no question opener (\"did the boy meet you ?\"). A clause-initial
   auxiliary followed by an NP labels the clause quest/ynquest/major and hands
   off to parse-subj, where AUX-INVERSION (from *inversion-rules*) attaches the
   post-aux NP as the subject and DO-SUPPORT folds the inverted `did' into the
   aux. No wh-comp -- a yes-no question has no fronted wh-element. Compose with
   *inversion-rules*.")


(defparameter *wh-pp-rules*
  '("{RULE WH-PP-BUILD IN CPOOL
      [=prep] [* is not np]
      [** c; there is a wh-comp and it is not utilized] -->
      Attach 1st to a new pp node as prep.
      Attach a new np node labelled trace to c as np.
      Set the binding of c to the wh-comp.
      Label the wh-comp utilized.
      Drop c %i.e. the trace%.
      Drop c %i.e. the pp%.}")
  "gram2 wh + preposition-stranding placement (\"who did the boy talk to ?\").
   This is the iconic harder wh-vp case: a stranded preposition at the end of
   the clause whose object is the fronted wh-element. WH-PP-BUILD fires in cpool
   on [prep][non-np] while a wh-comp is pending -- it builds the pp, drops a
   trace as the prep's object, binds the trace to the wh-comp, and marks the
   wh-comp utilized; PP-UNDER-VP-1 then attaches the pp under the VP. Compose
   with *wh-question-rules* + *object-wh-rules* (for aux-inversion) + *pp-rules*.
   (The richer placement rules WH-WITH-NP-NEXT / -NP-PP-NEXT / -PP-NEXT, with
   their semantic-preference logic, live in *ditransitive-wh-rules*;
   WH-WITH-PP-NEXT calls WH-PP-BUILD here for its stranded-with-trace branch.)")


(defparameter *proper-noun-rules*
  '("{AS RULE PROPNAME PRIORITY: 5 IN CPOOL
      [=name; * is not np] -->
      Create a np node labelled name, ns ,n3p, not-modifiable.
      Activate build-name.}"
    "{RULE NAME IN BUILD-NAME
      [=name] -->
      Attach 1st to c as noun.
      Set the names register of c to !(cons 1st {the names register of c}).}"
    "{RULE END-OF-NAME PRIORITY: 15 IN BUILD-NAME
      [t] -->
      If 1st is poss then attach 1st to c as poss; label c poss-np.
      If !(> (length {the names register of c}) 1) or there is a title of c
          then set the last-name register of c to !(car {the names register of c});
             set the names register of c to !(cdr {the names register of c}).
      Set the names register of c to !(nreverse {the names register of c}).
      Run np-done next.}")
  "gram5 proper-name layer (\"John\", \"Bob\", \"Sue\"). A [=name] word triggers
   the PROPNAME attention-shift (which needs `name' in *as-types* -- added to
   defs.lisp, since Marcus's list omits it), creating a not-modifiable name np;
   NAME attaches the name word(s) and accumulates the `names' register;
   END-OF-NAME finalises and drops the np (Run np-done next). PROPNAME's `* is
   not np' guard keeps the name np it builds from re-triggering the shift.")


(defparameter *wh-determiner-rules*
  '("{RULE WHAT-DIAG priority: 15 IN npool
      [=*what] [t] -->
      If 2nd is ngstart and 2nd is not det
          then label 1st det, ns, npl, n3p, wh; activate parse-det
          else label 1st pronoun, relpron, wh.}")
  "gram2 wh-determiner diagnosis for `what' (\"what did the boy break ?\").
   `what' is det\\relpron-ambig in the lexicon -- ambiguous between a
   determiner (\"what man\") and an interrogative pronoun (\"what did ...\").
   It carries only ngstart + det\\relpron-ambig, so it needs that ambiguity
   feature in *as-types* (added in defs.lisp) to fire STARTNP and reach this
   rule in npool; WHAT-DIAG then makes it a wh pronoun (when the next word is
   not a noun-group start) or a determiner. The result is a wh pron-np that
   WH-QUEST fronts like `who'. Needs *pronoun-rule'. (`which' is handled by
   *which-rules* + *quantifier-rules* instead -- it is diagnosed a `quant' and
   builds a quantifier-phrase wh-NP, not a bare pronoun.)")


(defparameter *quantifier-rules*
  '("{RULE QUANT IN PARSE-QP-1
      [=quant] -->
      Attach a new qp node to c as qp.
      Attach 1st to c as quant.
      If 1st is num then label c numqp.
      Transfer ns, npl from 1st to c.
      Transfer the feature wh from 1st to the np above c.
      Drop c. Run quant-done next.}"
    "{RULE DET-QUANT IN PARSE-QP-2
      [=quant; * is any of detq, num] -->
      Attach a new qp node to c as qp.
      Attach 1st to c as quant.
      If 1st is num then label c numqp.
      Transfer ns, npl from 1st to c.
      Drop c. Run det-quant-done next.}")
  "gram3 quantifier-phrase construction: a quant fills the qp slot of an NP.
   QUANT (parse-qp-1) handles a determiner-less leading quant (\"many boys\",
   \"which boy\") and propagates `wh' from the quant onto the NP (so a `which'
   phrase becomes a wh-NP that WH-QUEST can front); DET-QUANT (parse-qp-2)
   handles a post-determiner quant (\"the many boys\") for quants featured detq
   or num. Needs *qp1-done-rule* (QUANT-DONE) for the parse-qp-1 -> parse-adj
   handoff; DET-QUANT-DONE is in *np-rules*.")


(defparameter *number-rules*
  '("{AS RULE NUMBER IN NPOOL
      [=num ; * is not complete-num] -->
      Deactivate npool. Activate build-number.}"
    "{RULE NUMBER-DONE PRIORITY: 12 IN BUILD-NUMBER
      [t] -->
      Label 1st complete-num.
      If 1st is not ord then label 1st quant.
      If 1st is none of ns,npl then label 1st npl.
      Deactivate build-number. Activate npool. Restore the buffer.}"
    "{RULE ORDINAL-DONE PRIORITY: 5 IN BUILD-NUMBER
      [=ord] [** c; * is not num] --> Run number-done next.}"
    "{RULE NINETY-NINE IN BUILD-NUMBER
      [=tens] [=ones] -->
      Label a new num node 99s. Attach 1st to c as num1. Attach 2nd to c as num2.
      Set the quant of c to plus(the quant register of 1st, the quant register of 2nd).
      Transfer ord from 2nd to c. Drop c.}"
    "{RULE TWO-HUNDRED IN BUILD-NUMBER
      [ * is any of ones, *ten, *10, 99s; * is not ord] [=*hundred] -->
      Label a new num node hundred+. Attach 1st to c as num1. Attach 2nd to c as num2.
      Set the quant of c to times(the quant register of 1st, the quant register of 2nd).
      Transfer ord from 2nd to c. Drop c.}"
    "{RULE HUNDRED IN BUILD-NUMBER
      [=*hundred] -->
      Label a new num node hundred+. Attach 1st to c as num1.
      Set the quant register of c to the quant register of 1st.
      Transfer ord from 1st to c. Drop c.}"
    "{RULE HUNDREDS-STARTS-BIGNUMG IN BUILD-NUMBER
      [ * is any of hundred+, bignum+; * is not ord] -->
      Create a new num node labelled bignumg. Attach 1st to c as num1. Activate build-number.}"
    "{RULE 99S-ATTACH IN BUILD-NUMBER
      [t] [** c; = bignumg] -->
      If there is a conj of c or 1st is any of 99s, tens, ones then
          Attach 1st to c as num2;
          Transfer ord from 1st to c;
          Set the quant of c to plus (the quant register of num1 of c, the quant register of 1st)
          else set the quant register of c to the quant register of num1 of c.
      Drop c.}"
    "{RULE HUNDRED-AND IN BUILD-NUMBER
      [=*and] [** c; =bignumg] --> Attach 1st to c as conj.}"
    "{RULE BIGNUM IN BUILD-NUMBER
      [=num; * is not bignumg, ord] [=bignum; * is not *hundred] -->
      Create a new num node labelled bignum+. Attach 1st to c as num1. Attach 2nd to c as num2.
      Set the quant of c to times(the quant register of 1st, the quant register of 2nd).
      Transfer ord from 1st to c. Drop c.}"
    "{RULE TEN-TWENTY IN BUILD-NUMBER
      [* is any of ones, 99s, *ten] [* is any of 99s, tens; * is not ord] -->
      Create a new num node labelled listnum. Attach 1st to c as num1. Activate build-number.}"
    "{RULE TEN-TWENTY-FINISH IN BUILD-NUMBER
      [** c; = listnum] [* is any of 99s, tens; * is not ord] -->
      Attach 1st to c as num2.
      Set the quant register of c to plus (times (100, the quant register of num1 of c), the quant register of 1st).
      Drop c.}")
  "gram4 number grammar (gram4:4-104) -- builds a (possibly multi-word) number
   and lets it serve as a quantifier inside an NP (\"three books\", \"ninety
   nine boys\"). The NUMBER AS rule (keyed on `num', an as-type) fires inside an
   NP and shifts into the BUILD-NUMBER packet; the multi-word builders combine
   digit words by arithmetic on their `quant' registers -- NINETY-NINE (plus),
   TWO-HUNDRED / BIGNUM (times), HUNDREDS-STARTS-BIGNUMG / 99S-ATTACH / HUNDRED-AND
   (hundreds groups), TEN-TWENTY (listnum) -- and NUMBER-DONE finalises the
   assembled number, labelling it complete-num + quant + (default) npl and
   handing back to npool. From there the existing *quantifier-rules* QUANT rule
   (its `If 1st is num then label c numqp' branch) attaches the number as the
   NP's numqp. Self-contained: BUILD-NUMBER is a dedicated packet only active
   mid-number, so the group is inert for number-free sentences. Compose with
   *np-rules* + *quantifier-rules* + *qp1-done-rule*.")


(defparameter *temporal-adjunct-rules*
  '("{RULE TIME-NP-TO-PP PRIORITY: 5 IN CPOOL
      [=np, time] -->
      Insert the word 'during' into the buffer before 1st.}")
  "gram4 bare-temporal-adjunct rule (gram4:277). A bare time NP (\"yesterday\",
   \"monday\") can't fill a verb's TIME case positionally -- TIME is not an
   object case -- so TIME-NP-TO-PP rewrites it into a `during'-PP (`yesterday'
   -> `during yesterday'), which then attaches and fills TIME via `during'
   (cases-marked-by time). Used for \"you scheduled the meeting yesterday .\";
   the verb must carry a TIME case (e.g. schedule: cf (neut (time) (loc) agt)),
   and the time word must be a pseudopropnoun so the bare det-less NP is not
   flagged BAD by the NP-completion rule.

   DELIBERATELY NOT in *full-grammar*: Marcus flagged this rule \"needs to be
   controlled, but right zeroeth approx\", and it lives up to it -- [=np,time]
   fires on ANY clause-level time NP, including the object of an existing time
   PP, so it degrades sentences like \"schedule a meeting for friday .\" (it
   introduces a BAD node there). Compose it only for bare-time-adjunct sentences,
   with *np-rules* + *clause-rules* + *pp-rules* + *qp1-done-rule*.

   KNOWN LIMITATION: works for verbs with a native TIME slot (schedule), where
   the `during'-PP attaches under the VP and VP-PP fills TIME. For verbs WITHOUT
   one (give -- Marcus's literal \"give Sue yesterday\" examples) the PP attaches
   at the S level and TIME is not filled; that needs a universal-time-slot +
   S-level PP case-fill reconstruction, still deferred.")


(defparameter *day-of-week-rules*
  '("{RULE MONDAY PRIORITY: 5 IN NPOOL
      [=day-of-week; * is not complex-np] -->
      Create a new noun node labelled time, complex-np, propnoun.
      Attach a new np node labelled time, complex-noun-np to c as np.
      Attach 1st to c as noun.
      Finalize the cf of c.
      Drop c.
      Set the time register of c to 'dow'.
      Set the dow register of c to 1st.
      Set the markers register of c to !'(time).
      Drop c.}")
  "gram4 day-of-week date NP (gram4:202). A bare day name (`monday' ...
   `sunday', all `day-of-week') becomes a complex-noun TIME NP: MONDAY (an npool
   rule) builds a `complex-np' noun dominating a `complex-noun-np' over the day
   word, and records the semantics on the outer noun -- time register `dow', dow
   register = the day word, markers `(time)'. The result is a PROP-NP/TIME NP
   usable wherever a time NP is (a PP object, a bare adjunct, an utterance).
   Inert unless a day-of-week word appears. Compose with *np-rules* (+
   *np-utterance-rule* to parse one as a standalone utterance).")


(defparameter *month-date-rules*
  '("{RULE MONTH PRIORITY: 5 IN NPOOL
      [=month; * is not complex-np] -->
      Create a new noun node labelled time, complex-np, propnoun.
      Attach a new np node labelled time, complex-noun-np to c as np.
      Attach 1st to c as noun.
      Activate npool, month-build.}"
    "{RULE JUNE-1ST IN MONTH-BUILD
      [=complete-num] -->
      Attach a new qp node to c as qp. Attach 1st to c as ord.
      If 1st is not ord then label c ord.
      Drop c. Run month-complete next.}"
    "{RULE JUNE-THE-1ST IN MONTH-BUILD
      [=*the] [=ord] -->
      Attach a new qp node to c as qp. Attach 1st to c as det. Attach 2nd to c as ord.
      Drop c. Run month-complete next.}"
    "{RULE THE-FIRST-OF-JUNE PRIORITY: 5 IN PARSE-QP-2
      [=ord] [=*of] [=month] -->
      Create a new noun node labelled time, complex-np, propnoun, ns.
      Attach a new np node labelled time, complex-noun-np to c as np.
      Attach a new qp node to c as qp;
        Attach 1st to c as ord;
        Drop c.
      Attach 2nd to c as of. Attach 3rd to c as noun.
      Run month-complete next.}"
    "{RULE MONTH-COMPLETE PRIORITY: 15 IN MONTH-BUILD
      [t] -->
      Drop c.
      Set the month register of c to the noun of the np of c.
      Set the markers register of c to !'(time).
      If there is not a qp of the np of c
          then set the time register of c to 'month'; label c month
          else set the time register of c to 'date'; label c date;
          set the day register of c to the quant register of the ord of the qp of the np of c.
      Finalize the cf of c. Drop c.}")
  "gram4 month + date NP (gram4:142-191). A month name builds a complex-noun
   TIME NP; with an ordinal it becomes a DATE, otherwise a bare MONTH. MONTH (an
   npool rule) opens the month-build packet; the day is supplied in four shapes
   -- JUNE-1ST (\"june 1st\" / \"june first\"), JUNE-THE-1ST (\"june the 1st\"),
   and THE-FIRST-OF-JUNE (\"the first of june\", a parse-qp-2 rule) -- and
   MONTH-COMPLETE finalises: with a qp it records time `date', month register =
   the month word, day register = the ordinal's quant; with no qp, time `month'.
   The ordinal arrives as a `complete-num' from *number-rules* (which is in
   *full-grammar*), so `1st' (morpho: 1+st) and `first' both work. Not in
   *full-grammar* itself (months in other roles, `may' ambiguity); compose on
   top with *number-rules* present.")


(defparameter *which-rules*
  '("{RULE WHICH-DIAGN IN CPOOL
      [=*which; * is not any of quant, relpron] -->
      If there is an np above c and it is not modified
        then label 1st pronoun, relpron, wh, ngstart
        else label 1st quant, ngstart, ns, npl, wh.}")
  "gram2 diagnosis of `which'. `which' carries only the *which feature (its
   quantity is a register, not a feature), so it is invisible to the AS
   mechanism until this normal cpool rule fires: under an unmodified NP it is a
   relative pronoun (\"the boy which ...\"), otherwise an interrogative
   determiner/quant (\"which boy ...\"). The quant reading -- ngstart + wh --
   then triggers STARTNP and feeds *quantifier-rules* to build the wh-NP.")


(defparameter *relative-clause-rules*
  '("{RULE WH-RELATIVE-CLAUSE IN NP-COMPLETE
      [=relpron-np] [t] -->
      Label c modified.
      Attach a new s node labelled sec, relative to c as s.
      Activate cpool, parse-subj, wh-pool.
      Attach 1st to c as whcomp.
      Set the binding of 1st to the np above c.
      If 2nd is a verb then
          create a new np node labelled trace, not-modifiable;
          Set the binding of c to wh-comp;
          Label the binding of c utilized;
          Drop c into the buffer.}"
    "{RULE REDUCED-RELATIVE IN NP-COMPLETE
      [=np; * is not relpron-np][=verb] -->
      Insert the word 'wh-' into the buffer before 1st.}")
  "gram2 relative-clause layer (\"the boy who runs ...\"). When a completed NP
   is followed by a relative pronoun (a relpron-np), WH-RELATIVE-CLAUSE (in the
   NP-COMPLETE packet) labels the NP `modified', attaches a sec/relative S to
   it, makes the relpron the relative clause's whcomp BOUND to the head NP,
   and -- when a verb follows (a subject relative) -- drops a trace subject
   bound to the wh-comp. The relative S then parses as an ordinary embedded
   clause (reuses *clause-rules* + *inf-complement-rules*) and the NP-S crule
   (in *inf-complement-rules*) fills the head NP's mod slot. REDUCED-RELATIVE
   handles the no-relative-pronoun case (\"the boy you met\") by inserting a
   `wh-'. Needs *pronoun-rule* (who -> relpron-np) + *wh-question-rules*
   (S-WHCOMP / wh-pool). Relative clauses ARE wh-movement, so they reuse the
   whole wh-comp apparatus.")


(defun register-grammar (&rest groups)
  "Compile, LINK, and register each rule in GROUPS. Each group is a rule
   source string or a list of them."
  (dolist (g groups)
    (dolist (src (if (listp g) g (list g)))
      (eval (glang-cl::link (glang-cl::compile-rule src))))))


(defparameter *full-grammar*
  (list *np-rules* *np-utterance-rule* *clause-rules* *vp-np-full-rule*
        *pronoun-rule* *imperative-rule* *qp1-done-rule* *pp-rules*
        *inf-complement-rules* *raising-rules* *perfect-rules*
        *delta-complement-rules* *two-object-inf-rules* *that-complement-rules*
        *wh-question-rules* *inversion-rules* *object-wh-rules*
        *ditransitive-wh-rules* *there-rules* *yes-no-rules* *wh-pp-rules*
        *proper-noun-rules* *wh-determiner-rules* *quantifier-rules*
        *which-rules* *relative-clause-rules* *long-distance-wh-rules*
        *number-rules*)
  "EVERY validated rule group above, composed into one grammar -- the whole
   grammar the per-construction integration tests have built up, registered
   together instead of curated per test. Uses the COMPLETE *vp-np-full-rule*
   (with delta/raising binding); *vp-np-rule* is deliberately omitted because
   it would collide with it on the rule name VP-NP -- they are the only two
   groups that cannot coexist. The union composes with no parse-time conflicts:
   gram1's INITIAL-RULE dispatches each sentence type to the right packets and
   the diagnostic rules (THAT-DIAG, WHICH-DIAGN, SUBJ-QUEST?, REDUCED-RELATIVE,
   ...) stay disjoint. See `gram1-full-grammar-test'.")

(defun load-full-grammar ()
  "Reset the rule table and register *FULL-GRAMMAR* as a single composed
   grammar. After this one call, PARSE-SENTENCE parses ANY supported
   construction with no per-test rule-group selection -- the whole-grammar
   load path. (PARSE-SENTENCE resets per-parse state, so the same loaded
   grammar can parse many sentences in one image.)"
  (reset-rule-table)
  (apply #'register-grammar *full-grammar*))
