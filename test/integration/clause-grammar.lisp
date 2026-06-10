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
    "{ATTACHMENT CRULE VP-PP VP OVER PP
      The lower node fills a pp slot of the upper node.}")
  "gram5 PP construction + attachment ([prep][np] -> pp; under VP or S). The
   VP-PP attachment crule fires when a pp is attached to a VP and fills the
   verb's prepositional case from the pp's object (via the prep word ->
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


(defparameter *object-wh-rules*
  '("{RULE AUX-INVERSION IN PARSE-SUBJ
      [=auxverb] [=np] -->
      Attach 2nd to c as np.
      Deactivate parse-subj. Activate parse-aux.}"
    "{RULE SUBJ-QUEST? PRIORITY: 5 IN PARSE-SUBJ
      [=verb]  [** c; * is np-quest] [=np] [t] -->
      If 1st is not auxverb or 3rd is not verb
         then create a new np node labelled trace, not-modifiable;
              set the binding of c to wh-comp;
              drop c;
              label wh-comp utilized
         else run aux-inversion next.}"
    "{RULE DO-SUPPORT IN BUILD-AUX
      [=*do] [=tnsless] --> Attach 1st to c as do.}"
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
   *wh-question-rules*: SUBJ-QUEST? diagnoses, after WH-QUEST, whether the
   verb-initial buffer is a subject question (verb directly follows -> create
   the subject trace here) or subject-aux inversion (AUX-INVERSION attaches the
   post-aux NP as subject); DO-SUPPORT consumes inverted `do'/`did'. With the
   subject overt, the wh-comp's gap is in OBJECT position: MAIN-VERB activates
   wh-vp, WH-WITH-END-NEXT (the buffer is exhausted at the gap) runs
   CREATE-WH-TRACE to drop an object trace bound to the wh-comp, and WH-RESOLVED
   hands control back to ss-vp so OBJECTS attaches it. Compose with
   *wh-question-rules* + *pronoun-rule*.")


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
   (The richer non-stranding placement rules -- WH-WITH-NP-NEXT / -PP-NEXT /
   -NP-PP-NEXT, with their ditransitive semantic-preference logic -- are not
   included here.)")


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
   WH-QUEST fronts like `who'. Needs *pronoun-rule*.
   (`which' is NOT handled here: WHICH-DIAGN fires fine as a normal cpool rule,
   but a parsed which-question then needs the quantifier-phrase construction --
   `which' is diagnosed `quant' and wants a following noun -- which is not yet
   composed.)")


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
