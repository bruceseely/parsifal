 (declare (setq fasload nil)
          (requiredf '((parsfl) @declr))
	  (fasload @parse)
	  (fasload @glang))
(declare (muzzled t))
(declare (eval (read)))
(glang-setup)

;;%THE PARSER, BY CONVENTION, INITIALLY CALLS THE RULE INITAL-RULE.%

{RULE INITIAL-RULE IN NOWHERE
[t] -->
Create a new s node.
!(setq s {the current s}). % So we have a handle on the topmost s from lisp.%
Activate cpool,ss-start.}

;;%A RULE TO GRAB SINGLE NPS AS UTTERANCES%

{RULE NP-UTTERANCE IN SS-START
[=np] [=finalpunc] -->
Label c np-utterance.
Attach 1st to c as np.
Attach 2nd to c as finalpunc.
The parse is finished.}

{RULE PP-UTTERANCE IN SS-START
[=pp] [=finalpunc] -->
Label c pp-utterance.
Attach 1st to c as pp.
Attach 2nd to c as finalpunc.
The parse is finished.}

;;%RULES TO INITIATE MAJOR CLAUSES.%
(comment Initiate major clauses)
{RULE MAJOR-DECL-S IN SS-START
[=np] [=verb] -->
Label c decl, major.
Deactivate ss-start. Activate parse-subj.}

{RULE YES-NO-Q IN SS-START
[=auxverb] [=np] -->
Label c quest, ynquest, major.
Deactivate ss-start. Activate parse-subj.}

{RULE IMPERATIVE IN SS-START
[=tnsless] -->
Label c imper, major.
Insert the word 'you' into the buffer.
Deactivate ss-start. Activate parse-subj.}

{RULE HAVE-DIAG PRIORITY: 5 IN SS-START
[=*have, tnsless] [=np] [t] -->
%If can't prove it's not a yes-no-q then assume it is.%
If 2nd is ns, n3p or 3rd is not verb or 3rd is tnsless
	then run imperative next else
	run yes-no-q next.}

{RULE WH-QUEST PRIORITY: 5 IN SS-START
[=wh] [=verb] -->
Label c major, quest, wh-quest.
Attach 1st to c as whcomp.
If 1st is a pp then label c pp-quest else
If 1st is a np then label c np-quest.
Deactivate ss-start. Activate parse-subj, wh-pool.}


(comment Subject parsing)
{RULE UNMARKED-ORDER IN PARSE-SUBJ
[=np] [=verb] -->
Attach 1st to c as np.
Deactivate parse-subj. Activate parse-aux.}

{RULE AUX-INVERSION IN PARSE-SUBJ
[=auxverb] [=np] -->
Attach 2nd to c as np.
Deactivate parse-subj. Activate parse-aux.}

{RULE SUBJ-QUEST? PRIORITY: 5 IN PARSE-SUBJ
[=verb]  [** c; * is np-quest] [=np] [t] -->
If 1st is not auxverb or 3rd is not verb
   then create a new np node labelled trace, not-modifiable;
	set the binding of c to wh-comp;
	drop c;
	label wh-comp utilized
   else run aux-inversion next.}

(comment RULES FOR BUILDING AUXILIARIES)
;;%RULES FOR BUILDING AUXILIARIES%
;;%predicate adjectives and prepositions should be parsed as verbs%

{RULE STARTAUX IN PARSE-AUX
[=verb] -->
Create a new aux node.
Transfer vspl, v1s, v+13s, vpl+2s, v-3s, v3s from 1st to c.
%(The above features are "person/number codes", e.g. "vpl+2s"
means that this verb goes with any plural or  2nd person singular
np as subject.  The verb "are" has this feature.)%
Transfer pres, past, future, tnsless from 1st to c.
Activate build-aux, cpool.}

{RULE TO-INFINITIVE PRIORITY: 7 IN PARSE-AUX
[=*to, auxverb] [=tnsless] -->
Label a new aux node inf.
Attach 1st to c as to.
Activate build-aux, cpool.}

{RULE AUX-ATTACH IN PARSE-AUX
[=aux] -->
Attach 1st to c as aux.
Activate parse-vp. Deactivate parse-aux.}

{RULE PERFECTIVE IN BUILD-AUX
[=*have] [=en] --> Attach 1st to c as perf.  Label c perf.}

{RULE PROGRESSIVE IN BUILD-AUX
[=*be] [=ing] --> Attach 1st to c as prog.  Label c prog.}

{RULE PASSIVE-AUX IN BUILD-AUX
[=*be] [=en] --> Attach 1st to c as passive.  Label 2nd passive.}

{RULE MODAL IN BUILD-AUX
[=modal] [=tnsless] --> Attach 1st to c as modal.  Label c modal.}

{RULE FUTURE IN BUILD-AUX
[=*will] [=tnsless] --> Attach 1st to c as will. Label c future.}

{RULE DO-SUPPORT IN BUILD-AUX
[=*do] [=tnsless] --> Attach 1st to c as do.}



{RULE AUX-COMPLETE PRIORITY: 15 IN BUILD-AUX
[t] --> Drop c into the buffer.}

{RULE THERE priority: 5 IN BUILD-AUX
[=*be] [=np]
[** c; the noun of the nbar of the binding of the np of the current s is *there]  -->
Label the current s existential.
Attach 2nd to the current s as np.}

(comment Verb processing)

{RULE main-verb IN PARSE-VP
[=verb] -->

%Set up state of S node%
Deactivate parse-vp.
If c is major then activate ss-final else
If c is sec then activate emb-s-final.

%Attach VP and V.%
Attach a new vp node to c as vp.
Attach 1st to c %which is now the vp% as verb.
Activate cpool.

%Activate packets to parse objects and complements.%
If there is a verb of c and it is passive
   then activate passive; run passive next.
%The following is close to an explicit system of verb types.%
If it is inf-obj then
   if it is to-less-inf-obj then activate to-less-inf-comp andthen
   if it is to-be-less-inf-obj then activate to-be-less-inf-comp andthen
   if it is 2-obj-inf-obj then activate 2-obj-inf-comp
        else activate inf-comp;
	if it is subj-less-inf-obj then activate subj-less-inf-comp else
	if it is no-subj then activate no-subj.
If it is that-obj then activate that-comp.


%Activate correct packet for regular objects.%
If there is a wh-comp and it is not utilized
   then activate wh-vp else
If the current s is major then activate ss-vp else
Activate embedded-s-vp.}




{ATTACHMENT CRULE VP-VERB  VP OVER VERB
Associate a new case frame with the upper node.
Associate the case frame of the upper node with the s above upper.
The aux of the s above upper fills the spec slot of  upper.      
The lower node fills the pred slot of  upper.
If the lower node is none of no-subj, passive
    and there is a binding of the np of the s above upper
    then it fills the subj slot of upper.}

{ATTACHMENT CRULE PRED-PP PRED OVER PP
Associate a new case frame with the upper node.
Associate the case frame of the upper node with the current s.
The aux of the current s fills the spec slot of the current s.
The prep of the lower node fills the pred slot of upper.
The np of the current s fills the subj slot of upper.
The np of the lower node fills the obj slot of upper.}

{RULE PREDP IN parse-verb
[* is any of pp %more to come%] -->
Deactivate parse-verb.
If c is major then activate ss-final else
If c is sec then activate emb-s-final.
Label 1st predp.
Attach 1st to c as predp.}

{RULE PASSIVE PRIORITY: 5 IN PASSIVE
%Activated by MVB if verb is labelled passive.%
t -->
Label the current s np-preposed.
Create a new np node labelled trace, not-modifiable.
Set the binding of c to the np of the current s.
Drop c.
Deactivate passive.}

{RULE SEEMS IN NO-SUBJ
[=*to] [=tnsless] -->
Deactivate no-subj.
Run passive next.}


(comment Parse simple objects)
;;%Parsing the objects of simple sentences%

{RULE OBJECTS IN SS-VP
[=np] -->
Attach 1st to c as np.}

{RULE VP-DONE PRIORITY: 20 IN SS-VP
[t] --> Drop c.}

{RULE S-DONE  IN SS-FINAL
[=finalpunc] -->
Attach 1st to c as finalpunc.
Finalize the cf of c.
Parse is finished.}

;;%Parsing the objects of embedded clauses.%

{RULE OBJ-IN-EMBEDDED-S IN EMBEDDED-S-VP
[=np] -->
If 1st fits an obj slot of the cf of c
	then attach 1st to c as np
      else run embedded-vp-done next.}

{ATTACHMENT CRULE VP-NP VP OVER NP
The lower node fills an obj slot of the upper.
%With no-subj verbs, can't know that the subject really is the subject until
we see the complement.%
If the verb of the upper node is no-subj
    and the s above the upper node is not np-preposed
    then the np of the s above the upper node
        fills the subj slot of upper.
%We can bind delta subjects and finalize the case frames of infinitive
clauses only after they are attached to the clause, allowing all the
wh-placement stuff to have done whatever it will do.
See the rules on infinitives with delta subjects for more on this.%
If there is an s of lower and the np of it is delta then
    if the verb of the upper node is obj-binds-delta then
        set the binding of the np of the s of lower
            to the indirect object of upper
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
    finalize the cf of the s of lower.}

{RULE EMBEDDED-VP-DONE PRIORITY: 15 IN EMBEDDED-S-VP
[t] -->
Drop c.
Activate embedded-s-final.}

{RULE EMBEDDED-S-DONE PRIORITY: 20 IN EMBEDDED-S-FINAL
[t] -->
Finalize the cf of c.
Drop c.}

