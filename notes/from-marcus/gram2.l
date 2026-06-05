;;Rules to start relative clauses
(comment Relative clauses)

{RULE WH-RELATIVE-CLAUSE IN NP-COMPLETE
[=relpron-np] [t] -->
Label c modified.
Attach a new s node labelled sec, relative to c as s.
Activate cpool, parse-subj, wh-pool.
%Note that the node is attached as "whcomp"; "wh-comp" refers to
the special wh register.%
Attach 1st to c as whcomp.
Set the binding of 1st to the np above c.
If 2nd is a verb then 
	create a new np node labelled trace, not-modifiable;
	Set the binding of c to wh-comp;
	Label the binding of c utilized;
	Drop c into the buffer.}

{RULE REDUCED-RELATIVE IN NP-COMPLETE
[=np; * is not relpron-np][=verb] -->
Insert the word 'wh-' into the buffer before 1st.}

{RULE WHICH-DIAGN IN CPOOL
[=*which; * is not any of  quant, relpron] -->
If there is an np above c and it is not modified then
	label 1st pronoun, relpron, wh, ngstart
	else label 1st quant, ngstart, ns, npl, wh.}

{RULE WHAT-DIAG priority: 15 IN npool
[=*what] [t] -->
%This won't work for cleft sentences: what little fish eat is little worms
(but are these garden paths?)%
If 2nd is ngstart and 2nd is not det
    then label 1st det, ns, npl, n3p, wh;
    activate parse-det
    else label 1st pronoun, relpron, wh.}



(comment The wh-comp mechanism of the grammar interpreter)

{ATTACHMENT CRULE S-WHCOMP S OVER WHCOMP
%This crule and the next should really be a part of the grammar
interpreter, hardwired and universal.  I put them here so that 
they are clearly visible.  The effect is exactly equivalent.
This rule primes the wh-comp mechanism.%
Set the :wh-comp of the upper node to the lower node.}

{CREATION CRULE S-CREATE S
%This is ugly and adhoc.  It simply embodies a form of
the complex NP constraint, which is adhoc and ugly.
That this rule should be a putative universal doesn't help much.
The rule: scan up the active node stack until you come to either an NP
or an S.  If an NP, that's it.  If an S, copy its wh-comp.  This IS
really a piece of the interpreter, and must be written in LISP.%

Set the :wh-comp of c to find-wh-comp (the node above c)}


(defun find-wh-comp (node-variable)
;Now into PIDGIN
{if there is not a node-variable or
    the node-variable is an np then nil else
if the node-variable is an S
    then the :wh-comp register of the node-variable else
find-wh-comp (the node above node-variable)})

(comment WH placement)

{RULE WH-WITH-END-NEXT PRIORITY: 15 IN wh-vp
t -->
If the greatest possible number of objects of c is equal to 0
        or
    it isn't true that the wh-comp fits an obj slot of the cf of the current s
    and the verb of c is not comp-obj
	then run too-many-nps next
	else run create-wh-trace next.}

{RULE WH-WITH-NP-NEXT IN WH-VP
[=np] -->
%This rule decides whether or not to use the wh-comp if there is an NP
next.  Note that if the NP is followed by a PP, then a different rule
will run.  Running an object rule next indicates that the wh-comp is not
to be used, while running create-wh-trace indicates the wh-comp is to be
used.%

If the greatest possible number of objects of c is less than 2
    %i.e. the current verb can only take 1 object%
    then if the current s is major then run objects next
             else run obj-in-embedded-s next
    else
%There are 2 object slots available; we'll use both.%
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
    run create-wh-trace next.}

{RULE WH-WITH-NP-PP-NEXT PRIORITY: 7 IN WH-VP
[=np] [=prep] -->
If the greatest possible number of objects of c is greater than 1
    and a prepositional phrase of 2nd and the wh-comp fits a pp slot of c
    or
    the greatest possible number of objects of c is equal to 1
    and a prepositional phrase of 2nd and the wh-comp fits a pp slot of the current s
    then run objects next else
If the greatest possible number of objects of c is greater than 1
    then run wh-with-np-next next else
Run too-many-nps next.}



{RULE WH-WITH-PP-NEXT PRIORITY: 5 IN WH-VP
[=prep] [=np] -->
If a prepositional phrase of 1st and 2nd fits a pp slot of c 
%The syntactic preference is for simple pps.%
    then run pp next else
If it isn't true that
    a prepositional phrase of 1st and the wh-comp fits a pp slot of c
    %i.e it's true that the wh-comp can't serve as the object of the prep%
    then if greatest possible number of objects of c is greater than 0
        %i.e. if the wh-comp can serve as an object then%
	then run create-wh-trace next
	else %Hopefully the trace is a time word%
	     run too-many-nps next
    else
If the lowest possible number of objects of c is greater than 0
    %i.e. this verb MUST take an object%
    then run create-wh-trace next else
    run wh-pp-build next}


{RULE WH-PP-BUILD IN CPOOL
[=prep] [* is not np]
[** c; there is a wh-comp and it is not utilized] -->
Attach 1st to a new pp node as prep.
Attach a new np node labelled trace to c as np.
Set the binding of c to the wh-comp.
Label the wh-comp utilized.
Drop c %i.e. the trace%.
Drop c %i.e. the pp%.}

{RULE WH-WITH-PP-2 IN CPOOL
[=prep] [=np]
[** c; there is a wh-comp and it is not utilized] -->
If c is any of nbar, np and
     a prepositional phrase of 1st and 2nd fits a pp slot of c
     then run pp next %building the simple pp%
     else
If a prepositional phrase of 1st and the wh-comp fits a pp slot of c
    then run wh-pp-build next %building a pp with the wh-comp% else
%the current constituent is done%
If c is an nbar then run nbar-done next else
If c is an np then run np-done next else
run pp next.}


{RULE WH-RESOLVED PRIORITY: 5 IN wh-vp
[** c; the wh-comp is utilized ] -->
Deactivate wh-vp.
If the current s is major
	then activate ss-vp
	else activate embedded-s-vp.}

{RULE TOO-MANY-NPS PRIORITY: 15 IN WP-VP
[=np] 
[** c; the greatest possible number of objects of c is less than 2] -->
%This is just a start, clearly%
if there is not a whcomp of the current s
    %i.e the wh-comp originated from higher up%
    then run wh-resolved next
    else !(warn 1 too-many-nps loses).}

{RULE CREATE-WH-TRACE PRIORITY: 14 IN WH-POOL
T -->
Create a new np node labelled trace, not-modifiable. 
Set the binding of c to the wh-comp. 
Label the wh-comp utilized. 
Drop c into the buffer.}

{RULE WH-RESOLVED-1 PRIORITY: 5 IN WH-POOL
[** c; the wh-comp is utilized] -->
Deactivate wh-pool.}


;;Rules for that clauses
(comment That clauses)

{RULE THAT-S-START PRIORITY: 5 IN CPOOL
[=comp, *that] [=np] [=verb] -->
%Handles the marked case;always active.%
Label a new s node sec, comp-s, that-s.
Attach 1st to c as comp.
Attach 2nd to c as np.
Activate cpool, parse-aux.}

{RULE THAT-S-START-1 PRIORITY: 5 IN THAT-COMP
[=np] [=verb] -->
Label a new s node sec, comp-s, that-s.
Attach 1st to c as np.
Activate cpool, parse-aux.}

{RULE COMP-TO-NP IN CPOOL
[=COMP-S] -->
Attach 1st to a new NP node labelled comp-np, not-modifiable as s.
Drop c into the buffer.}

{RULE that-diag-1 in cpool
[=*that; * is none of comp, det, pronoun] [=np] -->
%This rule diagnoses whether "that" is a comp or a determiner or a
relative pronoun.  This rule will misdiagnose
"That deer ate everything in my garden surprised me."
but then again so did you.
The diagnostic is: if "that" must be a det, it is.
If c is nbar, i.e. we're under an np, assume "that" starts a relative clause.%
If there is not a det of 2nd
        and there is not a qp of 2nd
	and the nbar of 2nd is none of npl, massn
	and 2nd is not not-modifiable
 then   attach 1st to 2nd as det;
        label 1st det, ns
 else   if c is a nbar then label 1st pronoun, relpron
 else   label 1st comp.}

{RULE THAT-DIAG-2 PRIORITY: 13 IN CPOOL
%If there's nothing else to do with that, it's a pronoun.%
[=*that; * is not pronoun] -->
Label 1st pronoun.}


{RULE THAT-DIAG-3 PRIORITY: 5 IN NBAR-COMPLETE
[=*that; * is none of pronoun, comp] [=np] 
[** c; the verb of the vp of the current s is that-obj;
the lowest possible number of objects of the current s is equal to 2]
-->
Label 1st comp.}

;;%Rules to do infinitive clauses.%
(comment Infinitive clauses)

{RULE TO-DIAG PRIORITY: 5 IN INF-COMP, 2-OBJ-INF-COMP
[=*to, auxverb, prep] [=tnsless]
[** c; there is a wh-comp and it is not utilized] -->
%Does this really belong in this packet or in cpool? 
Should the fact that an inf-comp is expected serve as evidence?%
if a pp of 1st and wh-comp
	fits a pp slot of the cf of the current s
then remove the feature auxverb from 1st 
else remove the feature prep from 1st.}

{RULE INF-S-START PRIORITY: 5 IN CPOOL 
[=*for] [=np] [=*to] -->
%Handles the marked case; always active%
Label a new s node sec, comp-s, inf-s.
Attach 1st to c as comp.
Attach 2nd to c as np.
Activate cpool, parse-aux.}

{RULE INF-S-START1 PRIORITY: 5 IN INF-COMP
%The COMP can only be dropped if the complement is expected.%
[=np] [=*to,auxverb] [=tnsless] -->
Label a new s node sec, comp-s, inf-s.
Attach 1st to c as np.
Activate cpool, parse-aux.}

{RULE INSERT-TO IN TO-LESS-INF-COMP
%This rule and INSERT-TO-BE must be in different packets 
- consider "help" vs. "seems"%
[=np] [=tnsless] -->
Insert the word 'to' into the buffer before 2nd.}

{RULE INSERT-TO-1 IN TO-LESS-INF-COMP
%I've clearly missed a generalization somewhere.%
[=tnsless] -->
Insert the word 'to' into the buffer before 1st.}

{RULE INSERT-TO-BE IN TO-BE-LESS-INF-COMP
[=np] [* is any of en, adj] -->
%won't work for adjectives until interpreter changed%
Insert the word 'be' into the buffer before 2nd.
Insert the word 'to' into the buffer before 2nd.}

{RULE INSERT-TO-BE-1 IN TO-BE-LESS-INF-COMP
[* is any of en, adj] -->
%Again, I've missed the generalization%
Insert the word 'be' into the buffer before 1st.
Insert the word 'to' into the buffer before 1st.}



(comment Infinitives with delta subjects)

{RULE CREATE-DELTA-SUBJ IN 2-OBJ-INF-COMP
[=*to, auxverb] [=tnsless] -->
%This rule handles verbs like promise and persuade.  The analysis
I'm adopting assumes that the subject of the complement is a delta
(i.e. a "base-generated" trace), which is not bound by syntactic rules, 
but by later semantic processing.  The binding is done in the crule
vp-np%
Create an np node labelled trace, not-modifiable, delta.
Drop c into the buffer.
Deactivate 2-obj-inf-comp. Activate inf-comp.}

{RULE create-delta-subj-1 IN SUBJ-LESS-INF-COMP
[=*to, auxverb] [=tnsless] -->
%Handles verbs like "want" that may have an explicit subject
or may have a delta subject.  These cases are hard
if they interact with wh-movement.%
Create an np node labelled trace, not-modifiable.
Drop c into the buffer.}

{RULE SUBJECT-IS-DELTA-DIAG PRIORITY: 15 IN EMBEDDED-S-FINAL
[** c; the np of c is trace; the np of c is not delta;
      there is not a binding of the np of c] -->
%This rule finishes the analysis of embedded clauses of verbs like 
"want", deciding whether the subject is a bound trace or a delta.%
If there is a wh-comp and it is not utilized
    then set the binding of the np of c to wh-comp;
    %This next belongs in the case mechanism, but its simpler here.
    Life would be simpler if "want" took 2 objects.%
    the np of c fills the subj slot of the cf of c;
    label the wh-comp utilized
    else label the np of c delta.}

{RULE DELTA-SUBJ-S-DONE PRIORITY: 15 IN EMBEDDED-S-FINAL
[** C; THE NP OF C IS DELTA] -->
%This rule overrides embedded-s-done; and does not finalize the cf.%
Drop c into the buffer.}

{ATTACHMENT CRULE NP-S  NP OVER S
If lower is inf-s then set the markers register of upper to !'(inf-comp).
If lower is that-s then set the markers of upper to !'(that-comp).
If lower is relative
	then lower fills a mod slot of the upper node
	else associate the case frame of lower with the upper node.}

