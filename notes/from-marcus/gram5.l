;;%Pronouns, proper names, etc.%
(comment Pronouns proper names etc)

{RULE PRONOUN IN npool 
[=pronoun] -->
Attach 1st to c as pronoun.
Label c pron-np, not-modifiable.
Transfer ns, npl, n1p, n2p, n3p, wh from 1st to c.
If 1st is relpron then label c relpron-np.
If 1st is poss-pronoun then label c poss-np.
Run np-done next.}

{AS RULE PROPNAME PRIORITY: 5 IN CPOOL
[=name; * is not np] -->
Create a np node labelled name, ns ,n3p, not-modifiable.
Activate build-name.}

{AS RULE TITLE PRIORITY: 5 IN CPOOL
[=title] -->
Create a np node labelled name, ns, n3p, not-modifiable.
Attach 1st to c as title.
Set the title register of c to 1st. %set register for PA semantics%
Activate build-name.}         


{RULE NAME IN BUILD-NAME
[=name] -->
Attach 1st to c as noun.
Set the names register of c to
	!(cons 1st {the names register of c}). %for pa semantics%}

{RULE END-OF-NAME PRIORITY: 15 IN BUILD-NAME
[t] -->
If 1st is poss %i.e. possessive i.e. "'s"% then
   attach 1st to c as poss; label c poss-np.
If !(> (length {the names register of c}) 1) 
    	or there is a title of c
	then set the last-name register of c to
		!(car {the names register of c});
	   set the names register of c to
		!(cdr {the names register of c}). %for pa semantics%
Set the names register of c to
	!(nreverse {the names register of c}).
Run np-done next.}

{AS RULE PROPNOUN PRIORITY: 5 IN CPOOL
[=propnoun; * is not name] -->
Attach 1st to a new np node labelled propn-np, ns, n3p, not-modifiable as noun.
Run np-done next.}


(comment Pp attachment rules)
;;%PP rules%
{RULE PP IN CPOOL
[=prep] [=np]
[** c; there is not a wh-comp or the wh-comp is utilized]
%for WH-placement PP rules, see WH-placement rules%
 -->
Attach 1st to a new pp node as prep.
Attach 2nd to c as np.
Drop c.}

{RULE NBAR-PP-UNDER-S IN NBAR-COMPLETE
[=pp] [** c; the node above the np above * is s] -->
If 1st fits a pp slot of the cf of c
	then attach 1st to c as pp
	else run nbar-done next.}

{RULE OF-PP IN NBAR-COMPLETE
[=pp; the prep of * is *of] -->
Attach 1st to c as pp.}

{RULE NBAR-PP-UNDER-VP IN NBAR-COMPLETE
[=pp] [** c; the node above the np above * is vp] -->
If 1st fits a pp slot of the cf of the node above the np above c
	then run nbar-done next
	else run nbar-pp-under-s next.}

{ATTACHMENT CRULE NBAR-PP NBAR OVER PP
If the prep of the lower node is *of
	then the np of the lower node fills a obj slot of the upper node
	else the lower node fills a pp slot of the upper node.}


{RULE NP-PP-UNDER-VP IN NP-COMPLETE
[=pp] [** c; the node above * is vp] -->
if it isn't true that 1st fits a pp slot of the cf of c 
	or 1st fits a pp slot of the cf of the vp above c
then run np-done next
else attach 1st to c as pp.}

{RULE NP-PP-UNDER-PP IN NP-COMPLETE
[=pp] [** c ; the node above * is pp] -->
%For now, NPs only get a PP if the above VP or S does't want it.
which is wrong, but a zeroeth order approximation.
Will get wrong "Can we meet on the friday after the party"
Unless there is a notion of "mustmod"%

%For now,% if the node above the pp above c is vp
	then run np-pp-under-vp next else
if the node above the pp above c is s
	and 1st fits a pp slot of the cf of the s above the pp above c
then run np-done next
else attach 1st to c as pp.}


{RULE NP-PP-DEFAULT PRIORITY: 15 IN NP-COMPLETE
[=pp] -->
%This module is intended to run only when the NP is clause initial.%
Attach 1st to c as pp.}

{ATTACHMENT CRULE NP-PP NP OVER PP
Associate a new mod case frame with the lower node.
The lower node fills a mod slot of the upper node.
The prep of the lower node fills the pred slot of lower.
The np of lower fills an obj slot of lower.
Create a new * node labelled np.
Set the binding of c to the upper node.
C fills the subj slot of lower.
Finalize the cf of lower.
Drop c.}

{RULE PP-UNDER-VP-1 IN SS-VP
[=pp] -->
If 1st fits a pp slot of the cf of c
	then attach 1st to c as pp
else run vp-done next.}

{RULE PP-UNDER-S-1 IN SS-FINAL
[=pp] -->
If 1st fits a pp slot of the cf of c
      then attach 1st to c as pp
else %what else can we do?% attach 1st to c as pp.}

{RULE PP-UNDER-S-2 IN EMBEDDED-S-FINAL
[=pp] -->
If 1st fits a pp slot of the cf of c
      then attach 1st to c as pp
else run embedded-s-done next.}

{ATTACHMENT CRULE S-PREDP S OVER PREDP
If the lower node is pp then
	Associate a new case frame with the lower node;
	The prep of the lower node fills the pred slot of lower;
	The np of lower fills an obj slot of lower.
Associate the case frame of lower with the s above lower.
The np of the s above lower fills the subj slot of lower.}

{ATTACHMENT CRULE S-PP  S OVER PP
Associate a new mod case frame with the lower node.
The lower node fills a mod slot of the upper node.
The prep of the lower node fills the pred slot of lower.
The np of lower fills an obj slot of lower.
Create a new * node labelled s.
Set the binding of c to the upper node.
C fills the subj slot of lower.
Finalize the cf of lower.
Drop c.}

{CREATION CRULE *-DUMMY *
%All * nodes should disappear when dropped, so we make them look attached.%
Set the father of c to t.}

{RULE PP-UNDER-VP-2 IN EMBEDDED-S-VP
[=pp] -->
If 1st fits a pp slot of the cf of c
	then attach 1st to c as pp
else run embedded-vp-done next.}

{ATTACHMENT CRULE VP-PP  VP OVER PP
The lower node fills a pp slot of the upper node.}

