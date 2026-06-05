;;%NP CREATING RULES%
(comment Mainline np parsing)

{AS RULE STARTNP IN CPOOL
[=ngstart] -->
Create a new np node.
If 1st is det then activate parse-det
	else activate parse-qp-1.
Activate npool.}

{CREATION CRULE NP-START NP
if c is none of propn-np, pron-np, name, complex-noun-np, trace, *,
          comp-np
  and 1st is not pronoun %a hack patch%
          then associate a new case frame with c.}

;;this should be a finalize rule.
;;for the moment faked by having the attach noun guy send this
;;message as well.  
;;{ATTACHMENT CRULE NP-QP  NP OVER QP
;;The lower node fills the spec slot of upper.}

;THE PRIORITY OF THE NEXT RULE IS A CROCK.  THE FEATURE "NAME"
;GIVEN TO NAME NPS MUST BE CHANGED TO "NAME-NP" OR SUCH.  NOT
;DONE HERE FOR THE SAKE OF COMPATIBILITY WITH SEMANTICS.

{AS RULE POSSESSIVE-DET priority: 5 IN CPOOL
[=poss-np] -->
%While the rules that form poss-nps may be simple-minded, this
rule isn't too bad.%
Create a new det node labelled ngstart, poss-det, ns, npl, n3p, def.
Attach 1st to c as np.
Drop c into the buffer.
Restore the buffer.}

{RULE DETERMINER IN PARSE-DET
[=det] -->
Attach 1st to c as det.
Label c det.
Transfer the features indef, def, wh from 1st to c.
Deactivate parse-det. Activate parse-qp-2.}

{RULE A-HUNDRED-DIAG PRIORITY: 5 IN PARSE-DET
[=*a]
[=num, bignumg; the num1 of the num1 of * is any of *hundred, bignum] [t] -->
If 3rd is noun, ns, measure
    then run determiner next
    else attach 1st to the num1 of 2nd as num0;
	deactivate parse-det;
	activate parse-qp-1.}

{RULE ALL-THE PRIORITY: 5 IN PARSE-QP-1
[=*all] [=det,def] -->
%Another similar rule needed to cover "half the boys", "half A bottle"
Also there may be a timing problem with "all G-d's children".%
Insert the word 'of' into the buffer before 2nd.}

{RULE QUANT IN PARSE-QP-1
[=quant] -->
Attach a new qp node to c as qp.
Attach 1st to c as quant.
If 1st is num then label c numqp.
Transfer ns, npl from 1st to c.
Transfer the feature wh from 1st to the np above c.
Drop c.
Run quant-done next.}

{RULE QUANT-DONE PRIORITY: 15 IN PARSE-QP-1
[t] -->
Deactivate parse-qp-1. Activate parse-adj.}

{RULE NEXT-WEEK IN PARSE-QP-1
[=ord] [=noun, time] -->
%To handle "next tuesday", "next week", etc.%
%This and the following rule must be modified when the 
grammar is to handle "Next, take two eggs and ..."%
Attach a new qp node labelled ordqp to c as qp.
Attach 1st to c as ord.
Drop c.
Deactivate parse-qp-1. Activate parse-noun.}

{RULE ORDINAL IN PARSE-QP-2
[=ord] [t] -->
%Handles  "the first three boys", "the next three boys".%
Attach a new qp node labelled ordqp to c as qp.
Attach 1st to c as ord.
If 2nd is num then attach 2nd to c as num; 
	label c numqp;
	 transfer ns, npl from 1st to c
      else label c ns, npl.
Drop c.
Deactivate parse-qp-2. activate parse-adj.}

{RULE DET-QUANT IN PARSE-QP-2
[=quant; * is any of detq, num] -->
%There should be a less ad-hoc way than the feature "detq" to distinguish
quantifiers that can follow determiners (like "many") from those which don't
(like "some)...%
Attach a new qp node to c as qp.
Attach 1st to c as quant.
If 1st is num then label c numqp.
Transfer ns, npl from 1st to c.
Drop c.
Run det-quant-done next.}

{RULE DET-QUANT-DONE PRIORITY: 15 IN PARSE-QP-2
[t] -->
Deactivate parse-qp-2. Activate parse-adj.}

;;will need a rule that does "next <time> " when np stuff is nixed.



{RULE ADJ IN PARSE-ADJ
[t] -->
If 1st is adj then attach 1st to c as adj
      else if 1st is punc then attach 1st to c as punc
	else deactivate parse-adj; activate parse-noun.}

{ATTACHMENT CRULE NP-ADJ NP OVER ADJ
%Note that this does not handle non-attributive adjectives like "main"%
Associate a new mod case frame with the lower node.
The lower node fills a mod slot of the upper node.
The lower node fills the pred slot of lower.
Create a new * node labelled np.
Set the binding of c to the upper node.
C fills the subj slot of lower.
Finalize the cf of lower.
Drop c.}

{RULE NOUN IN PARSE-NOUN
[=noun] -->
Attach 1st to a new nbar node as noun.
Transfer the features massn, time, ns, npl,
	n1p, n2p, n3p from 1st to c.
Drop c.}

{RULE NBAR IN PARSE-NOUN
[=nbar] -->
Attach 1st to c as nbar.
If the noun of 1st is any of propnoun, pseudopropnoun then label c not-modifiable.
Transfer the features time, place, n1p, n2p, n3p from 1st to c.
Deactivate npool, parse-noun.
Drop c into the buffer.
Restore the buffer.}

{ATTACHMENT CRULE NP-NBAR NP OVER NBAR
Associate the case frame of the upper node with the lower node.
!'Something-or-other %(it doesn't matter what because of a hack
	in the semantics which uses this message to say that the det
	and qp are finished)% fills the spec slot of the upper node.
If there is a case frame of upper and
 there is a noun of the lower node 
	then it fills the pred slot of upper.}


(comment Incomplete nps)
{RULE INCOMPLETE-NP PRIORITY: 15 IN PARSE-NOUN
[t] -->
Attach a new nbar node to c as nbar.
Attach a new noun node labelled trace, not-modifiable, delta to c as noun.
Drop c.
Drop c.
Deactivate npool, parse-noun.
If there is a qp of c
      then label c quant-np, npl; activate quant-np-complete, cpool
	else drop c;
	restore the buffer.}

{RULE ALL-OF-THE-BOYS IN QUANT-NP-COMPLETE
[=pp; the prep of * is *of]  -->
Attach 1st to c as pp.
Set the binding of the noun of the nbar of c to
       the noun of the nbar of the np of 1st.
Label the np of 1st npl.
Run quant-np-done next.}

{RULE QUANT-NP-DONE PRIORITY: 15 IN QUANT-NP-COMPLETE
t -->
Deactivate quant-np-complete.
Activate np-complete.}


(comment NBAR-reactivation)

{NR RULE NBAR-COMPLETE IN CPOOL
[=nbar] -->
Activate cpool, nbar-complete.}

{RULE NBAR-DONE PRIORITY: 15 IN NBAR-COMPLETE
t -->
Drop c.
Restore the buffer.}

(comment Np-reactivation)
{NR RULE NP-COMPLETE IN CPOOL
[=np; * is none of modified, not-modifiable] -->
%NUMBER AGREEMENT%
If there is not a det of c and there is not a qp of c then
	if the nbar of c is npl then label c npl else
        if the noun of the nbar of c is  propnoun then label c prop-np else
	if the nbar of c is massn, ns then label c massnp else
        if the nbar of c is time and the ord of the qp of c is general-ord 
		then label c next-time-np else
      if the noun of the nbar of c is none of pseudopropnoun then
	label c bad.
If there is a det of c or there is a qp of c then
      transfer the meet of
      		 (if there is a det of c then the features of it else ns,npl)
		 and (if there is a qp of c then the features of it else ns,npl)
         from the nbar of c to c;
	if c is none of ns,npl then label c bad.
Activate cpool, np-complete.}

{RULE POSS-NP IN NP-COMPLETE
[=poss] -->
%This is the simple-minded possessive.  It assumes that the possessive
goes with the immediately previous NP, which is wrong for
	My aunt from Peoria's dog.%
Attach 1st to c as poss.
Label c poss-np.
Run np-done next.}

{RULE NP-DONE PRIORITY: 15 IN NP-COMPLETE
t -->
If there is a case frame of c then finalize the cf of c.
Drop c.
Restore the buffer.}

