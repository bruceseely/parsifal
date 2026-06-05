(comment Numbers)
;;%Rules to parse numbers%

{AS RULE NUMBER IN NPOOL
[=num ; * is not complete-num] -->
Deactivate npool.
Activate build-number.}

{RULE NUMBER-DONE PRIORITY: 12 IN BUILD-NUMBER
[t] -->
Label 1st complete-num.
If 1st is not ord then label 1st quant.
If 1st is none of ns,npl then label 1st npl.
Deactivate build-number.
Activate npool.
Restore the buffer.}

{RULE ORDINAL-DONE PRIORITY: 5 IN BUILD-NUMBER
[=ord] [** c; * is not num] -->
Run number-done next.}


{RULE NINETY-NINE IN BUILD-NUMBER
[=tens] [=ones] -->
Label a new num node 99s.
Attach 1st to c as num1.
Attach 2nd to c as num2.
Set the quant of c to
	plus(the quant register of 1st, the quant register of 2nd).
Transfer ord from 2nd to c.
Drop c.}

{RULE TWO-HUNDRED IN BUILD-NUMBER
[ * is any of ones, *ten, *10, 99s; * is not ord] [=*hundred] -->
Label a new num node hundred+.
Attach 1st to c as num1.
Attach 2nd to c as num2.
Set the quant  of c to
	times(the quant register of 1st, the quant register of 2nd).
Transfer ord from 2nd to c.
Drop c.}

{RULE HUNDRED IN BUILD-NUMBER
[=*hundred] -->
Label a new num node hundred+.
Attach 1st to c as num1.
Set the quant register of c to the quant register of 1st.
Transfer ord from 1st to c.
Drop c.}



{RULE HUNDREDS-STARTS-BIGNUMG IN BUILD-NUMBER
[ * is any of hundred+, bignum+; * is not ord] -->
Create a new num node labelled bignumg.
Attach 1st to c as num1.
Activate build-number.}

{RULE 99S-ATTACH IN BUILD-NUMBER
[t] [** c; = bignumg] -->
If there is a conj of c or 1st is any of 99s, tens, ones then 
	Attach 1st to c as num2;
	Transfer ord from 1st to c;
	Set the quant of c to
		plus (the quant register of num1 of c,
			the quant register of 1st)
	else set the quant register of c to the quant register of num1 of c.
Drop c.}

{RULE HUNDRED-AND IN BUILD-NUMBER
[=*and] [** c; =bignumg] -->
Attach 1st to c as conj.}

{RULE BIGNUM IN BUILD-NUMBER
[=num; * is not bignumg, ord] [=bignum; * is not *hundred] -->
Create a new num node labelled bignum+.
Attach 1st to c as num1.
Attach 2nd to c as num2.
Set the quant  of c to
	times(the quant register of 1st, the quant register of 2nd).
Transfer ord from  1st to c.
Drop c.}

{RULE TEN-TWENTY IN BUILD-NUMBER
[* is any of ones, 99s, *ten]
[* is any of 99s, tens; * is not ord] -->
Create a new num node labelled listnum.
Attach 1st to c as num1.
Activate build-number.}

{RULE TEN-TWENTY-FINISH IN BUILD-NUMBER
[** c; = listnum]
[* is any of 99s, tens; * is not ord] -->
Attach 1st to c as num2.
Set the quant register of c to
	plus (times (100, the quant register of num1 of c),
	      the quant register of 1st).
Drop c.}




;;%Special time np constructions%
(comment Time)

{RULE TWO-OCLOCK PRIORITY: 5 IN NPOOL
[=num] [ * is any of *o\'clock, *a\.m\., *p\.m\. ] -->
Create a new noun node labelled time, hour, complex-noun, propnoun.
Attach a new np node labelled time, complex-noun-np to c as np.
Attach 1st to c %which is now the np% as hour.
Activate build-hour.}

{RULE TWO-THIRTY PRIORITY: 5 IN NPOOL
[=num] [=*:] [=num] -->
Create a new noun node labelled time, hour, complex-noun, propnoun.
Attach a new np node labelled time, complex-noun-np to c as np.
Attach 1st to c %which is now the np% as hour.
Attach 2nd to c as colon.
Attach 3rd to c as minute.
Activate build-hour.}

{RULE HOUR-COMPLETE IN BUILD-HOUR
[t] -->
If 1st is any of *o\'clock, *a\.m\., *p\.m\. 
      then attach 1st to c as oclock.
Drop c %i.e. the complex-noun-np % .
%For the sake of semantics, all the following:%
Set the time register of c to 'hour'.
Label c hour.
Set the hours register of c
	to the quant register of the hour of the np of c.
Set the minutes register of c to
      (if there is a minute %node% of the np of c
	  then quant register of it else 0).
If there is an oclock %node% of the np of c 
      then set the time-of-day register of c to it
      else set the time-of-day register of c to the word 'o\'clock'.
Set the markers register of c to !'(time).
Finalize the cf of c.
Drop c.}

{RULE MONTH PRIORITY: 5 IN NPOOL
[=month; * is not complex-np] -->
Create a new noun node labelled time, complex-np, propnoun.
Attach a new np node labelled time, complex-noun-np to c as np.
Attach 1st to c as noun.
Activate npool, month-build.}

{RULE JUNE-1ST IN MONTH-BUILD
[=complete-num] -->
%Again, using fact that ORDs have NUM feature.%
Attach a new qp node to c as qp.
Attach 1st to c %i.e. the qp% as ord.
If 1st is not ord then label c ord.
Drop c.
Run month-complete next.}


{RULE JUNE-THE-1ST IN MONTH-BUILD
[=*the] [=ord] -->
Attach a new qp node to c as qp.
Attach 1st to c as det.
Attach 2nd to c as ord.
Drop c.      
Run month-complete next.}

{RULE THE-FIRST-OF-JUNE PRIORITY: 5 IN PARSE-QP-2
[=ord] [=*of] [=month] -->
Create a new noun node labelled time, complex-np, propnoun, ns.
Attach a new np node labelled time, complex-noun-np to c as np.
Attach a new qp node to c as qp;
	Attach 1st to c as ord;
	Drop c.
Attach 2nd to c as of.
Attach 3rd to c as noun.
Run month-complete next.}




{RULE MONTH-COMPLETE PRIORITY: 15 IN MONTH-BUILD
[t] -->
Drop c % i.e. the time np , so c is now the noun% .
Set the month register of c to the noun of the np of c.
Set the markers register of c to !'(time).
If there is not a qp of the np of c
	then set the time register of c to 'month'; label c month
	else set the time register of c to 'date'; label c date;
	set the day register of c to 
		the quant register of the ord of the qp of the np of c.
Finalize the cf of c.
Drop c.}

{RULE NUMBER-TO-YEAR IN QUANT-NP-COMPLETE
[** c; * is quant-np; * is not time; the quant of the qp of * is listnum]
 -->
Label c time,year.
Set the markers register of c to !'(time).
Set the time register of c to 'year'.
Set the year register of c to the quant register of the quant of the qp of c.}

{RULE MONDAY PRIORITY: 5 IN NPOOL
[=day-of-week; * is not complex-np] -->
Create a new noun node labelled time, complex-np, propnoun.
Attach a new np node labelled time, complex-noun-np to c as np.
Attach 1st to c as noun.
Finalize the cf of c.
Drop c %so c is now the noun%.
Set the time register of c to 'dow'.
Set the dow register of c to 1st.
Set the markers register of c to !'(time).
Drop c.}

;;The next rule (whatever it was for) probably won't work
;for the moment 12/19/77

;{AS RULE MONDAY-REGISTERS PRIORITY: 5 IN NPOOL
;[=noun, day-of-week; it isn't true that there is a time of *] -->
;%Sets some registers for semantics%
;Set the time register of 1st to 'dow'.
;Set the dow register of 1st to 1st.
;Restore the buffer.}

{NR RULE TIME-FINISH IN CPOOL
[=np, time] -->
Activate time-complete, cpool.}

{RULE MONDAY-THE-FIRST IN TIME-COMPLETE
[** c; * is dow] [=*\,]
    [=np,time; the noun of * is date] [* is not verb] -->
Attach 1st to c as comma.
Attach 2nd to c as date.
If 3rd is *\, then attach 3rd to c as comma.
Remove the feature dow from c.
Label c date.
Set the time register of the noun of c to 'date'.
Set the month register of the noun of c to
	the month register of the noun of 2nd.
Set the year register of the noun of c to
	the year register of the noun of 2nd.
Set the day register of the noun of c to
	the day register of the noun of 2nd.}

 {RULE JUNE-FIRST-1976 IN TIME-COMPLETE
[** c; the noun of * is date] [= *\,] [=np, the noun of * is year]
    [* is not verb] -->
Attach 1st to c as comma.
Attach 2nd to c as year.
If 3rd is *\, then attach 3rd to c as comma.
Set the year register of the noun of c to
	the year register of the noun of 2nd.}


{RULE TIME-DONE PRIORITY: 15 IN TIME-COMPLETE
t -->
Finalize the cf of c.
Drop c.
Restore the buffer.}

{RULE NUMBER-TO-TIME PRIORITY: 12 IN QUANT-NP-COMPLETE
[** c; :cheap-hacks; * is quant-np; * is not time] -->
%An ellision hack. Clearly special purpose for the PA domain.
A better approximation would be only to use this hack if the
incomplete np followed a prep that marked time, e.g. "on the first",
but even that, obviously, could be wrong.  Recovering ellisions, as
opposed to detecting them, really shouldn't be in the grammar.%
If there is a qp of c and it is ordqp %e.g. "the first"%
	then run month-complete next else
If it is numqp and there is not a det of the np above it
	then (if lessp (the quant register of the quant of it, 13) %e.g. "ten"%
		then run hour-complete next else
             if lessp (1900, the quant register of the quant of it, 20000) %e.g. "1976"%
		then run number-to-year next
	     else run np-done next)
else run np-done next.}

{RULE TIME-NP-TO-PP PRIORITY: 5 IN CPOOL
%This rule needs to be controlled, but right zeroeth approx%
[=np, time] -->
Insert the word 'during' into the buffer before 1st.}

