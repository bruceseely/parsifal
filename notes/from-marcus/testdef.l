From MAILER-DAEMON Sun Aug 19 23:41:23 2001
Date: Sun, 19 Aug 2001 23:41:23 -0400 (EDT)
From: Mail System Internal Data <MAILER-DAEMON@linc.cis.upenn.edu>
Subject: DON'T DELETE THIS MESSAGE -- FOLDER INTERNAL DATA
X-IMAP: 0998278883 0000000002
Status: RO

This text is part of the internal format of your mail folder, and is not
a real message.  It is created automatically by the mail system software.
If deleted, important folder data will be lost, and it will be re-created
with the data reset to initial values.

From @RELAY.CS.NET:mitch@research.att.com Sun Nov 22 18:47:47 1987
Posted-Date: Sun, 22 Nov 87 14:02 EST
Received-Date: Sun, 22 Nov 87 18:47:35 EST
Message-Id: <8711222347.AA02780@linc.cis.upenn.edu>
Return-Path: mitch <@RELAY.CS.NET:mitch@research.att.com>
Received: from RELAY.CS.NET by cis.upenn.edu via TCP; Sun Nov 22 18:45 EDT
Received: from relay2.cs.net by RELAY.CS.NET id aa00954; 22 Nov 87 18:41 EST
Received: from btl by csnet-relay.csnet id ak14592; 22 Nov 87 18:33 EST
Date: Sun, 22 Nov 87 14:02 EST
From: mitch%research.att.com@RELAY.CS.NET
Subject: testdef.l
To: research!mitch@linc.cis.upenn.edu
Status: RO
X-Status: 
X-Keywords:
X-UID: 1


(setsyntax '\# 2)


;;Redundant feature specifications
;;(redundef en adj) ;; part)
;;(redundef ing adj part)
(redund ngstart (det adj noun quant num ord pronoun))
(redund verb (pres past future tnsless))
(redund auxverb (modal))

;;The verb type tree
(redund comp-obj (inf-obj that-obj))
(redund inf-obj (subj-less-inf-obj 2-obj-inf-obj
		    to-be-less-inf-obj no-subj to-less-inf-obj))
(redund 2-obj-inf-obj (obj-binds-delta subj-binds-delta))


;;Cases -
;;A case's markerset is of the form : (# <semantic feats that indicate an
;; excellent "fit" for this case> # <feats that indicate an acceptable fit>)
;;This set is a mixed bag of cases.  At some point it will be necessary to
;;come up with a consistent set, but this will be easy, I think.

(df agt markerset (# hanim # anim)) 
(df dat markerset (# hanim # anim)) 
;;Location should go to Allative, Trajectory, Ablative, Locative, Gen-locative
(df loc markerset (# place # inanim)) 
(df neut markerset (all))
(df pfrom markerset (# place # event socev inanim))
(df pto markerset (# place # event socev inanim))
(df pat markerset (# time # place)) ;;added by candy
(df puntil markerset (# time # place)) ;;added by candy
(df pdur markerset (# measure #))

(df time markerset (# time event #))

;;The whole distinction between Agent and Instrument is probably wrong.
(df ins markerset (# inanim #)) 
(df com markerset (# hanim # anim)) 
(df manner)
(df exch markerset (# money # physob anim inanim hanim))
(df benef markerset (# hanim # anim inanim physob))
(df source markerset (# hanim anim inanim physob #))
(df goal markerset (# hanim anim inanim physob #))
(df ess markerset (# hanim anim inanim physob #))
(df genloc markerset (# place # inanim))
(setq refillables '(time))

(setq :nr-types '(np nbar s))
(setq :as-types '(noun pronoun verb adj num quant det ord prep poss-np))
(setq :specregs '(quant type cases-marked-by case-frame neut ess markers))
(setq :parts-of-speech '(noun verb adj det quant num ord name punc prep pronoun ))
(setq :sentence-types '(decl ynquest imper whquest inf-s))

;;VERBS

(abbrev act (event action))
(df go feats ( pres tnsless v-3s)
     cf (nil (pfrom) (pto) agt)
     markers act)

(df gone irreg (go (en) (tnsless v-3s)))

(df went irreg (go (past vspl) (tnsless v-3s pres)))

(df come feats ( pres tnsless ed v-3s)
     cf ((pfrom) (pto) agt)
     markers act)
(df came irreg (come (past vspl) (tnsless v-3s pres)))

(jlike walk go)
(jlike run go)
(df break
     feats ( pres tnsless v-3s)
     cf (neut   (ins) (agt))
     neut (# physob inanim socev #)
     markers act)

(df give
     feats ( pres tnsless v-3s)
     cf ((*obj dat) neut  (dat) agt)
     neut (# hanim  # anim inanim physob)
     markers act)

(df want feats (pres tnsless v-3s  subj-less-inf-obj ed=en)
     cf (neut  agt)
     neut (# physob inf-comp #)
     markers (state))

(df seem feats (pres tnsless v-3s that-obj to-be-less-inf-obj no-subj)
     cf (neut)
     neut ( # inf-comp that-comp # )
     markers (state))


(df believe feats ( pres tnsless v-3s  inf-obj that-obj ed=en)
     cf (neut  agt)
     neut (# hanim inf-comp that-comp #)
     markers (state))

(jlike know believe 
       feats (nil (ed=en)))

(df knew irreg (know (past vspl) (tnsless pres v-3s)))
(df known irreg (know (en) (tnsless pres v-3s)))

(df tell feats (pres tnsless v-3s obj-binds-delta comp-2-obj inf-obj that-obj)
     cf ((*obj dat) (neut) (dat) agt)
     neut (# inf-comp that-comp fact mentob # abstr-ob #)
     preps (about (neut))
     markers act) ;;changed by candy

(df told irreg (tell (past vspl en ed) (pres tnsless v-3s)))

(jlike persuade tell
       feats ((ed=en) nil)) ;;changed by candy (and partially corrected by mitch)

(jlike invite persuade
       neut (# inf-comp event socev #)
       preps (to (neut)))  ;;done by candy

(jlike say tell
       feats (nil (comp-2-obj))
       cases (nil (*obj))
       neut (# inf-comp that-comp #))

(df said irreg (say (past vspl en) (tnsless pres v-3s)))

(jlike promise persuade
       feats ((subj-binds-delta) (obj-binds-delta)))

(jlike ask persuade
       feats ((indirect-q-obj) (that-obj)))

(df shot irreg (shoot (past vspl ed en) (pres tnsless v-3s)))
(df gave irreg (give (past vspl) (tnsless pres v-3s)))
(df broke irreg (break (past vspl) (tnsless pres v-3s))) 

;;these definitions don't include case frames for now.
(df be feats (tnsless auxverb) cf (ess neut) neut (all) markers (state))
(df is irreg (be (pres v3s) (tnsless)))
(df am irreg (be (pres v1s) (tnsless)))
(df are irreg (be (pres vpl+2s) (tnsless)))
(df was irreg (be (past v13s) (tnsless)))
(df were irreg (be (past vpl+2s) (tnsless)))
(df will feats ( vspl auxverb future dummy))
;;did defined only as auxiliary here
(df do feats ( v-3s auxverb tnsless pres)
     cf (neut  (ins) agt)
     neut (# event thing #))
(df does irreg (do (v3s) (v-3s tnsless)))
(df did irreg (do (past vspl) (pres v-3s tnsless)))
(df done irreg (do (en) (pres v-3s tnsless)))
(df have feats (tnsless auxverb pres v-3s inf-obj to-less-inf-obj subj-less-inf-obj)
     cf (neut agt)
     markers (event)
     neut (all))
(df has irreg (have (v3s) (v-3s tnsless)))
(df had irreg (have (past vspl) (v-3s pres tnsless)))
(df can feats ( auxverb modal vspl pres))
(df could irreg (can (past) (pres)))


;;Quantifiers
(df which feats (*which) quant which)
(df all feats (quant npl) quant all)
;;Numbers
(abbrev twos (num ones npl n3p ))
(abbrev second (ord num ones npl n3p))
(abbrev tens (num tens ns n3p ))
(abbrev tenth (ord num tens n2 n3p))
(abbrev elevens (num 99s ns n3p ))
(abbrev eleventh (ord num 99s ns n3p))
(df one feats (num ones ns n3p ) quant 1)
(df twenty feats tens quant 20.)
(df hundred feats (num npl ) quant 100.)


;;Pronouns

(df who feats (pronoun relpron ns npl wh) markers (hanim))
(df wh\- feats (pronoun relpron ns npl wh) markers (hanim physob))
;;need to handle ambiguity here
(df what feats (ngstart det\\relpron-ambig))
;;whether that is used as a det or comp is decided
;;by a grammar rule
(df that feats (det\\comp-ambig) markers (physob))

(df i feats (pronoun ns n1p) markers (hanim physob))
(df me irreg (i nil nil))
(df my irreg (i (poss-pronoun) nil))
(df we feats (pronoun npl n1p) markers (hanim physob))
(df us irreg (we nil nil))
(df our irreg (we (poss-pronoun) nil))
(df you feats (pronoun ns npl n2p) markers (hanim))
(df your irreg (you (poss-pronoun) nil))
(df he feats (pronoun ns n3p) markers (hanim physob))
(df him irreg (he nil nil))
(df his irreg (he (poss-pronoun) nil))
(df she feats (pronoun ns n3p) markers (hanim physob))
(df her irreg (she nil nil))
(df hers irreg (she (poss-pronoun) nil))
(df it feats (pronoun ns n3p) markers (inanim physob event mentob))
(df its irreg (it (poss-pronoun) nil))
(df they feats (pronoun npl n3p) markers (hanim physob))
(df them irreg (they nil nil))
(df their irreg (they (poss-pronoun) nil))


;;Nouns
(df destruction feats (noun ns)
     markers (event)  
     cf ((neut) (ins) (agt))
     neut (# physob inanim place #))

(df mitch feats (noun ns n3p propnoun name) markers (hanim))

(df there feats (noun pseudopropnoun place ns n3p) markers (place))
(df monday feats (noun ns n3p day-of-week time) markers (time))

(df january feats (noun ns n3p time month) markers (time))
(df boy feats (noun ns n3p) markers (hanim physob))
(jlike girl boy)
(jlike man boy)
(jlike woman boy)
(df men irreg (man (npl) (ns)))
(df women irreg (woman (npl) (ns)))

(df block feats (noun ns n3p) markers (inanim physob))
(df pound feats (noun ns n3p measure) markers (measure))

(abbrev allphysobs  (# physob hanim anim inanim #))
;;Adjectives
(df next feats (ord general-ord))
(df last feats (ord general-ord))
(df red feats (adj)
     cf  (neut)
     markers (state)
     neut allphysobs)
(df big  feats (adj)
     cf  (neut)
     markers (state)
     neut allphysobs)
(jlike little big)
(df wide feats (measure adj)
     cf  (neut)
     markers (state)
     neut allphysobs)
(df small feats (adj))
(jlike stupid smart)
(jlike careful smart)
(df bright feats (adj adv degree)
     cf  (neut)
     markers (state)
     neut (# physob anim inanim #))
;;Determiners
(df the feats (det ns n3p npl def))
(df a feats (det ns n3p indef))
(df an irreg (a nil nil))
(df \'s feats (poss))
;;Prepositions
(abbrev prepcf (ess neut))
(df with feats (prep) cases-marked-by (ins com)) 
(df to feats (prep auxverb) cases-marked-by (dat time pto))
(df on feats (prep)					
     cf prepcf
     ess  (# time place physob #)
     neut (all)
     cases-marked-by (loc time))
(jlike in on)

(jlike at on) ;;To a first approximation
(df by feats (prep) cases-marked-by (agt loc time))
(df of feats (prep) cases-marked-by  nil)
(df for feats (prep) cases-marked-by (benef exch time goal pdur))
(df from feats (prep) cases-marked-by (source time pfrom))

;;Punctuation
(df ? feats (finalpunc punc qpunc))
(df \. feats (finalpunc punc)) 
(df \, feats (punc))


;;Randomness- unfilled for now.
(df and feats (none-for-now))

;;Definitions added on the fly by the "just like" feature

(jlike mary mitch) 
(jlike sue mitch) 
(jlike bill john) 
(jlike john mitch) 
(jlike book box) 
(jlike like want) 
(jlike would could) 

(jlike \'d would) 
(jlike \'ll will) 
 
(jlike expect believe) 
(jlike here home) 
(jlike exam meeting) 

From @RELAY.CS.NET:mitch@research.att.com Sun Nov 22 18:47:53 1987
Posted-Date: Sun, 22 Nov 87 14:03 EST
Received-Date: Sun, 22 Nov 87 18:47:48 EST
Message-Id: <8711222347.AA02792@linc.cis.upenn.edu>
Return-Path: mitch <@RELAY.CS.NET:mitch@research.att.com>
Received: from RELAY.CS.NET by cis.upenn.edu via TCP; Sun Nov 22 18:46 EDT
Received: from relay2.cs.net by RELAY.CS.NET id aa00958; 22 Nov 87 18:41 EST
Received: from btl by csnet-relay.csnet id al14592; 22 Nov 87 18:35 EST
Date: Sun, 22 Nov 87 14:03 EST
From: mitch%research.att.com@RELAY.CS.NET
Subject: newdef.l
To: research!mitch@linc.cis.upenn.edu
Status: RO
X-Status: 
X-Keywords:
X-UID: 2



(jlike canary dog)
(jlike bit hit)
(jlike roads block)
(jlike flooded left)
(jlike storm man)
(jlike reena mitch)
(jlike tired sad)
(jlike read see)

(jlike jump run)
(jlike lake store)
(jlike reasons men)
(jlike leave go)
(jlike fact place)
(jlike clear good)
(jlike learn see)
(jlike mountain place)
(jlike margaret mitch)
(jlike please read)
(jlike everyone he)
(jlike owns has)
(jlike where what)
(jlike why what)
(jlike |"yesterday| yesterday)
(jlike |left."| left)
(jlike both all)
(jlike ft left)
(jlike visit go)
(jlike watched saw)
(jlike wwtold told)
(jlike work be)
(jlike hungry tall)
(jlike young big)
(jlike drank read)
(jlike happpy happy)
(jlike face store)
(jlike stick book)
(jlike love store)
(jlike sing read)
(jlike fist foot)
(jlike paint hit)
(jlike finished read)
(jlike stop read)
(jlike sems seems)
(jlike hit read)
(jlike room store)
(jlike find read)
(jlike bly boy)
(jlike stay read)
(jlike ubjugated read)

(jlike took read)
(jlike watch see)
(jlike admire see)
(jlike win read)
(jlike son man)
(jlike story book)
(jlike person man)

(jlike suceed read)
(jlike stood went)
(jlike story book)
(jlike spot mitch)
(jlike foo go)
(jlike birthday party)
(jlike covers the)
(jlike reports book)
(jlike covers book)
(jlike p book)
(jlike old good)
(jlike wrote scheduled)
(jlike sasha sue)
(jlike watch see)
(jlike that1 this)
(jlike kicked hit)
(jlike bucket house)
(jlike can1 bucket)
(jlike can2 store)

(jlike painted schedule)
(jlike lion dog)
(jlike yet now)

