# OCR scan report: pidgin-grammar-rules-1977.text

Heuristic-driven scan for likely OCR artifacts in the 1977 corpus.
Each section lists the top occurrences with line number, the rule
they fall inside, the matched token, and surrounding context.

Categories are listed in roughly decreasing confidence order.
Use this against the hardcopy of Marcus's appendix to confirm
each fix before applying.

## Summary

| Category | Count | Confidence |
| --- | --: | --- |
| `capital-Is` | 49 | high (Marcus uses lowercase) |
| `capital-It` | 7 | medium (sentence-initial may be intended) |
| `digit-O-confusion` | 4 | high (zero vs O is a common OCR error) |
| `nonword-fragments` | 2 | high (already known to be garbled) |

## Rules with the most findings

Targets for hardcopy checking, in descending order. The total
count is across all categories.

| Rule | Total suspicious tokens |
| --- | --: |
| `NP-COMPLETE` | 5 |
| `NBAR-COMPLETE` | 4 |
| `SUBJ-QUEST?` | 3 |
| `WH-WITH-NP-NEXT` | 3 |
| `WH-WITH-PP-NEXT` | 3 |
| `MONDAY-THE-FIRST` | 3 |
| `HAVE-DIAG` | 2 |
| `S-CREATE` | 2 |
| `CREATE-DELTA-SUBJ` | 2 |
| `TWO-HUNDRED` | 2 |
| `BIGNUM` | 2 |
| `HOUR-COMPLETE` | 2 |
| `JUNE-FIRST-1976` | 2 |
| `WH-WITH-END-NEXT` | 2 |
| `NP-UTTERANCE` | 1 |
| `main-verb` | 1 |
| `VP-NP` | 1 |
| `WHICH-DIAGN` | 1 |
| `WHAT-DIAG` | 1 |
| `WH-WITH-NP-PP-NEXT` | 1 |

## capital-Is  (49 matches)

Likely OCR for lowercase `is`. Marcus's canonical 1987 file uses lowercase `is` throughout.

| Line | In rule | Token | Context |
| --: | --- | --- | --- |
| 20 | `NP-UTTERANCE` | `Is` | `h 2nd to c as finalpunc. / The parse Is finished.} /  / {RULE PP-UTTERANCE IN` |
| 51 | `HAVE-DIAG` | `Is` | `es-no-q then assume it is.% / If 2nd Is ns, n3p or 3rd Is not verb or 3rd` |
| 51 | `HAVE-DIAG` | `Is` | `e it is.% / If 2nd Is ns, n3p or 3rd Is not verb or 3rd is tnsless /    then` |
| 77 | `SUBJ-QUEST?` | `Is` | `5. IN PARSE-SUBJ / [=verb] [** c; * Is np-quest] [=p] [t] --> / if 1st Is n` |
| 78 | `SUBJ-QUEST?` | `Is` | `* Is np-quest] [=p] [t] --> / if 1st Is not auxverb or 3rd Is not verb /   t` |
| 78 | `SUBJ-QUEST?` | `Is` | `] --> / if 1st Is not auxverb or 3rd Is not verb /   then create a new np no` |
| 157 | `main-verb` | `Is` | `to-be-less-inf-comp andthen / If it Is 2-obj-inf-obj then activate 2-obj-` |
| 235 | `VP-NP` | `Is` | `the np of the current s. / If there Is an s of lower and  the np of it is` |
| 272 | `WHICH-DIAGN` | `Is` | `E WHICH-DIAGN IN CPOOL / [=*which; * Is not any of quant, relpron] --> / If` |
| 281 | `WHAT-DIAG` | `Is` | `paths?)% / If 2nd is ngstart and 2nd Is not det /    then label 1st det, ns,` |
| 299 | `S-CREATE` | `Is` | `f the complex NP constraint, which Is adhoc and ugly. / That this rule sho` |
| 328 | `WH-WITH-NP-NEXT` | `Is` | `or not to use the WH-comp If there Is an NP next. Note that If the NP Is` |
| 328 | `WH-WITH-NP-NEXT` | `Is` | `Is an NP next. Note that If the NP Is followed by a PP, then a different` |
| 328 | `WH-WITH-NP-NEXT` | `Is` | `le next indicates that the WH-comp Is not to be used, while running crea` |
| 347 | `WH-WITH-NP-PP-NEXT` | `Is` | `st possible number of objects of c Is greater than 1 / then run wh-with-np` |
| 357 | `WH-WITH-PP-NEXT` | `Is` | `st possible number of objects of c Is greater than O / XI.e. If the WH-com` |
| 434 | `that-diag-1` | `Is` | `LE that-diag-1 in cpool / [=*that; * Is none of comp, det, pronoun] [=p] -` |
| 475 | `INF-S-START1` | `Is` | `only be dropped If the complement Is expected.% / [=np][=*to,auxverb][=tn` |
| 493 | `INSERT-TO-BE` | `Is` | `BE IN TO-BE-LESS-INF-COMP / [=np] [* Is any of en, adj] --> / %won't work fo` |
| 509 | `CREATE-DELTA-SUBJ` | `Is` | `that the subject of the complement Is a delta (I.e. a "base-generated" t` |
| 509 | `CREATE-DELTA-SUBJ` | `Is` | `. a "base-generated" trace), which Is not bound by syntactic rules, but` |
| 567 | `A-HUNDRED-DIAG` | `Is` | `f *hundred, bignum] [t] --> / If 3rd Is noun, ns, measure then run determi` |
| 608 | `DET-QUANT` | `Is` | `. / Attach 1st to c as quant. / If 1st Is num then label c numap. / Transfer n` |
| 666 | `INCOMPLETE-NP` | `Is` | `tivate npool, parse-noun. / If there Is a qp of c then label c quant-np; a` |
| 685 | `NBAR-COMPLETE` | `Is` | `RULE NBAR-COMPLETE IN CPOOL / %This Is an experimental type of rule not d` |

_… and 24 more_

## capital-It  (7 matches)

Likely OCR for lowercase `it` mid-sentence. (Sentence-initial `It` may be intentional; spot-check.)

| Line | In rule | Token | Context |
| --: | --- | --- | --- |
| 172 | `VP-VERB` | `It` | `f the np of the s above upper then It fills the subj slot of upper.} /  / {R` |
| 290 | `<top-level>` | `It` | `a / ;register to cache the WH-comp. It can be considered an Implementatio` |
| 299 | `S-CREATE` | `It` | `-CREATE S / %This is ugly and adhoc. It simply embodies a form of the comp` |
| 323 | `WH-WITH-END-NEXT` | `It` | `r of objects of c is equal to O / Or / It isn't true that the WH-comp fits a` |
| 686 | `NBAR-COMPLETE` | `It` | `ent Is still in progress, although It now looks like there are better wa` |
| 713 | `NP-COMPLETE` | `It` | `Is a det of c then the features of It else ns,npl) and (If there is a qp` |
| 964 | `NUMBER-TO-TIME` | `It` | `en run month-complete next else / If It is numap and there is not a det of` |

## digit-O-confusion  (4 matches)

Capital `O` adjacent to digits or in a numeric context. Likely OCR for `0` (zero).

| Line | In rule | Token | Context |
| --: | --- | --- | --- |
| 321 | `WH-WITH-END-NEXT` | `O` | `number of objects of c is equal to O / Or / It isn't true that the WH-comp` |
| 357 | `WH-WITH-PP-NEXT` | `O` | `er of objects of c Is greater than O / XI.e. If the WH-comp can serve as` |
| 362 | `WH-WITH-PP-NEXT` | `O` | `er of objects of c is greater than O / %i.e. this verb MUST take an objec` |
| 849 | `HOUR-COMPLETE` | `O` | `f c / then quant register of it else O). / If there Is an oclock register o` |

## nonword-fragments  (2 matches)

Specific garbled phrases identified during earlier review.

| Line | In rule | Token | Context |
| --: | --- | --- | --- |
| 938 | `MONDAY-THE-FIRST` | `nonore to deto` | `month register of the noun of c to / nonore to deto. / the month register of the noun of` |
| 952 | `JUNE-FIRST-1976` | `are yet ef` | `year register of the noun of c to / are yet ef ther or he noun of 2nd.} /  / {RULE TI` |
