# Extensions to classic PARSIFAL

This project is, first and foremost, a faithful port of Mitchell Marcus's
PARSIFAL: the runtime (`parse.l`/`case.l`/`com.l`), the grammar-language compiler
(`glang.l`), and the grammar itself (`gram1.l`–`gram5.l`) are reproduced as
closely as the move to Common Lisp allows. The overwhelming majority of what the
parser does is Marcus's, verbatim modulo whitespace.

**This file is the exhaustive list of the few places we deliberately go BEYOND
classic PARSIFAL** — rules or runtime behavior that are *not* in Marcus's
originals. It is a living document: every future departure gets an entry here,
with a one-sentence illustration, so the boundary between "Marcus's parser" and
"our additions" stays explicit and auditable.

If it is not listed here, it is a port, not an invention.

## How extensions are kept separate

The grammar is composed from named rule groups (see
`system/grammar/clause-grammar.lisp`):

- **`*marcus-full-grammar*`** — every rule group that is a verbatim port of
  Marcus's grammar. Register it alone with **`(load-marcus-grammar)`** to run
  PARSIFAL exactly as Marcus defined it, with none of our additions.
- **`*grammar-extensions*`** — our own rule groups, the ones that are *not* in
  Marcus. Empty until the first genuinely new rule.
- **`*full-grammar*`** = `(append *marcus-full-grammar* *grammar-extensions*)`,
  registered by **`(load-full-grammar)`** — the default.

So the provenance of any grammar rule is mechanical: a group in
`*grammar-extensions*` is ours; a group in `*marcus-full-grammar*` is his.
Runtime-level departures (feature lists in `defs.lisp`, lexicon supplements) are
harder to fence off structurally, so they are called out in-code and listed
below.

---

## Grammar extensions (`*grammar-extensions*`)

### 1. Apposition — "the dog, Spot" → `[DOG: Spot]`

*Group: `*apposition-rules*` (rule `APPOSITIVE`).*

A common-noun NP directly followed by `, NAME` takes the name as an appositive
while keeping its **own, specific type**. Without this, a named individual can
only get the broad type from the name's semantic marker (`hanim`/`anim` →
`[ANIMAL: Spot]`, never `[DOG: Spot]`); apposition lets the head noun supply the
narrower type.

> **"the dog , spot sees the woman ."**
> → `[SEE]-(agnt)→[DOG: Spot] (obj)→[WOMAN: #]`

Mechanism: `APPOSITIVE` fires `IN NP-COMPLETE` on `[=apunc][=name]` (comma then
name). Because the comma occupies the first buffer cell, `PROPNAME` — the
attention shift that needs a name in the first cell — never fires; the rule
consumes the comma **and** the name NP together (PROPNAME builds that name NP
during look-ahead; its `* is not np` guard stops it re-triggering), attaches the
name under the head NP's `appos` daughter, and runs `np-done` next. Its default
priority (10) beats `NP-DONE` (15), so it intercepts before the NP is finalised
as a plain subject/object. This is the **first genuinely new grammar rule** in
the project; gram1–gram5 and the 1977 grammar appendix have no apposition rule.

### 2. Manner adverbs — the `ADVERB-ADJUNCT` and `PREVERBAL-ADVERB` rules (+ lexicon)

*Group: `*adverb-rules*` (rules `ADVERB-ADJUNCT`, `PREVERBAL-ADVERB`), plus
`quickly`/`slowly`/`gladly` in `system/core/runtime/supplement.dict`.*

gram1–gram5 have **no adverb-attachment rule at all**, and Marcus's dictionary has
no manner adverbs (his own `df1` adverbs like `ago`/`later` are commented out —
`df1` is his way of disabling a `df`). So a manner adverb was doubly unhandled:
dropped as an unknown word, and — once lexicalized — with nowhere to attach, it
stalled the parse. This extension supplies both halves: the lexicon entries
(`(df quickly feats (adv manner))`) and two rules to attach the adverb, one per
position.

**Clause-final — `ADVERB-ADJUNCT`.**

> **"the man sees the dog quickly ."**
> → `[SEE]-(agnt)→[MAN: #] (obj)→[DOG: #] (manr)→[QUICKLY]`

`ADVERB-ADJUNCT` fires `IN SS-FINAL` on `[=adv]`, attaching the adverb as an `adv`
daughter of the clause — modelled on gram5's `PP-UNDER-S-1`, which attaches an
unlicensed PP to the S the same way. After the main verb a major clause has both
`SS-VP` (with `VP-DONE` at priority 20) and `SS-FINAL` active; the rule's default
priority (10) fires before `VP-DONE` drops the VP. The extractor then reads the
adverb as `[PRED]-(manr)→[MANNER]`, the adverbial analog of an adjective's
`[NOUN]-(attr)→[ADJ]`.

**Pre-verbal (between a modal and the verb) — `PREVERBAL-ADVERB`.**

> **"i will gladly pay you ."**
> → the S gains an `adv` daughter `gladly` (→ `[PAY]-(manr)→[GLADLY]`)

An adverb wedged between a modal/`will` and the main verb never reaches
`SS-FINAL`: the trouble is upstream, in `BUILD-AUX`. Marcus's `MODAL`/`FUTURE`
rules need `[modal/*will][=tnsless]` adjacency to attach the modal, so an
intervening adverb blocks them — the empty aux completes and `MAIN-VERB` then
wrongly grabs `will` itself as the verb, dead-ending the parse. `PREVERBAL-ADVERB`
fires `IN BUILD-AUX` (priority 5, modelled on `THERE`) on
`[modal/*will][=adv][=tnsless]`, splicing the adverb onto the current S with
`Attach 2nd to the current s as adv` — restoring `[modal][verb]` adjacency so
`MODAL`/`FUTURE` fire normally. The `[=tnsless]` guard keeps it to the genuine
pre-verbal position. (The subject-adjacent, aux-less case — "I gladly pay you." —
is a separate clause-start-diagnosis gap, not handled.)

### 3. Bare temporal adjuncts fill TIME — `*bare-time-rules*`

*Group: `*bare-time-rules*` (rules `VP-TIME-NP-TO-PP`, `TRAILING-TIME-NP-TO-PP`,
`EMB-WH-VP-TIME-NP-TO-PP`, `EMB-VP-TIME-NP-TO-PP`).*

A bare time NP (`Tuesday`, `today`, `yesterday`) can't fill a verb's `TIME` case
positionally — `TIME` is not an object case, and case-fill only happens through a
PP that attaches under the VP (`VP-PP` + `ppcasegen`). So a bare temporal used to
ride along as a dropped `BAD` object (and, worse, corrupt the frame so a
following `for`-PP no longer fit its case). This extension turns the bare time NP
into a `during`-PP — reusing Marcus's own mechanism from `TIME-NP-TO-PP` (insert
`during` before the NP; the CPOOL PP rule then builds `[during][NP] → pp`) — so
the PP machinery fills `TIME` via `during` (which is `cases-marked-by time`).

> **"the boy gave the girl a book yesterday ."**
> → `[GIVE]-(agnt)→[BOY] (rcpt)→[GIRL] (obj)→[BOOK] (time)→[YESTERDAY]`

The **one difference from Marcus's `TIME-NP-TO-PP`** — which is deliberately *not*
in `*full-grammar*` because he flagged it uncontrolled — is the **packet**. His
fires `IN CPOOL` on *any* time NP, including a PP's own object (inserting `during`
inside "for friday" and degrading "schedule a meeting for friday ."). Ours is
gated:

- **`VP-TIME-NP-TO-PP`** (`PRIORITY: 5 IN SS-VP`) — the primary rule. Fires while
  the VP is open, ahead of `OBJECTS`, so the `during`-PP attaches *under the VP*
  and `VP-PP` fills `TIME`. A PP's object never surfaces here as the first cell
  (its preposition occupies it), so only a bare *adjunct* is caught.
- **`TRAILING-TIME-NP-TO-PP`** (`IN SS-FINAL`) — the fallback for a time NP that
  reaches clause-final position after some non-time PP already closed the VP;
  attaches at the S level (parses, but with the VP closed `TIME` can't fill).
- **`EMB-WH-VP-TIME-NP-TO-PP`** (`PRIORITY: 5 IN WH-VP`) and
  **`EMB-VP-TIME-NP-TO-PP`** (`PRIORITY: 5 IN EMBEDDED-S-VP`) — the embedded-clause
  siblings, for a bare temporal *inside* a relative or complement clause (which
  runs in `wh-vp`/`embedded-s-vp`, not `ss-vp`). Without them a temporal inside a
  reduced relative — "a hamburger you give me **today**" — is never rewritten, so
  `WH-WITH-NP-NEXT` grabs it as a spurious second object of the relative verb, the
  relative closes without binding its gap, and the stray gap-trace + time leak up
  into the matrix verb's open `NEUT`/`TIME` slots. At priority 5 they fire ahead
  of `WH-WITH-NP-NEXT` (10) / `WH-WITH-NP-PP-NEXT` (7) / `OBJ-IN-EMBEDDED-S`, so
  the temporal becomes a `during`-PP that fills the *relative* verb's `TIME` and
  the gap binds correctly. This also fixed a deferred item — **"What did you give
  Sue yesterday?"** (a bare temporal in a wh-question that used to overflow
  `give`'s objects → `TOO-MANY-NPS` → NIL) now parses, `yesterday` filling `TIME`.

Together with the give-class TIME case (#6) this makes the flagship **"I will
gladly pay you Tuesday for a hamburger today."** fill its whole frame:
`AGT/DAT/EXCH` plus `TIME` for both `Tuesday` and `today`.

---

## Runtime / lexicon departures

These are not grammar rules but small changes to the ported runtime that give the
parser behavior Marcus's own sources did not (usually because his lexicon
morphology tagged a word in a way our lexicon port does not reproduce). Each is
also commented at its source site.

### 4. `name` added to `*as-types*` (`system/core/runtime/defs.lisp`)

Marcus's `defs.l` attention-shift list omits `name`, so in our port a bare
proper-name word would never trigger the `PROPNAME` attention shift on its own.
Adding `name` to `*as-types*` lets a proper name start its own name NP.

> **"john sees the dog ."**
> → `[SEE]-(agnt)→[PERSON: John] (obj)→[DOG: #]`

### 5. `det\relpron-ambig` added to `*as-types*` (`system/core/runtime/defs.lisp`)

Also added to `*as-types*` so the wh-determiner `what` (whose only other feature
is `ngstart`) fires `STARTNP` and reaches `WHAT-DIAG` to be diagnosed as a
wh-pronoun vs a wh-determiner.

> **"what did the boy break ?"**
> → a wh-question with `what` as the queried object.

### 6. give-class verbs given a TIME case (`augment-give-class-time`, `system/core/runtime/dictionary.lisp`)

Give-class transfer verbs take temporal adjuncts in English ("give Sue the book
tomorrow"), but Marcus entered `give`/`tell`/`deliver` with **no `TIME` case**, so
even with the bare-temporal rules (#3) their `during`-PP had no slot to fill and
`TIME` dropped. `augment-give-class-time` runs after the dictionary loads and
splices an optional, refillable `(time)` case into each base verb's **raw `cf`**,
just before the clause-final subject case (matching every native-`TIME` frame
Marcus wrote, e.g. `schedule`: `(neut (time) (loc) agt)`); `jlike` descendants
(`persuade`/`promise`/`ask`/…) inherit it because `case-frame` is a `*specreg*`.
(`buy` was such a descendant until #7 gave it a dedicated commercial frame — which
also carries a `(time)` slot, so it keeps its temporal.)

> **"who did you give the book yesterday ?"**
> → `[GIVE]-(agnt)→[YOU] (rcpt)→[trace: who] (obj)→[BOOK] (time)→[YESTERDAY]`

Two deliberate choices: it edits the **raw `cf`** (not the expanded `case-frame`)
and leaves expansion lazy, so `feats` are not consumed prematurely; and it is
**targeted** (`*give-class-time-verbs*` = `give tell deliver`), not a universal
`TIME` slot — a universal one would let `for`/`to`/`on` PPs ambiguously fill
`TIME` vs their existing cases on many verbs, whereas these three carry no such
preposition conflict. `defs-dictionary.dict` and the case-frame engine stay
untouched.

### 7. Commercial-transaction verbs `buy`/`sell` (+ `dollar`) — `system/core/runtime/supplement.dict`

`pay`, `buy` and `sell` are three Fillmore **perspectives** on one commercial
exchange, each foregrounding different participants — so each needs the case slots
its perspective exposes. Marcus entered only `buy`, and only as `(jlike buy give)`
— a plain ditransitive (`agt`/`dat`/`neut`) with **no seller or price slot**, so
"buy a hamburger **from** you **for** two dollars" dropped both PPs; `sell` he
never entered at all. This extension gives them dedicated commercial frames:

- **`buy`** — `cf ((neut) (source) (exch) (time) agt)`: `agt`=buyer, `neut`=goods,
  `source`=seller (`from`), `exch`=money (`for`). **This is the one place
  `supplement.dict` supersedes a Marcus entry** (his incidental `jlike buy give`).
- **`sell`** — `cf ((*obj dat) neut (dat) (exch) (time) agt)`: `agt`=seller,
  `dat`=buyer, `neut`=goods, `exch`=money (`for`).
- **`dollar`** — a `money`-marked count noun, so a bare amount ("...for two
  dollars") reads as the price.

> **"I buy a hamburger from you for two dollars."** and
> **"You sell me a hamburger for two dollars."**
> → the extractor's exchange frame promotes **both** (and `pay`) to the *same*
> `[EXCHANGE]-(has-part)→[PAY]…[MONEY]/[DOLLAR] (has-part)→[GIVE]…[HAMBURGER]`.

The CG-side perspective mapping that unifies the three verbs lives in the
`cg-from-parse` back-end (`*commercial-perspectives*`), not the parser.

### 8. Existential vs. reduced-relative fork — the `REDUCED-RELATIVE` override (`*existential-relative-rules*`)

*Group: `*existential-relative-rules*` — the project's **first override** of a
Marcus rule, rather than a brand-new one.*

Marcus's `REDUCED-RELATIVE` (gram2, in `*relative-clause-rules*`) fires on a
completed NP followed by a verb (`[=np][=verb]`), inserting `wh-` to open a
reduced relative. On the canonical existential

> **"Is there a meeting scheduled for friday ?"**

it fires on `[a meeting][scheduled]` and commits to a *participial* reduced-relative
reading his grammar cannot finish (`INSERT-TO-BE` lives only in the raising
packets, never a relative packet), dead-ending at `TOO-MANY-NPS`. But the
existential-passive reading Marcus *does* support — `THERE` relabels the clause
`existential`, and `scheduled for friday` parses as a passive predicate via
`*raising-rules*` — needs that fork to go the other way. (In isolation, the curated
existential grammar *without* `*relative-clause-rules*` parses it: see
`gram1-existential-passive-test`.)

The override is Marcus's rule verbatim plus one guard:

> `[** c; the noun of the nbar of the binding of the np of the current s is not *there]`

— don't take the reduced-relative when the enclosing clause is an existential
`there` clause (a signal already computed, and used, by the `THERE` rule; the
`there` subject is attached by `AUX-INVERSION` long before this fork). With it,
`REDUCED-RELATIVE` defers, the NP finalizes, `THERE` fires, and the passive
predicate attaches.

**Why an override, not an edit.** `*relative-clause-rules*` stays byte-for-byte in
`*marcus-full-grammar*`; the guarded copy lives here in `*grammar-extensions*`.
Because `*full-grammar*` registers extensions after Marcus, re-registering the same
rule name **replaces** the original there (verified), so `load-full-grammar` gets
the guard while `load-marcus-grammar` runs the pristine rule — under which the
existential correctly still fails, faithful to Marcus. Normal reduced relatives
("the boy you met") are untouched (their clause subject is not `*there`). This only
steers an existing fork; it invents no construction — the *participial* relative
("the meeting scheduled for friday meets") remains a genuine Marcus gap and stays
deferred. Guarded by `gram1-existential-full-test` and the coverage test.

---

## Tooling / introspection

Unlike everything above, these change **no parse and no parser behavior** — they
are read-only utilities for inspecting the ported system. They earn an entry only
because they are non-Marcus code, and this catalogue is meant to be exhaustive.

### 10. Noun-noun compounds — the `NOUN-COMPOUND` rule (`*noun-compound-rules*`)

*Group: `*noun-compound-rules*` (rule `NOUN-COMPOUND`), plus the extractor's
`kind` relation in `cgraph-types`.*

`gram1`–`gram5` and the 1977 appendix have **no noun-noun compound rule**.
Marcus's `NOUN` (gram3:131) takes a single `[=noun]` into a fresh nbar, and
`ADJ` (gram3:114) handles attributive **adjectives** only. Nominal compounds
were never in scope, so "a cherry pie" parsed as *two* NPs:

```
np: NP2 (NS N3P INDEF DET NP)     ← "a cherry"
  det: a
  nbar: NBAR2 → noun: cherry
np: NP3 (BAD N3P NP)              ← "pie", determiner-less, so NP-DONE says bad
  nbar: NBAR3 → noun: pie
```

The extractor took the first as the object and dropped the BAD node, so
`(obj)→[CHERRY]` — the head noun vanished. Every `jlike block` food noun hit
this: "hamburger pie", "fruit pie", all the same.

`NOUN-COMPOUND` fires `IN PARSE-ADJ` — the packet that already runs immediately
before `PARSE-NOUN` — on two adjacent nouns, attaching the first as an `nmod`
daughter and letting the second continue as the head. The compound is
head-final: a cherry pie is a PIE.

**Priority 5**, ahead of `ADJ`'s `[t]` catch-all at the default 10, which would
otherwise deactivate `PARSE-ADJ` and hand the first noun to `NOUN` as a head in
its own right.

**Both cells exclude `time` and `place`.** Marcus enters the deictics and
calendar words as ordinary nouns — `there` is `(noun pseudopropnoun place ns
n3p)`, `monday` is `(noun ns n3p day-of-week time)` — so an adjunct puts two
nouns side by side: "…gave the girl a book **yesterday** ." is `[book][yesterday]`,
"…sees the dog **here** ." is `[dog][here]`. Unguarded, the rule ate both as
compounds, losing the DATIVE case in one and LOC in the other.
`gram1-give-time-test`, `gram1-pay-time-exch-test` and cg-from-parse's
`extract-deictic-headless` all caught it. (Guarding on `pseudopropnoun` — the
category Marcus's NBAR rule uses for "not-modifiable" — was tried first and is
**not** sufficient: `yesterday` is `jlike wednesday → monday`, and `monday`
carries `time` but no `pseudopropnoun`.)

**Modifiers stack flat.** Three nouns ("cherry pie crust") fire the rule twice,
giving two `nmod` daughters on one head rather than a nested `[cherry [pie
crust]]`. Compound bracketing is genuinely ambiguous and Marcus's grammar offers
no guidance, so flat is the honest under-commitment.

**The relation is `kind`, not `attr`.** `attr`'s dest-type is `ATTRIBUTE`, and a
modifying noun like `CHERRY` lives under `PHYSICAL → SUBSTANCE → FOOD → FRUIT`,
so `[PIE]→(attr)→[CHERRY]` is type-invalid and is silently dropped. `kind` is
new in `cgraph-types` (`entity → entity`) and says what a speaker means by
"cherry pie": a *kind* of pie, as against apple or mince. Adjective modifiers
still emit `attr`; the extractor picks by what the parser saw — an `nmod`
daughter rather than an `adj` one.

```
"a red pie"     →  (attr)→[RED]
"a cherry pie"  →  (kind)→[CHERRY]
```

**Known cost.** More word-salad now parses. With an unknown word dropped, "the
girl zorks pie ." leaves `[girl][pie]`, which is now a well-formed compound NP
and so a successful np-utterance parse where it used to fail — see the note in
cg-from-parse's `extract-diagnose-headless.lisp`. This is inherent to admitting
compounds at all, and is the usual argument for keeping the rule narrow.

### 9. Lexicon inventory — `lexicon-words` / `lexicon-noise-p` (`system/core/runtime/lexicon.lisp`)

Marcus's `com.l` can *resolve* a word (`morpho`/`expandsim`) but never *enumerate*
the lexicon: defined words are `:parsifal` symbols with their definitions hung on
the CL property list, with no registry of what exists. `lexicon-words` recovers the
inventory by walking the `:parsifal` package for the symbols that carry a dictionary
definition — a `feats`/`features` entry (a `df`), an `irreg` form, or a `jlike`
similar-word link, exactly the properties `expandsim` resolves on — so membership
matches what `morpho` can actually look up.

`lexicon-noise-p` is the companion classifier: the lexicon also holds punctuation
tokens (`.`, `?`, `,`) and clitic fragments (`'d`, `'ll`, `'s`) that are real entries
but not words one would add to a sentence, and it flags them so `lexicon-words` can
omit them by default (`:all t` returns the complete inventory). It spots punctuation
by its `punc`-family feature and a clitic by its leading apostrophe, resolving
through a `jlike` model where needed (`:` is `jlike` `,`) and treating a lone
abbreviation symbol as none (number words store `feats` as the atom `tens`, expanded
lazily by `expandm`) — all **without calling `expandsim`**, so merely listing the
lexicon never expands its entries as a side effect.

> `(pa:lexicon-words :sort t)` → 422 content words (`ABOUT` … `YOUR`);
> `:all t` adds the 9 punctuation/clitic entries (`. , : ? 'd 'll 'm 's 'til`).

The `cg-from-parse` driver's `cfp:browse-lexicon` is a REPL lister built on this —
the lexicon-side counterpart to its `browse-types` concept-type browser.

---

## Not extensions (faithful Marcus ports)

For the avoidance of doubt, these are **ports, not additions**, even though each
was substantial work: the NP-construction rules, declarative clauses, the
complement system (infinitival / that-clause, raising, passive, subject- and
object-control), wh-questions and long-distance wh-dependencies, relative clauses,
existentials, quantifiers, numbers and date/time NPs, and **genitives /
possessives** (`*genitive-rules*`, verbatim from Marcus's `gram3.l`). They live in
`*marcus-full-grammar*` and are covered by `(load-marcus-grammar)`.
