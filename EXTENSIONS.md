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

*Group: `*bare-time-rules*` (rules `VP-TIME-NP-TO-PP`, `TRAILING-TIME-NP-TO-PP`).*

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
(`persuade`/`promise`/`ask`/`buy`/…) inherit it because `case-frame` is a
`*specreg*`.

> **"who did you give the book yesterday ?"**
> → `[GIVE]-(agnt)→[YOU] (rcpt)→[trace: who] (obj)→[BOOK] (time)→[YESTERDAY]`

Two deliberate choices: it edits the **raw `cf`** (not the expanded `case-frame`)
and leaves expansion lazy, so `feats` are not consumed prematurely; and it is
**targeted** (`*give-class-time-verbs*` = `give tell deliver`), not a universal
`TIME` slot — a universal one would let `for`/`to`/`on` PPs ambiguously fill
`TIME` vs their existing cases on many verbs, whereas these three carry no such
preposition conflict. `defs-dictionary.dict` and the case-frame engine stay
untouched.

---

## Not extensions (faithful Marcus ports)

For the avoidance of doubt, these are **ports, not additions**, even though each
was substantial work: the NP-construction rules, declarative clauses, the
complement system (infinitival / that-clause, raising, passive, subject- and
object-control), wh-questions and long-distance wh-dependencies, relative clauses,
existentials, quantifiers, numbers and date/time NPs, and **genitives /
possessives** (`*genitive-rules*`, verbatim from Marcus's `gram3.l`). They live in
`*marcus-full-grammar*` and are covered by `(load-marcus-grammar)`.
