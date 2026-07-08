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

### 2. Manner adverbs — the `ADVERB-ADJUNCT` rule (+ lexicon)

*Group: `*adverb-rules*` (rule `ADVERB-ADJUNCT`), plus `quickly`/`slowly` in
`system/core/runtime/supplement.dict`.*

gram1–gram5 have **no adverb-attachment rule at all**, and Marcus's dictionary has
no manner adverbs (his own `df1` adverbs like `ago`/`later` are commented out —
`df1` is his way of disabling a `df`). So a manner adverb was doubly unhandled:
dropped as an unknown word, and — once lexicalized — with nowhere to attach, it
stalled the parse. This extension supplies both halves: the lexicon entries
(`(df quickly feats (adv manner))`) and a rule to attach the adverb.

> **"the man sees the dog quickly ."**
> → `[SEE]-(agnt)→[MAN: #] (obj)→[DOG: #] (manr)→[QUICKLY]`

Mechanism: `ADVERB-ADJUNCT` fires `IN SS-FINAL` on `[=adv]`, attaching the adverb
as an `adv` daughter of the clause — modelled on gram5's `PP-UNDER-S-1`, which
attaches an unlicensed PP to the S the same way. After the main verb a major
clause has both `SS-VP` (with `VP-DONE` at priority 20) and `SS-FINAL` active; the
rule's default priority (10) fires before `VP-DONE` drops the VP. The extractor
then reads the adverb as `[PRED]-(manr)→[MANNER]`, the adverbial analog of an
adjective's `[NOUN]-(attr)→[ADJ]`.

---

## Runtime / lexicon departures

These are not grammar rules but small changes to the ported runtime that give the
parser behavior Marcus's own sources did not (usually because his lexicon
morphology tagged a word in a way our lexicon port does not reproduce). Each is
also commented at its source site.

### 3. `name` added to `*as-types*` (`system/core/runtime/defs.lisp`)

Marcus's `defs.l` attention-shift list omits `name`, so in our port a bare
proper-name word would never trigger the `PROPNAME` attention shift on its own.
Adding `name` to `*as-types*` lets a proper name start its own name NP.

> **"john sees the dog ."**
> → `[SEE]-(agnt)→[PERSON: John] (obj)→[DOG: #]`

### 4. `det\relpron-ambig` added to `*as-types*` (`system/core/runtime/defs.lisp`)

Also added to `*as-types*` so the wh-determiner `what` (whose only other feature
is `ngstart`) fires `STARTNP` and reaches `WHAT-DIAG` to be diagnosed as a
wh-pronoun vs a wh-determiner.

> **"what did the boy break ?"**
> → a wh-question with `what` as the queried object.

---

## Not extensions (faithful Marcus ports)

For the avoidance of doubt, these are **ports, not additions**, even though each
was substantial work: the NP-construction rules, declarative clauses, the
complement system (infinitival / that-clause, raising, passive, subject- and
object-control), wh-questions and long-distance wh-dependencies, relative clauses,
existentials, quantifiers, numbers and date/time NPs, and **genitives /
possessives** (`*genitive-rules*`, verbatim from Marcus's `gram3.l`). They live in
`*marcus-full-grammar*` and are covered by `(load-marcus-grammar)`.
