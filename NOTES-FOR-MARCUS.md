# Notes for Mitch

A short orientation for the one reader who knows PARSIFAL better than anyone:
what this port is faithful to, what's been adapted, and what's deliberately left
undone. The short version — the parser runs again, on its own grammar, and it
inherits even your own "this isn't quite finished" annotations.

## What it is

A Common Lisp port of PARSIFAL, built from the MacLISP / Franz Lisp sources you
sent in June 2026. Three pieces run today:

- **`glang.l`** — your grammar-language compiler. It reads the
  `{RULE NAME IN packets [...] --> actions}` notation and emits the compiled Lisp
  your `glang.l` would have produced (a Pratt/CGOL port; see
  `system/reference/glang-cl/`).
- **`parse.l` + `case.l` + `com.l`** — the wait-and-see runtime: the buffer,
  attention-shifting, the packet machinery, and the case mechanism with your
  markersets, `smqval`, and delta/trace binding.
- **A broad slice of the 1987 grammar.** English sentences parse end to end into
  your case-frame structures.

**47 example sentences — many of them yours — parse with their predicate-argument
structure checked, all green.** They run from "Who broke the jar?" up to "I gave
the boy who you wanted to give the books to three books," where a single NP, *the
boy*, is recovered as the main-clause dative, a relative pronoun, and (through a
stranded preposition deep in a want-complement) the embedded verb's recipient.

## What is faithful (verbatim)

- The grammar **rules are yours**, transcribed verbatim (modulo whitespace). The
  glang-cl compiler reads all of `gram1.l`–`gram5.l`, and a second (cl-yacc)
  rule-frame parser cross-checks it: on `gram4.l` the two agree rule-for-rule
  (kind, name, priority, packets), and 144 of 146 chunks agree on the 1977
  grammar appendix.
- The **framework is intact** — nothing was redesigned. Packets activate and
  deactivate as you wrote them; the case frames fill via your `VP-NP` /
  delta-binding crules; the wh-comp register and complex-NP constraint are yours.

## One thing worth knowing about organisation

The rules live as named, **composable groups** in
`test/integration/clause-grammar.lisp`, not as `gram*.l` loaded as files.
`load-full-grammar` registers them as a single grammar that parses every family
the tests build. So it is *your rules, assembled* — not `(load "gram3.l")`.

## What has been adapted (and why)

The runtime is a hand port; the adaptations are small and almost all correct a
porting issue or restore data, rather than change your design:

- the `redund` table reads implication→feature (so a `det` picks up `ngstart`);
- the case markersets (`defs.l` 31–53) were re-included, so `smqval` gates roles;
- a compiler fix so `:`-prefixed register names (e.g. `:wh-comp`) become CL
  keywords, matching the runtime that reads them;
- `name` and `det\relpron-ambig` added to `:as-types` (your morphology presumably
  tagged these via an existing as-type that our lexicon port doesn't reproduce);
- the `prefer` degrees (`much`/`somewhat`/`no`) and a few primitives that weren't
  in the received fragments.

A couple of tests hand-build one lexical item to isolate a construction — e.g.
`yesterday` as a `pseudopropnoun`, which simply restores your `df1 yesterday …
pseudopropnoun` that a later, self-described "MM 4/78 hack" `jlike yesterday
wednesday` masks (the `df1` being define-if-new, it never takes effect).

## What is deferred (and why) — including your own caveats

These are documented with their precise blockers; several trace back to notes
*you* left:

- **Bare temporal adjuncts on time-slotless verbs** ("give Sue yesterday").
  Works for verbs with a TIME case (`schedule`), via your `TIME-NP-TO-PP`. But
  that rule carries your own comment — *"this rule needs to be controlled, but
  right zeroeth approx"* — and on `give` the `during`-PP lands at the S level and
  the TIME case doesn't fill.
- **Composite dates** ("monday, june 1st"). `TIME-FINISH` can't reactivate a
  finished time NP: `NP-COMPLETE` spends the once-only NR check while the NP is
  still being built, before it is a complete time NP.
- **Pied-piping** ("To whom did Bob give it?"). Deliberately absent — it isn't in
  your grammar; you question PPs by *stranding* ("Who did Bob give the book
  to?"), which we do.

## One operational caveat

One parse per process is safest. Parsing mutates a little shared lexical state
(an expanded word's features), so a long sequence of parses in one image can
occasionally interfere; the test suite does one parse per process.

## Trying it

`README.md` has the quickstart; `GLOSSARY.md` walks the terminology against one
worked parse; and `test/integration/*.lisp` is one commented sentence each —
each parses a sentence and checks its case roles, so they double as a tour of the
grammar.

---

*With thanks for the source, the permission, and the answers along the way.*
