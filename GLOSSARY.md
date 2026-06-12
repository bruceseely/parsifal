# Parsifal glossary

A plain-English guide to the terminology you meet when reading Parsifal's output
or its grammar. It covers two overlapping vocabularies:

1. **Linguistics** — the terms for *reading what a parse means* (predicate,
   agent, trace, control…). This is the half you need to *use* the parser.
2. **PARSIFAL machinery** — the terms for *how the parser works* (packet,
   buffer, wait-and-see…). You only need these to read or write grammar rules.

For where each term lives in the data and how to get at it, see the **API
cheatsheet** at the bottom. For the deeper story, see Marcus's book (*A Theory
of Syntactic Recognition for Natural Language*, MIT Press 1980), the per-feature
index in `notes/pidgin-grammar-features.text`, and the heavily-commented
`test/integration/*.lisp` files (each parses one sentence and explains it).

---

## A worked example

Everything below is grounded in one sentence. Parsing

```lisp
(parse-sentence "the boy persuaded the girl to go ."
                :initial-rule (intern "INITIAL-RULE" :parsifal))   ; => T
```

produces two **case frames** — one per verb:

```
MATRIX  (the boy persuaded the girl ...)        EMBEDDED  (... to go)
  predicate PERSUADE                              predicate GO
    AGT  via SUBJ  -> BOY                           AGT via SUBJ -> trace -> GIRL
    DAT  via OBJ   -> GIRL
    NEUT via OBJ   -> [the embedded clause: to go]
```

Read in plain English: **the boy persuaded the girl to go, and it is the girl
who does the going.** Every term below is a piece of how the parser said that.

---

## Reading the output (linguistics)

**predicate** — the verb (relation) at the heart of a clause. Each clause has
one; here `PERSUADE` for the main clause and `GO` for the embedded one. A frame's
predicate is the lexical root, so "persuaded" → `PERSUADE`, "went" → `GO`.

**argument** — a phrase the predicate requires or licenses: its subject, objects,
and complements. "the boy", "the girl", and "to go" are persuade's arguments.

**case / thematic role (a.k.a. theta-role)** — the *meaning* role an argument
plays, independent of where it sits in the sentence. Parsifal's main cases:

| case | role | gloss |
| --- | --- | --- |
| `AGT`  | agent      | the doer |
| `DAT`  | dative     | the recipient / affected one ("give *Sue*", persuade *the girl*) |
| `NEUT` | neutral    | the theme — "the thing" (the book, the meeting, an embedded clause) |
| `TIME` | time       | when (for friday, yesterday) |
| `LOC`  | locative   | where |
| `INS`  | instrument | what it was done with |
| `COM`  | comitative | with whom |
| `BENEF`| benefactive| for whose benefit |
| `SOURCE`/`GOAL` | | start / end point |
| `PFROM`/`PTO`   | | motion from / to |
| `ESS`  | essive     | what something *is* (a predicate complement) |

(Full set in `notes/from-marcus/testdef.l`.) The point of cases is that the
*subject* isn't always the *agent* — passives, raising, and control all break
that, and the case tells you the real role.

**grammatical function** — the *surface* slot an argument occupied: `SUBJ`ect or
`OBJ`ect. Each filled role records both ("AGT via SUBJ", "DAT via OBJ"): the case
is the meaning, the function is the syntax. They usually agree but deliberately
needn't.

**filler** — the actual phrase (tree node) that fills a role. In the frame each
role is the triple **`(case filler function)`**, e.g. `(DAT (NP2 . 3) OBJ)` —
*the dative is filled by node NP2, which arrived as an object*.

**case frame** — the bundle of `(case filler function)` triples for one
predicate: its complete predicate-argument structure, i.e. who-did-what-to-whom.
The parser's real output.

**trace** — an invisible placeholder marking a *gap*: a position where something
belongs but no word appears, because it moved or was left understood. In "Who did
the boy see?" the object of "see" is a trace; in "to go" the subject is a trace.

**binding** — the link from a trace to the real phrase it stands for (its
*antecedent*). The embedded `GO`'s subject is `trace -> GIRL`: a trace whose
binding is "the girl".

**control** — when a silent embedded subject is understood as an argument of the
matrix clause. *Object control* (persuade): the matrix **object** is the
controller — *the girl* goes. *Subject control* (promise, want): the matrix
**subject** is — *you* go. Parsifal shows this as the embedded subject trace
being bound to the matrix object (or subject).

**delta** — Marcus's name for the particular empty subject that control fills: a
"base-generated" trace, present from the start and bound later by who controls
it (rather than by syntactic movement). You'll see a controlled subject labelled
`TRACE … DELTA`.

**raising** — like control, but the matrix verb has no role of its own for the
shared argument ("the jar *seems* to be broken" — *seems* assigns the jar no
agent; it is purely the embedded predicate's argument). Contrast control, where
the controller is also a matrix argument.

**complement** — a clause serving as an argument of a verb: the infinitive "to
go" or the that-clause "that the lecture meets". It shows up as a `comp-np` (an
NP wrapper) filling the matrix verb's `NEUT` slot.

**long-distance dependency** — a gap separated from its filler by one or more
clause boundaries: "Who did you say that Bill told __?" — *who* is the object of
the embedded *told*, clauses away. Parsifal handles these deterministically by
*inheriting* the wh-element down into embedded clauses.

**wh-element / wh-question** — the questioned word (who, what, which) and the
question it fronts. "Who broke the jar?" fronts *who*; the gap it leaves is
filled by a trace bound back to it.

---

## How the parser works (PARSIFAL machinery)

You can skip this section unless you're reading or writing grammar rules.

**node** — one item in the parse tree (a clause, phrase, word, or trace). Printed
like `(S3 . 0)`; its data lives in *registers* and *features*.

**feature** — a label on a node, retrieved with `(fe node)`. The root S above is
`(DECL MAJOR S)`: a declarative, major (top-level) sentence. Features drive rule
matching (a rule fires on `[=np]`, `[=verb]`, etc.).

**register** — a named data slot on a node, retrieved with `(getr 'name node)`.
Different node types carry different registers: `word` on lexical leaves,
`binding` on traces, `caseframe` on clause/VP nodes, `dow`/`month`/`hours` on
date nodes. A node has only the registers relevant to it — so `(getr 'word c)`
on a phrase node is correctly `NIL`.

**daughter** — a child node. `(daughter 'np node)` returns the one np child (and
errors if there are several); `(daughters 'np node)` returns the list.

**node types you'll see** — `S` (clause), `NP` (noun phrase), `VP` (verb
phrase), `NBAR` (N′, the noun + its modifiers), `AUX` (auxiliary/tense),
`PP` (prepositional phrase), `QP` (quantifier phrase), `comp-np` (an NP wrapping
a complement clause), `prop-np`/`pron-np` (proper-noun / pronoun NP), `trace`.
Sentence types tag the top S: `decl`, `ynquest` (yes-no), `whquest`, `imper`,
`inf-s`.

**buffer** — the small look-ahead window (about three slots) of upcoming
constituents the parser can see before deciding what to do.

**packet** — a named bundle of grammar rules. The parser switches packets on and
off as it goes (parsing the subject, then the aux, then the verb's objects…), so
only the relevant rules are ever live. Marcus's rules say `IN ss-vp`, `IN cpool`,
etc. — that's the packet they belong to.

**wait-and-see** — Parsifal's defining strategy: with the bounded buffer it looks
ahead just enough to decide, then commits **with no backtracking**. Every rule
application is final — no search, no ambiguity left for later.

**attention-shift (AS)** — the mechanism for pausing the current job to build a
sub-phrase (e.g. shifting attention to assemble an NP, then resuming). Lets the
parser process sub-phrases as sub-tasks.

**node-reactivation (NR)** — the mechanism for revisiting an already-built node
when later context calls for it (e.g. finishing an NP once the clause looks past
it). Each node is NR-checked once.

**creation crule / attachment crule** — rules that fire when a node is *created*
or *attached* under another, used to set up and fill case frames (e.g. `VP-NP`,
which fills a verb's object case when an NP attaches to its VP).

---

## API cheatsheet

```lisp
;; --- setup (once) ---
(ql:quickload :parsifal)
(load "test/integration/clause-grammar.lisp")   ; brings in :parsifal + glang-cl + rule groups
(in-package :parsifal)
(load-full-grammar)                              ; compile + register the whole grammar

;; --- parse ---
(parse-sentence "the boy persuaded the girl to go ."
                :initial-rule (intern "INITIAL-RULE" :parsifal))   ; => T (ok) / NIL (fail)
;; the tree is left in the global  c  (the root S node)

;; --- inspect the tree ---
(fe c)                                              ; node features  => (DECL MAJOR S)
(getr 'word (daughter 'verb (daughter 'vp c)))      ; a word (leaf)  => PERSUADED
(daughters 'np (daughter 'vp c))                    ; the VP's objects (a list)

;; --- a binding (trace -> antecedent): the silent subject of "to go" ---
(let* ((vp     (daughter 'vp c))
       (compnp (find-if (lambda (n) (member 'comp-np (fe n))) (daughters 'np vp)))
       (delta  (daughter 'np (daughter 's compnp))))
  (list (fe delta) (getr 'binding delta)))          ; => ((NP TRACE … DELTA) <the-girl node>)

;; --- the case frame (the meaning) ---
(closeframe openframe)                              ; flush the open frame first (see gotcha 3)
(let ((cf (getr 'caseframe (daughter 'vp c))))
  (list (get cf 'pred)                              ; => PERSUADE
        (cadr (first (get cf 'hypo-slots)))))       ; => ((NEUT …) (DAT …) (AGT …))
```

### Gotchas
1. **`fe` is a macro, not a function** — `(mapcar #'fe …)` errors; use
   `(mapcar (lambda (n) (fe n)) …)`.
2. **Use `daughters` (plural) when a node can have several children of a type** —
   a ditransitive VP has two `np` daughters, so `(daughter 'np vp)` raises
   "multiple daughters".
3. **Call `(closeframe openframe)` before reading a still-open frame's roles** —
   the filled roles are cached in working specials until the frame is closed or
   finalized; without this `(get cf 'hypo-slots)` can look empty. (Embedded
   clauses are already finalized during the parse; the open matrix frame is the
   one that needs it.)
4. **One parse per process is safest** — parsing mutates some shared lexical
   state, so a long run of parses in one image can occasionally interfere. The
   integration tests do one parse per `--load` for exactly this reason.

### Best way to learn it
Read a few `test/integration/*.lisp` files, easy → hard — each parses one
sentence, explains the construction in prose at the top, and asserts exactly
which features, bindings, and case roles to expect. They're written to be read.
