# notes/from-marcus/ — semantic inventory

Companion to `MANIFEST.md` (which records the raw mail-attachment split of
`parse1.orig`). This file captures what each piece of Marcus's bundle
actually *is*, what's missing relative to the system's own loader, and a
priority ask-list for future correspondence.

The bundle dates from 1987-11-22 and represents the state of Marcus's
PARSIFAL implementation at that time. We have **11 of the ~18 source
files** the system needs to run.


## The two "parsers"

When Marcus said the Parsifal parser is "a top down operator precedence
parser adapted from Vaughn Pratt's CGOL parser," he was talking about
**`glang.l`** — the parser that compiles the *rule language* (the
`{RULE ...}` syntax) into Lisp. That file is in the bundle.

The *English parser* — the actual Parsifal that takes a sentence + a
compiled grammar and produces a parse tree — lives in **`parse.l`**,
which **is not in the bundle**. Without it there is no way to parse
English; without it the grammar is just text.

Distinction worth keeping clear:

| | What it parses | Status |
| --- | --- | --- |
| `glang.l` | the rule language `{RULE NAME IN PACKETS [...] --> actions}` → Lisp | **have** |
| `parse.orig` | English sentences using a compiled grammar → parse tree | **have (as of 2026-06-04)** |


## What `load1.l` says the system needs

`load1.l` is the system loader. Its `fasl` and `load` calls enumerate
every file the running system depends on. We use it as ground truth for
"what's missing."

```
(fasl 'parsifal/fixes)        ; in bundle
(fasl 'util/set)              ; MISSING — separate set-theory utility
(fasl 'parsifal/myutil)       ; MISSING (or possibly = util.l, ask)
(fasl 'parsifal/glang)        ; in bundle
(fasl 'parsifal/parse)        ; have as parse.l (from pfiles.orig) + parse.orig
(fasl 'parsifal/case)         ; have as case.l (from pfiles.orig) + case.orig
(fasl 'parsifal/com)          ; have as com.l  (from pfiles.orig) + com.orig
(fasl 'parsifal/util)         ; in bundle (also util.orig from 2026-06-04 delivery)
(load 'parsifal/testdef)      ; MISSING — test fixtures
```

`declr.l` additionally references `util/macros1` — also missing.

**About `.orig` vs `.l`:** Marcus said the `.orig` files are the original MacLISP source; he later converted them to Franz Lisp on the VAX, and *those* are the `.l` files. So:
- For files we have both versions of (`util`, `defs`), the `.l` is the more recent / more portable form.
- For files we have only as `.orig` (`parse`, `case`, `com`), the MacLISP source is what we have. A CL port will need to handle the same MacLISP-isms we already enumerated for `glang.l` (FEXPRs, `if*`, `for`, keyword-named specials, etc.).


## Per-file analysis (what we have)

### `gram4.l` — 281 lines — 1987 grammar (numbers / time / pronouns)

Just one subsystem of the full 1987 grammar. Contains the rules for
parsing number expressions (`NINETY-NINE`, `TWO-HUNDRED`, `BIGNUM`),
time expressions (`TWO-OCLOCK`, `TWO-THIRTY`, `MONTH`, `MONDAY`), and a
few PP/pronoun helpers. **The name `gram4` strongly implies there are
companion files `gram1.l`, `gram2.l`, `gram3.l`** covering clause-level
rules, NP, VP, etc.

### `glang.l` — 616 lines — rule-language compiler (Pratt-style TDOP)

Self-described header:

```
;;;The translator from Parsifal's grammar language into LISP.
;;;Top down operator precedence parser for Parsifal's grammar language
;;;The parser itself is adapted from PRATT;CGOL >
;;;This parser converts the grammar language into LISP
```

Key entry points: `advance`, `:parse`, `associate`, `lederr`. The
`nilfix`, `prefix`, `suffix`, `infix`, `infixr`, `infixd`, `infixm`,
`delim` macros define operator binding-power tables in Pratt's idiom.

This is the most reusable piece in the bundle for understanding how
Marcus parses his rule language. Reading it before designing our own
action-language parser is recommended.

### `defs.l` — 876 lines — feature ontology and constants

Defines the universe of grammatical features the parser knows about:

```lisp
(setq :nr-types '(np nbar s))
(setq :as-types '(noun pronoun verb adj num quant det ord prep poss-np))
(setq :parts-of-speech '(noun verb adj det quant num ord name punc prep pronoun))
(setq :sentence-types '(decl ynquest imper whquest inf-s))
(redund verb (pres past future tnsless))    ; "if verb, then one of these"
(redund auxverb (modal))
(redund comp-obj (inf-obj that-obj))
...
```

Critical reference for what every feature name in the grammar
*means*. The `redund` declarations are redundant-feature implications
that the parser uses to expand sparse feature sets.

### `util.l` — 790 lines — parser interface / display

Top-level entry point `parsifal`, plus REPL plumbing, tracing
(`trace-on`, `trace-off`, `ctrace`, `pstate`), and tree printing
(`bprint`, `wstring`, `stree`, `prule`, `defeat`). Header comment says
"Parser interface and utility functions - none of which are necessary
to run the parser" — i.e., this is the user-facing skin around the
core parser in `parse.l`.

### `pautil.l` — 193 lines — generic Lisp utilities

Version checking (`requiredf`, `version?`, `bump-version`), file I/O
(`readfile`, `writefile`), list/string utilities, numeric/logical/string
predicates. Not parser-specific; closer to a personal Lisp stdlib.

### `patches.l` — 113 lines — runtime patches + tree visualization

MacLISP environment patches (`(*rset t)`, `(sstatus translink nil)`,
`gctwa`) plus the tree-printing routines `display-trace`,
`do-display`, `htree`, `hptree`, `ctree`. The `df meeting feats ...`
line is a single test/example case frame.

### `declr.l` — 73 lines — declarations and core macros

Special-variable declarations (`:activepackets`, `:activerule`,
`:activenodestak`, `:nextrule`, `:bufpntr`, `s c`, `:current-s`,
`:wh-comp`, `1st 2nd 3rd`, …) and core node-manipulation macros
(`flags`, `setflags`, `setup**`, `clear-current-s`, `activatenode`,
`setfe`, `fe`, `setr`, `getr`).

Most parser source files `(declare (include declr))`. Note the
reference to `util/macros1` at the top, which is **missing**.

### `macros2.l` — 57 lines — parser-language macros

`say`, `meet` (= intersection), `make-eq-set`, `set-union`,
`set-difference`, `set-intersection`, `warn`, `cat` (= concat),
`default-arg`. Sugar used in the parser source files.

### `fixes.l` — 13 lines — MacLISP compatibility shims

`le`, `ge`, `consprop`, `[` and `]` as syntax, plus a few `(putd 'X
(getd 'Y))` aliases. Reads like a bring-up file for moving between
MacLISP dialects.

### `load1.l` — 18 lines — system loader

(Already discussed above.) The authoritative list of what the running
system loads.

### `parsifal.help` — 27 lines — user help text

REPL command reference: `time`, `ctree`, `ftree`, `tree`, `ntree`,
`ltree`, `p` (call the parser), `(ptrace)`, `(runfast)`, `(ctrace t)`,
`(noshow ...)`, `(pstate)`.


## What's missing — priority order for asking Marcus

Items resolved on 2026-06-04 (`parse`, `case`, `com`) moved to
"Received 2026-06-04" below.

1. **The other grammar files** — `gram1.l`, `gram2.l`, `gram3.l` (or
   whatever the actual filenames are) that contain clause-level / NP /
   VP rules. `gram4.l` alone is just numbers and time. Marcus has said
   he'll look for these.

2. **`util/macros1`** — referenced explicitly by `declr.l`. Without it
   any compilation of the parser files will fail.

3. **`util/set`** — the set-theory utility loaded by `load1.l`. Used
   for the `meet`/`union`/`setminus` operations in `macros2.l`.

4. **`myutil`** — `load1.l` loads `parsifal/myutil`. We have `util.l`
   but not `myutil`; they may be the same file under different names,
   or `myutil` may predate `util.l`. **Worth asking explicitly so we
   don't double-up.**

5. **`testdef`** — `load1.l` ends with `(load 'parsifal/testdef)`. This
   is the test-sentence corpus. Useful for validating any grammar we
   compile.


## Received 2026-06-04

Marcus delivered four additional files (`parse.orig`, `case.orig`,
`com.orig`, `util.orig`, plus a copy of `defs.l` that turned out to be
byte-identical to the one we already had once mail-folder headers were
stripped). Three of them are exactly the top-priority pieces from the
ask-list:

| File | Lines | Role | Key contents |
| --- | --: | --- | --- |
| **`parse.orig`** | 658 | **English parser.** The single most important file in the system. | Top-level bindings; `:buffer`, `:1stfvec`, `:2ndfvec`, `:3rdfvec` arrays; node-manipulation macros (`flags`, `setup**`, `activatenode`, `fast-is`, `setfe`, `fe`, `setr`, `getr`); the wait-and-see parse loop. |
| **`case.orig`** | 602 | **Case-frame mechanism** (Marcus's Appendix E). | Defines the case-frame datatype with properties `CASE-FRAME` (NORMAL or MOD), `ASSOC-NODE`, `SPEC`, `CASES`, `PRED`, `HYPO-SLOTS`, `OBJS-NEEDED`. Functions: `newcf`, `associate-cf`, `assoc-node`, `maxunls`, `minunls`, `need-slots`, `fits`, `open-obj-cases`. Notably commented by Kurt Van Lehn. |
| **`com.orig`** | 589 | **Command interface, REPL, morphology.** | `sentin` (sentence input), `morpho` (morphology), word-tree manipulation (`get-string`/`put-string`), case handling (`origcase`, `lowcaseify`), and the `df+` fexpr for user-defined word definitions. |
| `util.orig` | 790 | MacLISP-original of the `util.l` we already have. | Diff against `util.l` if we ever care about the conversion history; otherwise the `.l` version is what we'd port. |

**About `.orig` vs `.l`:** Marcus said the `.orig` files are the
original MacLISP source; he later converted them to Franz Lisp on
the VAX, and *those* are the `.l` files. For `parse`, `case`, `com`,
and `util` we have both, side-by-side in this directory. The `.l`
files are the more recent / more portable form (Marcus made his Franz
versions work after writing his own Franz debugger; treat them as the
canonical source for porting).


## Received 2026-06-05

Marcus delivered the rest of the system as a mail bundle in
`pfiles.orig`, plus standalone `testdef.l` and a parsifal.help copy
identical to the one we already had (discarded). The bundle was split
into its 10 attachments (see `PFILES_MANIFEST.md` for the table):
**`parse.l`**, **`case.l`**, **`com.l`** (the long-asked-for Franz
Lisp versions of the parser, case frames, and REPL), **`gram1.l`**,
**`gram2.l`**, **`gram3.l`**, **`gram5.l`** (the missing grammar
files — gram4 was already in hand from the first bundle), **`load.l`**
(the system loader Marcus actually used), **`xutil.l`** (extra
utilities), and **`newdef.l`** (likely user-word-definitions; declr.l
references `:newdeffile`).

`testdef.l` is the test-sentence corpus referenced by `load1.l`'s
`(load 'parsifal/testdef)` line. We now have everything `load1.l`
expects except `util/set` and `util/macros1`.


## Notes about dialect

These files are MacLISP, not Common Lisp. Several constructs won't run
under SBCL as-is:

- `(declare (special ...))` instead of `(declaim (special ...))`
- `(declare (macros t))` (compiler-mode marker)
- `(fasl 'parsifal/foo)` (MacLISP's file-loading)
- `(defun NAME nil ...)` for zero-arg defun (instead of `(defun NAME () ...)`)
- `(defun NAME macro (l) ...)` for old-style `defmacro`
- `(defun NAME fexpr (l) ...)` for fexprs (no CL analogue)
- `:keyword-style` symbols used as regular variables (e.g. `:activerule`)
- `if*`, `do`, `prog`, `terpri-hack`, `cursorpos`, `tyo`/`tyi`
- `(setsyntax '\[ 2)` reader-table hacks
- `(*lexpr say-it warner)` lambda-list-keyword declarations
- `(putd 'X (getd 'Y))` for function aliasing

When porting any file to Common Lisp, plan on substantial rewriting,
not just minor surgery.
