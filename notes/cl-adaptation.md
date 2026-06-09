# CL Adaptation Notes

This document records the decisions made when porting Marcus's PARSIFAL from
MacLISP / Franz Lisp to Common Lisp. It is *not* a complete diff. It is the
ruleset we apply and the reasoning behind it, so a future reader can
distinguish *Lisp-dialect mechanical changes* from *design choices we made*.

The document grows incrementally — each new file we port adds a section under
[Per-file notes](#per-file-notes).


## Purpose

PARSIFAL is being ported with a "minimal-change" philosophy: change as little
as we can while making the code load and run on a modern ANSI Common Lisp
(SBCL, primarily). The goal is preservation, not modernization. When we
deviate from Marcus's original — and we sometimes will — the deviation is
documented here so it can be re-examined later.

Three audiences read this document:

1. Future researchers studying PARSIFAL who want to know which lines came
   from Marcus and which came from us.
2. Future maintainers (likely future-us) deciding whether a given line is
   safe to refactor.
3. Marcus, if he ever wants to know what we did to his code.


## Provenance

Marcus delivered the source in three email batches in June 2026 (see
`notes/from-marcus/INVENTORY.md` for the full provenance log). The relevant
forms are:

- **`*.orig`** — Marcus's original MacLISP source from 1987, preserved
  verbatim in `notes/from-marcus/`.
- **`*.l`** — Marcus's Franz Lisp ports of those same files, made when he
  moved from Bell Labs to Penn. He noted that Franz on the VAX lacked
  adequate debugging tools and that he had to write his own before getting
  the parser running there. The `.l` files are the result of that work and
  are the **canonical source for porting** — they're the more recent and
  more tested form.
- **CGOL** — Pratt's MIT AI Lab Working Paper 121 (1976) is the ancestor of
  Marcus's grammar-language parser; included as
  `notes/from-marcus/CGOL-Pratt.pdf` for reference. The `nud` / `led` /
  binding-power terminology comes from there.


## Minimal-change ruleset

The mechanical translations below are applied as a matter of course. Each is
a Lisp-dialect concern, not a design choice; reviewers can skim past them.

| MacLISP / Franz Lisp | Common Lisp | Notes |
| --- | --- | --- |
| `:keyword` used as a variable name | `*keyword*` (earmuffs), declared special | CL's `KEYWORD` package is constant; `:token`, `:left`, `:rulename`, `:drbp` become `*token*`, `*left*`, `*rulename*`, `*drbp*`. |
| `(declare (special X))` inside a defun | `(declaim (special *x*))` at top level | Matches the variable rename above. |
| `(defun NAME nil ...)` | `(defun NAME () ...)` | Empty lambda list. |
| `(defun NAME macro (l) ...)` | `(defmacro NAME ...)` | MacLISP's `macro` lambda-list keyword; bodies usually translate one-to-one. |
| `(defun NAME fexpr (l) ...)` | `(defmacro NAME (&rest l) ...)` or a function operating on quoted args | FEXPRs have no direct CL analogue; macros are the safe substitute when the original was used at expand time. |
| `(declare (macros t))` | drop | MacLISP compiler mode marker. |
| `(declare (include declr))` | drop; use ASDF | File-inclusion replaced by system loading. |
| `if*` | `cond` / `if` / `when` / `unless` | Marcus's `if*` is MacLISP's `if`: implicit progn over the then-clauses, no else. Translation depends on shape. |
| `for (b1 v1 b2 v2 ...) body` | `let` / `let*` / `do` / `loop` | Marcus's `for` is multi-binding `let` with progn body. Picking the CL form depends on whether bindings are sequential and whether the body iterates. |
| `prog` + `go` labels | `prog` + `go` labels | **Preserved verbatim.** CL has both; rewriting risks semantic drift. |
| `(fasl 'parsifal/X)` | drop | ASDF or `(load ...)` replace MacLISP's file loader. |
| MacLISP arrays — `(array NAME TYPE SIZE)`, `(arraycall TYPE A I)` | `(make-array SIZE :element-type ...)` and `(aref A I)` | CL arrays cover the same cases. |
| `putd`, `getd` | `(setf (symbol-function ...) ...)` and `symbol-function` | Function-cell access. |
| `defprop`, `putprop` | `(setf (get ...) ...)` | Direct property-list write. |
| `setplist`, `remob` | `(setf (symbol-plist ...) ...)`, `unintern` | Plist and intern-table management. |
| `cat` (MacLISP `concat`) | `(intern (format nil "~A~A..." x y ...) :package)` | MacLISP's `concat` concatenates print-names of its args and returns a **symbol**, not a string. Used throughout for synthesizing names like `::pat-of-FOO`. CL needs `intern` to make a symbol. |
| `memq`, `delq`, `eq` family | `member ... :test #'eq`, `delete ... :test #'eq` | Use the explicit-test forms; behavior matches. |
| `(setq A 1 B 2)` — multiple pairs | `(setq A 1 B 2)` | Works the same in CL. No change needed. |
| `tyo`, `tyi`, `tyipeek`, `cursorpos` | `write-char`, `read-char`, `peek-char`, custom terminal control | I/O primitives substituted per file. |

For everything not listed, the default is **preserve verbatim**.


## Marcus's own utilities (not MacLISP)

Several things that look MacLISP-flavored in the source are actually
Marcus's own utility functions defined in `fixes.l`, `patches.l`, or
`macros2.l`. They are not part of the MacLISP language and become trivial
in CL once you know they're Marcus's. Identifying them avoids both
unnecessary translation work and false claims of "MacLISP-ism" in this
document.

| In source | Defined where | CL substitute |
| --- | --- | --- |
| `le`, `ge` | `fixes.l` | `<=`, `>=` |
| `consprop` | `fixes.l`; defined in terms of MacLISP's `putprop` | `(push val (get sym prop))` — direct in CL; no helper needed |
| `linel` | `fixes.l` | one-line accessor |
| `if*` | `patches.l` (alias to MacLISP `if`) | translate per the `if*` row in the ruleset above; `if*` is *Marcus's name* for MacLISP `if` |
| `meet`, `make-eq-set`, `make-equal-set`, `set-union`, `set-difference`, `set-intersection` | `macros2.l` (aliases to MacLISP set ops) | CL set primitives |
| `say`, `warn` | `macros2.l` | small CL macros over `format` |
| `default-arg` | `macros2.l` | `&optional` parameter defaults |
| `logand`, `logor` | `macros2.l` (aliases to `boole`) | CL's `logand`, `logior` — same names, conveniently |
| `meet1`, `meet2`, `union1`, `setminus1` | `macros2.l` (set-op variants) | CL `intersection`, `union`, `set-difference` with appropriate `:test` |

`fixes.l` *also* aliases some MacLISP internals (`cat`, `charpos`,
`sprinter`, `sprint`) — those *are* MacLISP-isms via a Marcus alias and
appear in the main ruleset above.


## Preserved deliberately

These are *not* dialect-mechanical. They look unusual to modern CL eyes but
they are part of Marcus's design and we keep them on purpose.

- **`prog` + `go` labels.** Marcus's `parse` and `set*` use multiple jump
  targets that don't decompose cleanly into structured control flow.
  Rewriting them risks subtle bugs in a parser whose correctness is hard
  to reverify.
- **Symbol-property storage for node features and registers.** Each parsed
  node's features live in the symbol-value of a gensym; registers live on
  its property list. CL supports both, and the `setr` / `getr` / `setfe` /
  `fe` accessor macros translate one-for-one. We considered defstruct slots
  and rejected them — Marcus's representation is documented in his book and
  used by surrounding code in ways that go beyond simple slot access.
- **The buffer as a mutable array.** The wait-and-see lookahead buffer in
  `parse.l` is an in-place mutable array of size 10, with `insert-index-pos`
  and `remove-index-pos` shifting contents. This is the architecture; we
  don't switch to a queue or a list.
- **The `:nextrule` / `:parsecomplete` indirection.** Rule actions don't
  call the main loop directly; they set globals that the loop reads at the
  top of each iteration. It's the cleanest control-flow interface in
  `parse.l` and we preserve it exactly.
- **Marcus's binding-power numbers in `glang.l`.** The values (bp 1 for
  `.`, bp 10 for the flat action-verb tier, bp 17 for `,`, bp 25 default,
  etc.) encode a deliberate design — they're not arbitrary. See the
  `marcus-bp-design` project memory for the longer story; the short
  version is "do not round or normalize them."
- **Marcus's vocabulary on symbols.** Property indicators `:nud`, `:led`,
  `:lbp` come from Pratt's CGOL paper and Marcus uses them verbatim. We
  preserve the names so a reader can compare side-by-side with `glang.l`.


## Deliberate deviations

Where we have departed from Marcus's original, the deviation and its reason
are recorded here.

### `glang-cl/pratt.lisp` — `associate` stops at `*eof*`

Marcus's `associate` (glang.l line 29-35) signals `lederr-bug` on EOF.
glang-cl's `associate` instead returns the current `*left*` value when
`*token*` is `*eof*`. This lets standalone expressions parse cleanly without
the rule-closing `}` that Marcus's outer driver guarantees. The rule
language always *does* have a closing `}`, so the deviation never fires in
practice; it surfaces when running unit tests against arithmetic-style input
during parser bring-up.

### Token case: uppercase, not lowercase

Marcus turns on MacLISP's `uctolc` ("upper-case to lower-case") flag in
`glang-read`, so all his symbols are interned as lowercase. CL's default
reader uppercases identifiers; matching the default is far simpler than
overriding it, so glang-cl tokens are uppercase. The compiled-Lisp output
therefore prints in uppercase rather than Marcus's lowercase. The two are
informationally equivalent.

### Reader hackery replaced by a custom tokenizer

Marcus uses MacLISP's `setsyntax` to make `[`, `]`, `{`, `}`, `(`, `)`, `'`,
`;`, `,`, `.`, `=` into single-character symbols, then calls `(read)` to
get tokens. CL's reader doesn't expose this kind of per-character
configuration cleanly. glang-cl ships a small hand-written tokenizer
(`tokens.lisp`, ~120 lines) that produces the same symbol stream Marcus's
`read` would have. The grammar surface is the same.

### `denfun` / `denfunc` / `buildfun` not ported

Marcus's `denfun` and `denfunc` are runtime-expansion helpers for
denotation bodies; `buildfun` is a pattern-matching utility for consuming
literal tokens while building a result form. They depend on MacLISP FEXPR
semantics that don't have a clean CL equivalent.

In glang-cl we write denotation bodies inline using ordinary CL idioms.
Example: Marcus's `(prefix activate 10 (denfun 'activate (kwote (getvarlist))))`
becomes
`(prefix activate 10 (list 'activate (list 'quote (get-var-list))))`.
The translation is mechanical and shorter; the cost is that we don't
benefit from `buildfun`'s expectation-checking when consuming literal
tokens. For now this is acceptable; we may revisit if it causes errors as
we port more action verbs.

### Error reporting via CL conditions

Marcus's `lederr` throws `'lederr-bug` to a catch handler in the outer
driver. glang-cl signals a `glang-parse-error` condition instead. CL's
condition system gives better error messages with stack context; the
loss is interactivity-with-debugger, which we don't currently need.


## Translation limits

A small set of MacLISP-isms don't have clean CL substitutes. When we hit one
in a file we're porting, we record the file-specific workaround here.

- **MacLISP readtable manipulation** — `setsyntax`, the `vmacro`,
  `vsingle-character-symbol`, `vsplicing-macro`, `vcharacter` syntax
  classes. CL's `set-macro-character` and `set-syntax-from-char` cover
  *most* of the same ground, but the syntax classes don't align one-for-one.
  Our workaround so far: replace with hand-written tokenizers (see
  `glang-cl/tokens.lisp`).
- **FEXPRs** — translated to macros where they were used at expansion time;
  to functions taking quoted args where they were used at runtime. Subtle
  semantic differences possible; flagged per-occurrence.
- **`(comment ...)` as a top-level form** — Marcus uses this for section
  headers. CL has no `comment` operator. We either:
  (a) preserve them as `;;;` comments in the CL output, or
  (b) wrap them in `(defun comment (&rest x) (declare (ignore x)))` and
  ignore. Choice TBD per file.
- **`#testdef.l#` / `.#testdef.l` Emacs autosaves** — irrelevant to the
  port; filtered via `.gitignore`.


## Per-file notes

Each section below documents the decisions made porting one specific
Marcus file. Sections are added as files get ported; before that, an entry
is a placeholder.

### `glang.l` — *ported (in progress)*

**Location of port:** `system/reference/glang-cl/`

**Scope completed:**
- Pratt parser machinery (`advance`, `verify`, `associate`, `pratt-parse`,
  `lederr`)
- Fixity macros (`nilfix`, `prefix`, `suffix`, `infix`, `infixr`, `infixd`,
  `infixm`, `delim`)
- Rule machinery: `rule` (prefix + infix), `[`, `]`, `,`, `-->`, `--->`,
  plus `parse-rule`, `parse-rule-header`, and `compile-rule-form`
- Action sequence: `.` (infix), `;` (infixm — `and` inside patterns,
  `progn` elsewhere)
- Pattern feature match: `=` with `build-=` and `pick-index`
- Relational infixes: `is`, `not`, `none`, `any`
- Action verbs: `activate`, `deactivate`, `restore`, `run`, `parse`,
  `attach`, `drop`, `insert`, `label`, `remove`, `transfer`, `features`,
  `set`
- Tree-access infixes: `of`, `above`; prefixes: `node`, `binding`;
  infix: `register`
- Control flow: `if/then/else/andthen`; logical `and`/`or` (infixm)
- Grouping / function call: `(` (prefix grouping, infixd call), `)`
  (delim)
- Nilfix atoms: `last`, `it`, `current`, `wh-comp`

Two complete `gram4.l` rules now compile end-to-end (`NUMBER` and
`NUMBER-DONE`) — verifies the full pipeline: tokenize → Pratt-parse →
intermediate AST → emit Marcus's compiled-Lisp triple.

**Scope not yet done:** remaining action verbs (`lift`, `meet`, `word`,
`make`, `there`); the quoting operator `'`; test-pattern
denotations (`fills`, `fits`, `greater`, `less`, `equal`, `lowest`,
`greatest`, `number`, `semantics`, `filling`, `prepositional`); the
`indirect` tree-access prefix; and the case-rule denotations (`crule`,
`upper`, `lower`).

**Per-decision references:**
- Symbol case → uppercase (see *Deliberate deviations* above)
- Tokenizer → custom (see *Deliberate deviations* above)
- `denfun` / `buildfun` → not ported (see *Deliberate deviations* above)
- `associate` → EOF-tolerant (see *Deliberate deviations* above)
- Binding powers → preserved verbatim from Marcus (see *Preserved
  deliberately*)


### `declr.l` — *ported*

Lives at `system/core/runtime/declr.lisp` in the `:parsifal` package.

The special-variable list translates to a block of `defvar` forms plus
a top-level `(declaim (special ...))`. Two naming rules:

- Marcus's `:keyword`-prefixed names (`:activepackets`, `:current-s`,
  `:wh-comp`, ...) cannot survive because CL's `KEYWORD` package is
  constant — they become earmuffed specials (`*activepackets*`,
  `*current-s*`, `*wh-comp*`).
- Plain symbols Marcus declared special (`s`, `c`, `1st`, `2nd`, `3rd`,
  `nth`, `tyo`, `tyi`, `pred`, `hypoth-frames`, etc.) are preserved
  verbatim. glang-cl emits these names directly into compiled rule
  bodies, so renaming would force a parallel change on the parser-output
  side.

The macros (`flags`, `setflags`, `fe`, `setfe`, `setr`, `getr`,
`clear-current-s`, `activatenode`, `setup**`) translate directly —
`store` → `(setf (aref ...) ...)`, `symeval` → `symbol-value`, `putprop`
→ `(setf (get ...) ...)`, `if*` → CL `if`. `setup**` keeps Marcus's
free reference to the caller's lexical `NODE`; we document the
contract rather than re-design it.

Dropped: `(fasl ...)` (→ ASDF), `(declare (macros t))` (compiler-mode
marker), `(*lexpr ...)` (CL `&rest` covers it), `(setsyntax '\# 2)`
(handled by `|...|`-escape on the single symbol that needed it).


### `parse.l` — *partial port (primitives + buffer ops)*

Split across two files:

- `system/core/runtime/primitives.lisp` — the leaf primitives that
  don't touch the buffer or packet stack:
  - Feature-vector matching: `fast-is`, `testindices`, `featindexify`
  - Feature-list mutation: `addf1`, `remf1`
  - Relational predicates: `is`, `is-not-all-of`, `is-none-of`,
    `is-any-of`
  - Transfer: `transfer`, `liftr`
  - Adaptation utilities: `for` (Marcus's `util/macros1`, which he
    hasn't delivered), `consprop` (from `fixes.l`)
- `system/core/runtime/buffer-ops.lisp` — buffer/packet machinery:
  - Top-level array bindings: `*buffer*`, `*1stfvec*`, `*2ndfvec*`,
    `*3rdfvec*`, `*index-to-fvec-alist*` (Marcus's `parse.l` lines
    1-15)
  - Buffer maintenance: `insert-index-pos`, `remove-index-pos`,
    `bufrestore`
  - Packet activation: `activate`, `deactivate`
  - Attach / drop / insert: `attach`, `attach1`, `drop`,
    `insert-node`
  - Stubs for case-frame hooks: `attach-monitor`, `create-monitor`
    (no-ops until `case.l` is ported), and `rule-index` (no-op until
    the main parse loop is ported)
- `system/core/runtime/node-ops.lisp` — node creation and tree
  traversal:
  - Node creation: `makesym`, `makenode`, `newnode`
  - Tree search: `daughters`, `daughter`, `father-node`, `node-above`,
    `binding`, `find-node`, `find-node1`, `io`
  - Current-S / wh-comp: `setup-current-s`, `current-s`, `wh-comp`,
    `s-type`
  - Identity helper: `nid1`
- `system/core/runtime/parse-loop.lisp` — the wait-and-see main loop
  in MVP form (~enough to actually run a grammar):
  - Rule indexing: `*rule-table*`, `rule-index`, `rem-index`,
    `testrules`, `act-of-rule`, `reset-rule-table`
  - The loop itself: `parse-loop` (preserves Marcus's `PROG` + `GO`
    structure with `runrule` and `nextrule` labels)

MVP deviations in `parse-loop.lisp` worth knowing:

- **Rule storage.** Marcus indexes rules by feature using nested cons
  cells (a "type-plist" living in the cdr of `(ncons nil)`) so that
  `fetchrules` can do quick lookups via the buffer head's feature
  list. We store rules in `*rule-table*`, a CL hash-table keyed by
  packet, and `testrules` walks all rules of each active packet
  linearly, sorting by priority. Same observable behaviour, slower
  for big grammars. Feature-indexed buckets can be reintroduced
  later without touching emissions.
- **`act-of-rule` lookup.** Marcus's loop reconstructs the action
  function's name by string-concatenating `::act-of-` with the rule
  name. We instead have `rule-index` stash the act-fn under
  `(get rule-name :act-fn)`, so a rule chained via `*nextrule*`
  doesn't need to live in any particular package.
- **Buffer advance.** `set*`, `setup*`, `nextword`, and `as-check`
  are ported. `set*` pulls from `*wstring*` into the buffer, evicts
  already-attached nodes, fills the buffer registers via `setup*` /
  `setup**`, then runs `as-check`: if the new node has any feature
  in `*as-types*` AND an AS-typed rule's pattern matches, `*activerule*`
  gets set and `set*` returns NIL so the loop fires it before
  consulting NORMAL rules. **NR rules** are still deferred --
  Marcus's NR-check at the top of `set*` needs the bit-2
  "NR-checked" flag bookkeeping we haven't ported yet.
- **Sentence input.** Marcus's `(sentin)` reads a sentence string and
  builds the initial `*wstring*` of word-nodes. That code lives in
  `com.l` (not ported). The MVP expects the caller to populate
  `*wstring*` with pre-built nodes; a real lexicon / morphology
  belongs to the next batch.
- **setup**.** Marcus's `setup**` (in declr.l) only works correctly
  when every feature already has a `:findex` (`defs.l` does that for
  the standard ontology); the "no :findex" branch conses NIL onto the
  saved index list, which breaks the next cleanup pass. Our port
  assigns `:findex` lazily inside `setup**` -- same logic as
  `featindexify` -- so any feature works even if no pattern ever
  named it.
- **Trace / break / display.** `cursorpos`, `drain`, `display-trace`,
  `break beforerun`, and the `say` chatter are no-ops here. They
  affect the TTY parser experience, not correctness.

Notes on the port:

- `fast-is` is a MacLISP `defun NAME macro` (old-style defmacro) that
  runs `featindexify` at macro-expansion time. Our CL `defmacro
  fast-is` keeps that semantics — the expanded form calls `testindices`
  with a list of integer indices baked in. The feature symbols must
  therefore have `:findex` properties set by the time the rule body is
  compiled; `featindexify` assigns them lazily on first reference.

- Marcus's `daughters` register stores a "disembodied property list"
  — a `(ncons nil)` cons cell whose CDR he extends with MacLISP's
  `putprop`. CL's `get` only works on symbols, so we substitute a
  fresh uninterned symbol from `gensym`. Same role (a private namespace
  for type→daughters mappings), works with CL's plist accessors
  natively.

- Several CL standard set ops don't preserve element order the way
  MacLISP's `setminus`/`intersectq2` do. `remf1`, `transfer`, and
  `deactivate` use `remove-if` / `remove-if-not` loops to keep order
  stable.

- `attach-monitor` and `create-monitor` are case-frame hooks defined
  in `case.l`. Until that file is ported they are stubbed as no-ops.

**Recently landed (parse.l small helpers):**

- `buffer-gc` (parse.l 175-185) — ported into `parse-loop.lisp` and
  wired into the loop's `nextrule` label as `(when *buffer-gc* (buffer-gc))`.
  `*buffer-gc*` now defaults to `t` (parse.l 23), matching Marcus.
- `:last` (parse.l 565) — ported as `last*`; a keyword can't head a CL
  call form, so glang-cl's `last` denotation now emits `(last*)`.
- `alt-attach` (parse.l 583) — records a deferred attachment on `s`.

**Not yet ported from parse.l:**

- Node cleanup: `node-reset`, `nodegc` (uninterning + `cat`)
- AS / NR rule dispatch from `set*` (the bit-2 NR-checked bookkeeping)
- Phrase-structure helpers: `head`, `word`, `root-of`
- Tree printing: `nid`, `node-id` (need `phrasify` from util.l)
- `alt-fillslot` (needs `putc` from case.l)
- The full `parse` driver (needs `sentin` from com.l)
- Debug / timing: `endtime`, `starttime`, `ruletrap`, `breaksw`

**Package alignment between glang-cl and parsifal — resolved.**

`:parsifal` exports the runtime API (functions, macros, special vars)
and `:glang-cl` does `(:use #:cl #:parsifal)`. This means:

- Unqualified references in denotation source (`(prefix attach 10 ...)`,
  `(list 'attach ...)`) read in `:glang-cl` resolve via inheritance to
  the `parsifal:attach` symbol.
- The tokenizer's `(intern "ATTACH" :glang-cl)` finds the same
  inherited `parsifal:attach` and returns it, so an emitted
  `(ATTACH C 1ST 'NP)` evaluates against the parsifal runtime
  directly.
- Symbols *not* in parsifal's exports (rule names, feature names,
  packet names like `NP`, `CPOOL`, `NINETY-NINE`) still intern into
  `:glang-cl`. They are grammar data, not runtime API, so a separate
  namespace is the right home for them.

Two related fixes landed in the same commit:

- `denotations.lisp` previously emitted `(setq :nextrule ...)` and
  `(setq :parsecomplete t)` -- invalid CL because keywords can't be
  SETQ'd. Now emits `*nextrule*` / `*parsecomplete*` to match the
  parsifal earmuff-rename convention.
- `buffer-ops.lisp` gains a `rule-index` stub (no-op) so a
  freshly-emitted PROGN top-level form -- which calls `rule-index`
  to register the rule -- evaluates cleanly until the real
  parse-loop machinery is ported.

End-to-end smoke test: a glang-cl-compiled `Attach 1st to c as np.
Deactivate cpool.` rule, eval'd against parsifal-bound `c`, `1st`,
`*activepackets*`, actually mutates the runtime state correctly.


### `macros2.l` — *ported (what survives) at `runtime/macros2.lisp`*

Small file (57 lines), and most of it dissolves into the CL standard
library rather than becoming code. A usage census across `parse.l`,
`case.l`, `com.l`, and `util.l` drove the disposition:

- **`say` (56 uses) — ported.** A trace/print macro: `(say a b $ x)`
  prints the literal tokens `a`, `b` then the *value* of `x`; the `$`
  marker flips an item from quoted to evaluated. We keep Marcus's
  split between the macro (the quote/escape transform) and the printer
  (`say-it`), porting both. `say-it` is the minimal `terpri` +
  space-separated `princ` version; util.l's richer `say-it1`/`say1`
  (the `$$` splice marker, cursor/highlight control) supersede it when
  util.l lands.
- **`warn` (14 uses) — CL's native `cl:warn`.** Marcus's
  `(warn LEVEL items…)` becomes `(warn "message")`, dropping the
  numeric severity. `buffer-ops.lisp` already does exactly this.
  Defining a `warn` macro would shadow `cl:warn`, so we deliberately
  don't — `warn` is *not* in `macros2.lisp`.
- **`meet`→`intersectq`, `meet1`→`intersectq2`, `union1`→`unionq2`,
  `setminus`** — these named MacLISP's external `util/set` library
  (not in our sources). On feature-symbol lists, `cl:intersection` /
  `cl:union` / `cl:set-difference` (default `eql`) are identical;
  where Marcus relies on survivor order (`setminus`), use the
  `remove-if` idiom as `deactivate` does. Handled at each call site.
- **`logand`/`logor`** — `cl:logand`/`cl:logior` are native.
- **`cat`→`concat` and `default-arg`** — deferred to util.l (their
  only remaining consumers). `cat`'s three `parse.l` uses are already
  obsoleted by this port: `makesym` (node-ops) builds node symbols and
  `act-of-rule` (parse-loop) replaced the `(cat ':act-of- …)` symbol
  synthesis with a plist lookup.
- **Dropped — zero call sites and natively available:** `meet2`,
  `setminus1`, `set-union`, `set-difference`, `set-intersection`,
  `make-eq-set`, `make-equal-set`. (`set-difference` must *not* be
  redefined as a macro — it is a CL standard function.)

The MacLISP `(declare (macros t))` pragma and the `(*lexpr …)`
declaration have no CL equivalent and are dropped. Tests in
`test/macros2-test.lisp`.


### `fixes.l` — *ported (dissolves entirely; no new file)*

Pure dialect-shim file (13 lines); nothing in it needs a `.lisp` of
its own. Disposition:

- `le`/`ge` → CL `<=`/`>=` at the two `parse.l` call sites.
- `consprop` → **already ported** (`primitives.lisp`), used by
  `makesym`.
- `(putd 'cat (getd 'concat))` — the `cat`=`concat` alias; deferred
  alongside `cat` (util.l).
- `(setsyntax '[ …)` / `(setsyntax '] …)` — reader-syntax for the
  bracket forms, handled by our `rule-lexer` tokenizer, not the CL
  reader.
- `linel`/`charpos`/`sprinter`/`sprint` — terminal/display indirections
  that belong with util.l's display plumbing.


### `defs.l` — *partial port (feature ontology only)*

Lives at `system/core/runtime/defs.lisp`. Ports lines 1-84 of
Marcus's source — the parser's structural foundation:

- The canonical feature lists `*nr-types*`, `*as-types*`,
  `*parts-of-speech*`, `*sentence-types*`, `*specregs*`, plus
  `refillables`. `*as-types*` and `*nr-types*` are already declared
  in declr.lisp; defs.lisp just `setf`s them to their proper values.
- The `redund` macro: every declaration records its implications in
  `*redund-table*` (an EQ hash from feature symbol to implication
  list). The expansion logic that consults the table is parser-side
  work for later.
- All seven `redund` declarations from Marcus's source applied.

The feature symbols Marcus mentions (`np`, `verb`, `decl`, `tens`,
`ones`, `complete-num`, etc.) are interned in :parsifal and exported
at compile time, so glang-cl-tokenized grammar source -- which lives
in :glang-cl with `(:use #:cl #:parsifal)` -- finds the same symbol
identity. With this in place, the integration tests no longer need
the `(setq *as-types* (list (intern "NUM" :glang-cl)))` hand-wiring
that scenarios used to require.

**Deferred:** lines 85-876 -- the lexicon. Hundreds of verb, noun,
and adjective definitions via `(df ...)`, `(df1 ...)`, `(df+ ...)`,
`(jlike ...)`, `(abbrev ...)`, and `(irreg ...)`. These don't matter
until we're parsing real strings, which needs `com.l`'s `sentin` /
`morpho` ported -- so the lexicon comes alongside that.


### `parse.l` — *not yet ported*

The English parser. See the `parse-orig-architecture` project memory for
the architectural overview. Likely porting order, bottom-up:

1. Data-structure accessors (`flags`, `setflags`, `fe`, `setfe`, `getr`,
   `setr`)
2. Buffer operations (`insert-index-pos`, `remove-index-pos`, `nextword`,
   `:last`)
3. Feature operations (`addf1`, `remf1`, `is`, `is-any-of`, `is-none-of`,
   `transfer`, `liftr`, `featindexify`, `testindices`)
4. Tree navigation (`node-above`, `father-node`, `find-node`, `binding`)
5. Node creation and attachment (`newnode`, `makenode`, `attach`,
   `attach1`, `drop`)
6. Rule indexing (`rule-index`, `rem-index`, `fetchrules`, `testrules`)
7. The main `parse` loop


### `case.l` — *not yet ported*

Case-frame mechanism (Marcus's Appendix E). Kurt Van Lehn's comments at
the top of the file make the data-type design unusually clear. The
case-frame is a gensym with properties `CASE-FRAME` (NORMAL or MOD),
`ASSOC-NODE`, `SPEC`, `CASES`, `PRED`, `HYPO-SLOTS`, `OBJS-NEEDED`. CL's
property-list machinery handles all of this directly.


### `com.l` — *not yet ported*

REPL, sentence input (`sentin`), morphology (`morpho`), word-tree
manipulation. The REPL needs CL-style stream handling; `morpho` uses
character-level processing that will translate via `peek-char` and
`read-char`.


### `util.l` — *not yet ported*

Parser-interface / display utilities. Header comment says "none of which
are necessary to run the parser" — accurate. Tree-printing, REPL plumbing,
tracing. Will likely port last.


### `pautil.l`, `patches.l`, `xutil.l`, `load.l`, `load1.l`, `newdef.l`, `testdef.l`

Not yet examined in detail. Notes will be added when porting begins.


### `gram1.l`–`gram5.l`

These are *grammar files*, not Lisp source — they're consumed by `glang-cl`
(or Marcus's `glang.l`) to produce compiled Lisp. They don't need a port;
once `glang-cl` covers enough denotations to compile them, they'll be
processed as-is.


## Acknowledgments

Decisions about minimal-change vs. modernization track conversations with
Mitchell Marcus throughout the porting work. Where this document
"deliberately deviates" from his original, the deviation is ours and the
responsibility for it is ours; the original code is what's preserved in
`notes/from-marcus/`.
