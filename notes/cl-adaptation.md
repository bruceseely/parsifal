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
- Surface-structure crules: the `creation` / `attachment` prefixes feed
  `build-crule`, and `compile-crule-form` emits Marcus's `crule' macro
  output (glang.l 601-610) — `(progn 'compile (crule-index 'TYPE
  '(NODE-SPEC ::crule-of-NAME NAME)) (defun ::crule-of-NAME () BODY))`,
  with NODE-SPEC the lone node-type (creation) or `(upper . lower)`
  (attachment). This is exactly the shape the runtime `crule-index`
  files (case-frame.lisp), wiring `{CREATION CRULE}` / `{ATTACHMENT
  CRULE}` grammar rules to the create/attach monitors.
- Crule-body verbs (glang.l 344-346, 502-508, 544): `upper` / `lower`
  (the attachment node references, emitting the runtime specials
  `fnode` / `snode` that ATTACH-MONITOR binds); `associate` (→
  `associate-cf`), `fills` (→ `fillslot`, with Marcus's optional
  `[ of cf of ]`), `case frame of X` (→ `(getr 'caseframe X)`),
  `finalize` (→ `finalize-frame`), and `indirect object of X` (→
  `(io X)`).
- The `isn` infix (glang.l 447): `it isn't true that X` → `(not X)`
  (the left `it` is discarded; `isn't` tokenizes as `isn` + the
  apostrophe single-char symbol + `t`). bp 6, below `and`/`or`.
- The `!` read-macro (glang.l 72-79), handled in the tokenizer: `!`
  escapes to Lisp syntax and reads one form as a single literal token,
  so `!'(inf-comp)` becomes the datum `(quote (inf-comp))` flowing
  through as a self-evaluating operand.

  With these (plus the already-ported `new`, `set`, `binding`,
  `current`, `of`, `if/then/else`, `there is`, and the self-evaluating
  atoms `c` / `1st`), **every active crule in gram1-5 now compiles
  end-to-end** — VP-VERB, PRED-PP, VP-NP, S-WHCOMP, S-CREATE, NP-S,
  NP-START, NP-ADJ, NP-NBAR, NBAR-PP, NP-PP, S-PREDP, S-PP, VP-PP, and
  the `*-DUMMY` creation rule. (NP-QP is commented out in gram3.l.)
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

- Gate 3 (gram2/gram3 `{RULE}` denotations): `word` (`the word 'X'` →
  `(wordify 'X)`); the numeric comparisons `greater`/`less`/`equal`
  (reached through `is`); `greatest`/`lowest` (`... possible number of
  objects of X` → `(maxunls X)`/`(minunls X)`); `fits` (case-frame
  sibling of `fills`); `meet` (`the meet of A and B` → CL
  `(intersection A B)`); `number` (`the number of objects of X will be
  Y` → `(need-slots X Y)`); `filling` (`X filling SLOT slot of Y` →
  `(fit-of X 'SLOT Y)`); `semantics` (`semantics prefers A <degree>
  better than C` → `(prefer A <degree> C)`); and `prepositional` (`a
  prepositional phrase of P and N` → `(pgof P N)`). 58 of the 60 active
  gram2/gram3 rules now compile.

**Scope not yet done:** remaining action verbs (`lift`, `make`); the
standalone matching-quote operator `'`; the rest of the test-pattern
denotations. One gram3 rule (NP-COMPLETE) needs `transfer` to accept an
*expression* argument (`transfer the meet of (...) ... from X to Y`) --
the port's `transfer` currently reads only a literal comma feature-list
(`get-var-list`); Marcus parses `(qlistfy right)`. Extending it (without
breaking the literal-list case) is the next gate-3 step. The
crule path is complete: the frame (CREATION / ATTACHMENT) and all the
crule-body constructs the grammar's case rules use now compile (see
*Scope completed*); what remains are pattern/test denotations used by
ordinary `{RULE ...}`s, not by crules.

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
  (enough to actually run a grammar):
  - Rule indexing (feature-indexed): `*rule-index*`, `rule-index`,
    `rem-index`, `fetchrules`, `rules-packets`, `testrules`,
    `act-of-rule`, `reset-rule-table`
  - Buffer GC: `buffer-gc` (wired into the loop's `nextrule`)
  - The loop itself: `parse-loop` (preserves Marcus's `PROG` + `GO`
    structure with `runrule` and `nextrule` labels)

MVP deviations in `parse-loop.lisp` worth knowing:

- **Rule storage (now feature-indexed).** Marcus indexes rules by
  feature so `fetchrules` can prune the candidate set via the buffer
  node's feature list before any pattern runs. The port now does this
  too: `*rule-index*` is a hash-table keyed by `(indexf type packet)`,
  and `fetchrules` collects only the buckets whose `indexf` is among
  the node's features (plus the catch-all `noindexf`). `testrules`
  then stable-sorts the merged buckets by priority. Marcus keeps the
  buckets on each feature symbol's plist; we keep them in one
  reboundable hash-table so tests can isolate a rule set and the
  global feature symbols' plists stay clean (same spirit as the
  gensym `daughters` plist and the `:act-fn` stash). The glang-cl
  emission contract is unchanged — it already passes `indexf` to
  `rule-index`. Marcus additionally restricts AS rules to
  `(cpool npool)` and NR rules to `(cpool)`; we don't (those packet
  names live in the grammar's package, and AS/NR rules only ever live
  in those packets anyway, so filtering by active packet yields the
  same set). Revisit when NR dispatch from `set*` lands.
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


### `defs.l` — *ported (feature ontology + lexicon)*

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

**The lexicon (lines 85-876):** ~540 verb / noun / adjective
definitions via `(df ...)`, `(df1 ...)`, `(df+ ...)`, `(jlike ...)`,
`(abbrev ...)`, and `(irreg ...)`. The definers already live in
`lexicon.lisp`; this is just their data. Because it is Marcus's source
verbatim, it is kept as a data file, `defs-dictionary.dict`, and read by
`dictionary.lisp`'s `load-dictionary` (the last runtime component, run
at system-load time) rather than transcribed into a CL component.

Two MacLISP reader behaviours the dictionary relies on are supplied by
`*dictionary-readtable*` (installed only while reading the .dict file):

- **`#` as a constituent.** In the lexicon `#` is the marker-set
  separator (the "ok `#` great" split `smqval` reads) — i.e. the symbol
  `|#|`. CL makes `#` a dispatching macro char, so we give it
  constituent syntax; it then reads as `|#|`, `EQ` to the `|#|` the
  scorer splits on.
- **bare `:` as the symbol `|:|`.** A lone colon appears as a
  punctuation "word" (`(jlike - :)`, defs.l 756/759); CL's token parser
  rejects it (package marker), so a reader macro returns `|:|`. Escaped
  colons (`\:`, e.g. `(jlike \: \,)`) and everything else are unaffected
  — the lexicon has no package-qualified symbols. The MacLISP backslash
  escapes (`wh\-`, `jan\.`, `det\\relpron-ambig`) already read
  identically under CL's single-escape.

The `comment` macro (a no-op swallowing its body, used twice in the
data) is defined in `dictionary.lisp`.

**Six dropped forms.** The .dict file is verbatim defs.l 85-876 *except*
for six terminal-noise forms at the tail (source lines 830, 832-836):
captured `(PARSE)` parser-prompt echo and tokens corrupted by literal
backspace / 0x1E control characters (`meetio<BS>ong`,
`|MMM...(PARSE)_|`, `\<0x1E>`). These are session detritus that crept
into the saved source, not lexicon entries, and CL's reader can't read
the control characters anyway. Documented in the .dict header.

Tests in `test/dictionary-test.lisp`. Because the lexicon is global
mutable state that other suites (e.g. morpho-test) destructively
redefine, `dictionary-test` reloads the dictionary first so it is
order-independent.


### `parse.l` — *largely ported; see the detailed section above*

The English parser is split across `primitives.lisp`, `buffer-ops.lisp`,
`node-ops.lisp`, and `parse-loop.lisp` (full breakdown in the "parse.l —
partial port" section above). The bottom-up porting order is essentially
complete:

1. Data-structure accessors (`flags`/`setflags`/`fe`/`setfe`/`getr`/`setr`) — **done** (declr.lisp)
2. Buffer operations (`insert-index-pos`/`remove-index-pos`/`nextword`/`last*`/`buffer-gc`) — **done**
3. Feature operations (`addf1`/`remf1`/`is`/`is-any-of`/`is-none-of`/`transfer`/`liftr`/`featindexify`/`testindices`) — **done**
4. Tree navigation (`node-above`/`father-node`/`find-node`/`binding`) — **done**
5. Node creation/attachment (`newnode`/`makenode`/`attach`/`attach1`/`drop`/`alt-attach`) — **done**
6. Rule indexing (`rule-index`/`rem-index`/`fetchrules`/`testrules`, feature-indexed) — **done**
7. The main loop (`parse-loop`, with `buffer-gc` wired in) — **done**

Still deferred (each blocked on another file or a non-exercised path):
the full `parse` driver (needs `sentin`, com.l); the morphology/tree-print
accessors `head`/`word`/`root-of`/`nid`/`node-id` (need `phrasify`, util.l,
and the lexicon); node cleanup `nodegc`/`node-reset` (uninterning + `cat`);
`alt-fillslot` (needs `putc`, case.l); AS/NR dispatch from `set*` (the
bit-2 NR-checked bookkeeping); and debug/timing (`ruletrap`/`breaksw`/
`starttime`/`endtime`).


### `case.l` — *fully ported (core in case-frame.lisp; oracle in util.lisp)*

Case-frame mechanism (Marcus's Appendix E): assigns a clause's NPs/PPs
to a verb's thematic slots by semantic-marker scoring; consumes the
`case-frame` data the lexicon builds (`expandcf`). Kurt Van Lehn's
header (case.l 4-46) documents the data type: a frame is a gensym with
props `case-frame` (NORMAL/MOD), `assoc-node`, `spec`, `cases`, `pred`,
`hypo-slots`, `objs-needed`. Self-contained -- `semcall` (the "call to
semantics") is reduced to an optional trace in the delivered source, and
the scoring is pure marker arithmetic. Being ported in four increments.

**Increment 1 -- data type + access (done).**
`system/core/runtime/case-frame.lisp` (loaded last): `newcf`,
`case-frame`, `associate-cf`, `assoc-node`, `open-obj-cases`,
`maxunls`/`minunls`, the open/close caching (`openchek`, `closeframe`,
`openframe`, `clearcf`), `putc`/`getc`, the trace hook (`semcall`,
`node-w-feats`, `nodep`), and the now-unblocked `alt-fillslot`
(parse.l 586, which needed `putc`). The cached specials `hypo-slots` /
`objs-needed` are added here (`openframe`/`pred` were already in declr).

Also lands the deferred phrase-structure accessors in `node-ops.lisp`:
`head`/`word`/`root-of` (parse.l 357-378), which case.l's `smarkers`
needs. `nead` (case.l 429) is just a typo for `head`.

Notes: `openframe` is both a special var (the open frame) and a function
(open a frame) -- CL allows both on one symbol, as Marcus relies on.
`putc` (case.l 537) guards on the node's `caseframe` but writes its
`case-frame` prop while `getc` reads `caseframe`; reproduced verbatim
(they don't round-trip, but `putc`'s only caller stores data nothing
reads and `getc` is unused). Tests in `test/case-frame-test.lisp`.

**Increment 2 -- surface-structure monitors (done).**
`crule-index`, `create-monitor`, `attach-monitor` (case.l 574-598), now
the real implementations in `case-frame.lisp`; the `buffer-ops.lisp`
no-op stubs are removed (the functions are forward-referenced from
`attach`/`newnode`, which load earlier). A `create` crule runs when a
node of its type is created (via `newnode`); an `attachment` crule runs
when a node is attached under a father of its type (via `attach`), with
`fnode`/`snode` bound for the crule body.

Storage mirrors Marcus but is reboundable for tests (cf. `*rule-index*`):
creation crules in the list `*create-rules*`, attachment crules in the
hash `*attach-rules*` keyed by father-node type (Marcus keeps them on
the `:create-rules` variable and the `:attach-rules` symbol's plist).
`crule-index` for `attachment` rewrites its `((father . attach) fn name)`
argument in place to `(attach fn name)`, filed under the father type,
exactly as case.l 574 does. With no crules registered the monitors are
no-ops, so gram4 / parse-string stay green.

**NB -- glang-cl gap:** glang-cl does not yet compile the
`{CREATE ...}` / `{ATTACHMENT CRULE ...}` grammar rule forms into
`crule-index` calls; that is a separate glang-cl extension. The runtime
monitor mechanism is exercised in tests by registering crules directly.

**Increment 3 -- scoring + hypothesis generation (done).**
The semantic-marker scoring (`smqval`, `smqchek`, `smarkers`,
`maxsmqval`, `fit-of`/`fit-of-1`) and the hypothesis generators
(`cases`, `subjcases`/`objcases`/`ppcases`, `subjcasegen`/`objcasegen`/
`ppcasegen`, `consolidate-frame`, `filter-out-filled`, `bind-slots`),
case.l 268-440. A case's marker requirement is a list split by `#` into
an "ok" set (score 0) and a "great" set (score 1); a node scores -2 if
nothing matches. The `#` marker is written `'|#|` and Marcus's `#sw`
flag is renamed `great-sw`. Several generator locals shadow function
names in the source (`head`, `cases`, `ppcases`); renamed to
`hd`/`filled-cases`/`prep-cases`.

**`super-smqval`/`super-fit-of`/`super-fit1-of`** (case.l 448-477) -- the
*interactive* "smart semantics" oracle (they `read` a 0/1/2 rating from
the terminal). Not needed for marker-based parsing; ported later in the
util.l increment (they need `phrasify`/`cfprint`). See the util.l
Increment 3 note.

**Increment 4 -- major operations (done).**
The grammar-facing case-frame operations (case.l 99-252, 514-527):
`need-slots`/`set-objs-needed`, `fits`/`fits*`, `fillslot` and its
helpers `fillcase`/`fillmod`/`fillpred`/`fillspec`, `finalize-frame`,
`passivize-cf`/`passivize-cf1`, plus `prefer`/`domf`/`dom-cf`. A clause's
case-frame lifecycle now runs end to end at the unit level: `fillpred`
seeds `hypo-slots` from the verb's lexical case-frame, `fits`/`fillslot`
fit the subject and object NPs into their cases, and `finalize-frame`
keeps the hypotheses with all obligatory cases filled (verified in
`case-frame-test`).

Stubs / defers: `pp-cf-check` (referenced by `fillslot` but undefined in
any delivered file) and `dp1` (util.l case-frame display, only reached
under `ctrace`) are no-op stubs.

**Increment 5 -- final aux + interactive oracle (done).**
`pgof` (case.l 565, the prep+NP denotation — its daughters go in a
gensym holder to match our `attach` representation) and `real-caseset`
(case.l 528, genitive case generation; its `poss-pg-cases` is undelivered
and stubbed to NIL, so the genitive branch is dormant) landed as
increments 5a/5b. The `super-*` interactive oracle (5c) was deferred to
util.l, where its `phrasify`/`cfprint` dependencies live — now ported
there (see the util.l Increment 3 note).

With this, **case.l is fully ported.** The remaining work before a full
English sentence parses is independent of case.l: loading the real
`defs.l` dictionary (a `#`-constituent readtable), teaching glang-cl to
compile the `{CREATE}` / `{ATTACHMENT CRULE}` rule forms into
`crule-index` calls (so grammar rules actually drive the monitors), and
porting the NP/clause rules in gram2.l/gram3.l.


### `com.l` — *ported (non-interactive core; TTY/define deferred)*

REPL, sentence input (`sentin`), morphology (`morpho`), word-tree, and
the lexicon definers (`df`/`df+`/`jlike`/...). Ported in four increments
(below). The interactive TTY reader, `define?`, node-uninterning
cleanup, and the case-frame *modification* path (`modcasef`/`:caseorder`)
are deferred.

**Increment 1 — MacLISP char/symbol compat layer (done).**
`system/core/runtime/maclisp-chars.lisp`. com.l and `morpho` take words
apart and rebuild them through MacLISP's symbol/character primitives,
which live in the Lisp / `myutil`, not com.l. Ported faithfully:

- `explodec` (object → list of one-char symbols), `exploden` (→ char
  codes), `implode` (char-symbols/codes/strings → interned symbol),
  `maknam` (→ uninterned symbol), `readlist` (read one form from the
  concatenated printnames), `ascii` (code → char-symbol), `getchar`
  (Nth char), `flatc` (printname length).
- Char-symbols are interned in :parsifal (not gensymed) so they compare
  `eq` against the char-class lists `morpho` will use (`*vowels*` etc.),
  mirroring Marcus's obarray-interned characters.
- `readlist` concatenates printnames with **no** separators (this is
  what `df+` relies on to read a phrase into a single symbol) and binds
  `*read-base*` to 10 (Marcus wraps the numeric uses in
  `(for (ibase 10.) ...)`). Tests in `test/maclisp-chars-test.lisp`.

**Increment 2 — lexicon loader (done).**
`system/core/runtime/lexicon.lisp` (loaded after `defs`). The
data-building half of com.l:

- **Word-tree**: `*wstring-tree*`/`*wstring-list*`, `get-string`,
  `put-string`, `reset-lexicon`. Each tree node is a gensym whose plist
  maps a char-symbol to its subtree and a `:tree-word` key to the word
  ending there — the same "gensym for a disembodied cons plist" trick
  `attach` uses for `daughters`.
- **Definers** (MacLISP `fexpr`s → CL macros that quote their literal
  arg list and call a worker): `df`, `df1` (a no-op — Marcus disables a
  `df` by writing `df1`), `df+` (multi-token phrases), `jlike`,
  `abbrev`.
- **Word building**: `buildword`, `buildnumber`, `buildirregword`,
  `mod-features` (Marcus's `mod`, renamed — the MacLISP name shadows
  `cl:mod`).
- **Expansion**: `expandsim`, `expanddef`, `expandm`, `add-redunds`,
  `expandcf`, plus `modcasef`/`after`.

Deviations: `add-redunds` reads the `*redund-table*` defs.lisp already
populates (com.l's duplicate `redund` fexpr, which used a `:redunds`
plist, is dropped). `lowcaseify` folds to **upper**, not lower, so
symbols rebuilt from raw characters stay `eq` to the ones CL's reader
interns from the lexicon source (the runtime is uppercase-canonical).
`modcasef`/`after` are ported but `*caseorder*` is NIL until case.l/defs
set it, so the rare `cases`-override path orders added cases at the end.
Tests in `test/lexicon-test.lisp`.

**Increment 3 — morphology (done).**
`system/core/runtime/morpho.lisp` (loaded after `lexicon`). `morpho`
plus `sta`/`ends-in`/`strip-if`/`strip-if-any`/`modfe`/`try`/`origcase`
and the char-class globals (`*vowels*`/`*consos*`/`*liquids*`/`*noend*`/
`*endpuncs*`/`*puncs*`; `*numbers*` is in `lexicon.lisp`). It strips
inflections (-s, -ed, -ing, -er, -est, -ly, ordinals, contractions) and
undoes English spelling changes (consonant doubling, final -e, y/i) to
reconstruct a root it looks up via `expandsim`.

- **Reversed character lists.** `morpho`'s input is a list of character
  *codes in reverse order*, exactly as Marcus's `sentin` collects a
  word; `*rt*`/`*word*` stay reversed and `ends-in`/`sta` match suffixes
  against the front of the reversed list.
- **Three faithful reproductions of likely source errata**, flagged
  inline: `(member (cdddr *rt*) *vowels*)` (a tail never `eq` a vowel →
  always falls to `adde`), `try`'s `(get 'features x)` (looks transposed
  from `(get x 'features)`), and `strip-if-any` never testing its last
  suffix. Preserved as-is; revisit if Increment 4 needs them.

Verified end-to-end in `test/morpho-test.lisp`: `cats`→`cat`+npl,
`running`→`run`+ing (consonant doubling), `walked`→`walk`+past,
`faster`→`fast`+comp, `quickly`→`quick`+adv, `don't`→`do`, `5th`→ord.

**Increment 4 — sentence input + parse driver (done).**
`system/core/runtime/input.lisp` (loaded after `morpho`):

- `nodify`/`nodify*`/`wordify` (com.l 281-310) -- canonical word →
  buffer node, collapsing multi-token phrases via the word-tree.
- `tokenize` + `read-sentence` -- the non-interactive replacement for
  com.l's TTY `sentin`: split a string (peeling trailing punctuation
  into its own tokens), `morpho` each token, then `nodify*`. Sets
  `*wstring*`.
- `reset-parser-state` + `parse-sentence` -- the startup half of
  Marcus's `parse` (parse.l 103) with `(sentin)` replaced by
  `read-sentence`, then `parse-loop`.

Deviations / notes:

- **No TTY.** The character-level line editor (rubout, `$c`/`$s`,
  cursor control) and the `*nextmorph*` re-feed are replaced by a
  whole-string tokenizer that pre-splits punctuation.
- **Naming collisions with glang-cl.** glang-cl `:use`s `:parsifal` and
  defines its own `tokenize` and `parse-string`; exporting those names
  let glang-cl's definitions clobber the runtime's (a 1-arg
  `parse-string` shadowing the driver). So `tokenize` stays internal
  and the driver is exported as **`parse-sentence`**.
- `nodify` faithfully initialises a fresh node's `daughters` register
  to the symbol `word` (`(list 'daughters 'word ...)`); inert for leaf
  word-nodes, flagged in case it matters.

End-to-end pipeline verified in `test/integration/parse-string-test.lisp`:
the string `"it ."` flows reader → `parse-sentence` → `parse-loop` →
glang-cl-compiled `INITIAL-RULE`/`NP-UTT`, building an S with the NP and
final punctuation attached.

**Still deferred (need other files):** the interactive `sentin`,
`define?`, `say-sent`, node-uninterning cleanup (`remnodes`/`remnode`);
loading the real `defs.l` dictionary (lines 85-876) needs a readtable
that treats MacLISP `#` markers as constituents (CL's reader rejects a
bare `#`); and full gram1.l declarative-sentence parsing needs the
case-frame machinery (`case.l`) plus the NP/clause rules in
gram2.l/gram3.l.


### `util.l` — *port started (foundational utilities)*

Parser-interface / display utilities. Header comment says "none of which
are necessary to run the parser" — accurate. Tree-printing, REPL plumbing,
tracing, and the terminal-control "movie" machinery (tied to MacLISP
`cursorpos` / split-screen, which have no modern equivalent). Being ported
in increments, bottom-up, taking the pieces the rest of the port actually
needs rather than the whole interactive shell.

**Increment 1 — symbol synthesis `concat` / `cat` (done).**
`system/core/runtime/util.lisp` (loaded last). Marcus defines these in
`macros2.l` (lines 50, 55), but their consumers live in util.l, so the
`macros2.lisp` port deferred them here. `concat` builds an interned
symbol from its arguments' concatenated printnames; `cat` is the macro
shorthand that rewrites to a `concat` call. Used to synthesize rule /
denotation names (`:act-of-`, `:nud-`, …) and the date/dump strings.

Note: unlike `implode` (`maclisp-chars.lisp`), which reads an integer
argument as a character *code*, every `concat` argument contributes its
`princ` representation — so the number 5 contributes the digit "5", not
character 5. Interned in `:parsifal`, matching the port's single-obarray
convention (cf. `implode` / `readlist`). A *string* argument keeps its
case verbatim while a *symbol*'s printname is upcased by the reader; in
practice `concat` is called with symbols/numbers. Tests in
`test/util-test.lisp`.

**Increment 2 — phrase display (done).**
`collectw`, `phrasify`, `prphrase`, `nodes-to-words`, `cfprint` (util.l
545-575), plus `print2` (the undelivered Franz no-slashification printer
= `princ`). This is the surface-phrase layer: gather the words a node
spans, in input order, restore their original case, and print them. It is
the prerequisite for case.l's interactive supervisor (which prints the
phrase it asks the operator to grade).

Two adaptations: (1) `collectw` recurses over a node's daughters; Marcus
walks `(cdr (getr 'daughters node))` of his disembodied-plist daughters
register, but the port's `daughters` register is a gensym whose plist
carries the type->kids mappings (see `attach`), so the equivalent is
`(symbol-plist (getr 'daughters node))` — which *is* that `cdr` (his list
has a leading NIL header the gensym's plist lacks). Leaf word-nodes are
found by their `word' register and never reach this branch. (2) `phrasify`
uses Marcus's `sortcar` (sort pairs by CAR); rendered as `CL:SORT` keyed
on `car`. `cfprint` prints the cases of the global `certain-frame` (not of
its argument's frame) — reproduced verbatim; and its output spacing
differs slightly because the port's `say-it` space-separates items where
Marcus's `print2` concatenated them.

**Increment 3 — interactive supervisor (done).**
`super-smqval` / `super-fit1-of` / `super-fit-of` (case.l 448-477) — the
hand-scoring "supervisor" path deferred out of case.l because it needs
the phrase-display layer above. `super-smqval` shows the operator a
phrase and frame and reads a 0/1/2 grade, mapping it to the SMQVAL scale
−2/0/1; the others drive it over a caseset's fit-possibilities. They live
in util.lisp (not case-frame.lisp) because they call `phrasify`/`cfprint`,
and util.lisp loads later. Nothing in the parser calls them — the
automatic SMQVAL path is what runs.

Adaptations: Marcus's `prog` + `huh?` retry loop becomes a `LOOP`;
`lessp`→`<`, `if*`→`when`, `ifnot`→`unless`. `super-fit-of` is a MacLISP
fexpr that evaluates its node and caseset arguments and *also* evaluates
the last element of the caseset *form* as the frame node
(`(eval (car (last (cadr l))))`) — reproduced as a macro that threads
`(car (last caseset-form))`. Two undelivered dependencies: `cursorpos`
(the terminal cursor primitive; the trio uses only `(cursorpos 'c)`,
ported as a no-op stub) and `fitspg1` (the fit-possibility generator;
stubbed to *signal* so the dormancy is explicit rather than failing
obscurely in `super-fit1-of`'s `(apply #'max ())`).

Still deferred to a later util.l increment: `default-arg` (a port-time
`&optional` translation, ports with its sole caller `ptree`); the tree
printers (`tree`, `stree`, `ptree`, `short-ctree`, which also need
`cursorpos`'s positioning forms); and the full `say-it1`/`say1` with
`$$`-splice and `$$up` cursor control. The REPL/top-level loop and the
terminal "movie" code will likely port last, if at all.


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
