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
- Action verbs: `activate`, `deactivate`, `restore`, `run`, `parse`
- Nilfix atoms: `last`, `it`, `current`, `wh-comp`

Two complete `gram4.l` rules now compile end-to-end (`NUMBER` and
`NUMBER-DONE`) — verifies the full pipeline: tokenize → Pratt-parse →
intermediate AST → emit Marcus's compiled-Lisp triple.

**Scope not yet done:** more action verbs (`attach`, `drop`, `label`,
`transfer`, `lift`, `features`, `meet`, `word`, `create`, `new`, `make`,
`insert`, `remove`, `set`, `there`); `if/then/else/andthen`; `and`/`or`
infixm; the bracketing/quoting operators (`(`, `'`); test-pattern
denotations (`fills`, `fits`, `greater`, `less`, `equal`, `lowest`,
`greatest`, `number`, `semantics`, `filling`, `prepositional`); tree-access
infixes (`above`, `of`, `binding`, `register`, `node`, `indirect`); and the
case-rule denotations (`crule`, `upper`, `lower`).

**Per-decision references:**
- Symbol case → uppercase (see *Deliberate deviations* above)
- Tokenizer → custom (see *Deliberate deviations* above)
- `denfun` / `buildfun` → not ported (see *Deliberate deviations* above)
- `associate` → EOF-tolerant (see *Deliberate deviations* above)
- Binding powers → preserved verbatim from Marcus (see *Preserved
  deliberately*)


### `declr.l` — *not yet ported*

Will be a CL package + a small set of `(declaim (special ...))` forms.
Most of `declr.l`'s content is the special-variable list; the macros
(`flags`, `setflags`, `setup**`, `activatenode`, `setfe`, `fe`, `setr`,
`getr`, `clear-current-s`) translate directly. The `(declare (include ...))`
forms drop.


### `macros2.l` — *not yet ported*

Small file (57 lines). `say` and `warn` become CL macros over `format`. The
`meet`, `set-union`, `set-difference`, `set-intersection` macros translate
to CL set operations. `default-arg` translates to `&optional` parameter
defaults.


### `fixes.l` — *not yet ported*

Pure dialect-shim file (13 lines). Most of it disappears in CL — `le`/`ge`
are `<=`/`>=`, `consprop` becomes a small util, the `[` / `]` syntax
adjustments are part of our custom tokenizer rather than the reader.


### `defs.l` — *not yet ported*

Feature ontology. The `redund` declarations define redundant-feature
implications; we'll either translate them into a small `defredund` macro
or a data table consulted at parse time. The big constant lists
(`:nr-types`, `:as-types`, `:specregs`, `:parts-of-speech`,
`:sentence-types`) translate trivially to `defparameter` forms.


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
