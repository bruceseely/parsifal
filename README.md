# Parsifal

A Common Lisp port-in-progress of Mitchell Marcus's PARSIFAL, the deterministic
wait-and-see parser described in *A Theory of Syntactic Recognition for Natural
Language* (MIT Press, 1980).

## About PARSIFAL

PARSIFAL is one of the foundational systems in deterministic natural-language
parsing. Marcus argued, against the dominant view of the late 1970s, that
English could be parsed without backtracking given a bounded look-ahead window
and a deterministic rule-application strategy. Every rule application is
final — no search, no ambiguity-resolution at parse time, no exponential
blow-up. The parser's "wait-and-see" mechanism is built around a small buffer
(typically three positions deep) of upcoming constituents, plus an
attention-shifting stack that lets the parser process sub-phrases as
sub-tasks. The result reads, in Marcus's own informal grammar notation, like
a small declarative language.

PARSIFAL has not been publicly available in a runnable modern form. This
repository preserves Marcus's original source (delivered by him in June 2026)
and ports it to Common Lisp, with the goal of producing a working PARSIFAL on
modern systems while keeping the original code intelligible to future
researchers.

## Provenance

The MacLISP and Franz Lisp sources here came directly from Mitchell Marcus by
email, in three deliveries between 2026-06-03 and 2026-06-05. Marcus's first
reply included blanket permission to reproduce:

> It's delightful that this old work of mine is noticed and of value to you!
> You can reproduce whatever you'd like.

All received material is preserved verbatim in `notes/from-marcus/`; the
canonical received mail bundles (`parse1.orig`, `pfiles.orig`) are kept
alongside the split-out individual files for traceability. See
`notes/from-marcus/INVENTORY.md` for what each file is and what it does.

## Status

In progress, and further along than it sounds: the grammar-language compiler,
the wait-and-see runtime, and a broad slice of the English grammar all run
today. English sentences parse end to end into case-frame-annotated trees. The
main remaining work is grammar *breadth* — more constructions — not new core
machinery. (The whole grammar now loads as one composed unit via
`load-full-grammar`; consolidation is done.)

| Component | Status | Location |
| --- | --- | --- |
| Marcus's original source | Preserved verbatim | `notes/from-marcus/` |
| 1977 grammar (book appendix) | Hand-cleaned OCR; 144 rules parse end-to-end | `notes/pidgin-grammar-rules-1977.text` |
| Rule-frame parser (cl-yacc) | Working; parses both corpora cleanly | `system/core/rule-processing/` |
| Grammar-language Pratt port (`glang-cl`) | Working; 21 action denotations ported; compiles the rule groups the integration suite uses | `system/reference/glang-cl/` |
| Runtime port (`parse.l` + `case.l` + `com.l`) | Working; ~180 functions (buffer, nodes, case frames, the parse loop); drives 47 end-to-end integration parses | `system/core/runtime/` |
| Grammar coverage | Broad: 34 composable rule groups — slices of the 136-rule `gram1`–`gram5` — covering declaratives, the full complement system (infinitive/that-clause, raising, passive, subject- and object-control), wh-questions (subject/object gaps, aux-inversion, preposition stranding, wh-vp PP placement), **long-distance** wh-dependencies, relative clauses (incl. deeply nested), existentials, quantifiers, numbers, and date/time NPs. Most parse as pure composition of these groups; the bulk compose into one grammar via `load-full-grammar`, which parses one sentence from each of the families it covers in a single image. (A few groups with greedy openers — date/time NPs, the bare-temporal-adjunct rule — are kept out of that union and composed only where needed.) | `test/integration/clause-grammar.lisp` |
| CL adaptation summary | Rationale in `NOTES-FOR-MARCUS.md` + inline runtime comments; detailed per-file porting notes still planned | `notes/cl-adaptation.md` |

Each construction is pinned by an end-to-end integration test under
`test/integration/` (47 of them, all passing): a real sentence is tokenized,
parsed deterministically, and checked down to its case roles — e.g. that
*"The boy persuaded the girl to go."* binds the embedded subject to the
object (object control), while *"the boy wants to go ."* binds it to the
subject. The suite works through Marcus's own example sentences, up to deeply
nested cases like *"I gave the boy who you wanted to give the books to three
books."* — where a single NP, *the boy*, is bound across three clauses (the
main-clause dative, a relative pronoun, and, via a stranded preposition deep in
a want-complement, the embedded verb's recipient). `gram1-full-grammar-test`
goes further still: it loads the entire grammar at once and parses one sentence
from every family against that single grammar.

## Repository layout

```
parsifal/
├── parsifal.asd                            ASDF system definition
├── notes/
│   ├── from-marcus/                        Marcus's original source files
│   │   ├── parse1.orig, pfiles.orig        Canonical mail bundles as received
│   │   ├── parse.l, case.l, com.l, ...     Franz Lisp sources (more recent)
│   │   ├── parse.orig, case.orig, ...      MacLISP sources (older)
│   │   ├── glang.l                         Marcus's rule-language parser (Pratt-style)
│   │   ├── gram1.l ... gram5.l             The 1987 grammar
│   │   ├── INVENTORY.md                    Per-file analysis
│   │   ├── MANIFEST.md                     parse1.orig extraction manifest
│   │   ├── PFILES_MANIFEST.md              pfiles.orig extraction manifest
│   │   └── CGOL-Pratt.pdf                  Pratt's CGOL working paper (MIT AI Lab 121, 1976)
│   ├── pidgin-grammar.pdf                  Marcus's 1977 grammar appendix
│   ├── pidgin-grammar-rules-1977.text      Hand-cleaned OCR of the appendix
│   ├── pidgin-grammar-intro.text           Marcus's introduction to the appendix
│   ├── pidgin-grammar-features.text        Marcus's feature index
│   ├── ocr-cleanup-inventory.md            Documented history of the 1977 OCR cleanup
│   └── ocr-scan-report.md                  Final heuristic scan against the cleaned corpus
├── system/
│   ├── core/runtime/                       wait-and-see runtime (parse.l/case.l/com.l port)
│   ├── core/rule-processing/               cl-yacc rule-frame parser
│   └── reference/glang-cl/                 Pratt port of Marcus's glang.l rule compiler
└── test/
    ├── *-test.lisp                         unit tests for the runtime + rule parsers
    └── integration/                        end-to-end sentence parses (one per construction)
```

## Quickstart

Requires SBCL (or another ANSI Common Lisp) and Quicklisp. The dependencies
`cl-lex` and `yacc` are pulled in automatically on first load.

To make the repo discoverable to ASDF, either symlink it under
`~/quicklisp/local-projects/`:

```bash
ln -s /path/to/parsifal ~/quicklisp/local-projects/parsifal
```

or push it onto `asdf:*central-registry*` at the REPL.

### Load and test the runtime

```lisp
(ql:quickload :parsifal)
(pa::test-all t)
```

Expected: all 14 unit suites pass (rule lexer/parser, the runtime's buffer,
node, case-frame and parse-loop primitives, the lexicon, morphology, and the
dictionary).

### Load the `glang-cl` Pratt parser and run its tests

The Pratt port is not yet wrapped as a separate ASDF system; load its files
in order:

```lisp
(dolist (f '("package" "tokens" "pratt" "fixes" "denotations" "compiler"
             "pratt-test" "glang-test"))
  (load (format nil "system/reference/glang-cl/~a.lisp" f)))
(glang-cl::pratt-test)
(glang-cl::glang-test)
```

### Parse an English sentence end to end

Each integration test is a self-contained end-to-end parse: it loads the
runtime and `glang-cl`, registers the rule groups it needs, and parses a real
sentence down to its case roles. Run one directly with SBCL:

```bash
sbcl --noinform --non-interactive \
     --load test/integration/gram1-object-control-test.lisp
```

Expected: every assertion for *"the boy persuaded the girl to go ."* passes,
ending with `gram1-object-control-test: passed`. The other files under
`test/integration/` cover the remaining constructions (relative clauses,
wh-questions, raising, existentials, quantifiers, numbers, …).

To parse against the **whole grammar at once** rather than a curated slice,
load `clause-grammar.lisp` and call `load-full-grammar`, then parse any
supported sentence — the same loaded grammar handles every construction, and
many sentences in one image:

```lisp
(load "test/integration/clause-grammar.lisp")   ; pulls in :parsifal + glang-cl
(in-package :parsifal)
(load-full-grammar)                              ; register the entire grammar once
(parse-sentence "the boy persuaded the girl to go ."
                :initial-rule (intern "INITIAL-RULE" :parsifal))   ; => T
(parse-sentence "is there a meeting ?"
                :initial-rule (intern "INITIAL-RULE" :parsifal))   ; => T
```

`gram1-full-grammar-test.lisp` exercises this across one sentence per family.

### Compile a rule end-to-end

```lisp
(glang-cl:compile-rule "{RULE FOO IN BAR [t] --> Activate cpool.}")
```

returns the compiled-Lisp triple Marcus's `glang.l` would have produced:

```lisp
(PROGN
 'COMPILE
 (RULE-INDEX 'NORMAL '(BAR) 'NOINDEXF
             '(10 |::PAT-OF-FOO| FOO |::ACT-OF-FOO|))
 (DEFUN |::PAT-OF-FOO| () T)
 (DEFUN |::ACT-OF-FOO| () (PROGN (ACTIVATE '(CPOOL))))
 (FEATINDEXIFY NIL))
```

### Cross-validate the two parsers

```lisp
(load "system/reference/glang-cl/cross-validate.lisp")
(glang-cl::cross-validate-file "notes/from-marcus/gram4.l")
```

Current result: on `gram4.l` (the 1987 numbers/time grammar), all 29 rules
agree between the two parsers on kind, name, priority, and packets. On the
1977 corpus, 144 of 146 parseable chunks agree (the other 2 are non-rule
embedded-Lisp blocks).

## Two parsers, one system

PARSIFAL contains **two distinct parsers** at different levels of abstraction:

1. **The grammar-language parser** (Marcus's `glang.l`) reads the
   `{RULE NAME IN PACKETS [...] --> actions}` notation Marcus invented and
   **compiles** it to executable Lisp. This file uses a top-down
   operator-precedence (Pratt) parser adapted from Vaughan Pratt's CGOL — see
   `notes/from-marcus/CGOL-Pratt.pdf`. The CL port lives at
   `system/reference/glang-cl/`.

2. **The English parser** (`parse.l` + `case.l` + `com.l`) takes the *output*
   of step 1 and runs it against English sentences, producing a syntactic tree
   with case-frame structure. This is the wait-and-see parser proper, and it is
   ported and working at `system/core/runtime/` — the buffer, the
   attention-shifting node machinery, the case mechanism, and the
   rule-application loop. The integration suite drives it end to end: a
   sentence in, a checked parse tree out.

This repository also ships a **complementary tool** — a cl-yacc-based
rule-frame parser at `system/core/rule-processing/` — that recognizes just
the rule *frame* (kind, name, priority, packets) using LALR(1) machinery.
It's lighter than `glang-cl`, doesn't try to handle the action language,
and exists to cross-check the Pratt port at the structural level. Agreement
between the two parsers on every rule is our principal correctness signal
at this stage.

## Documentation

The most useful entry points:

- `GLOSSARY.md` — plain-English guide to the terminology (predicate, case roles,
  trace/binding, control, packets, wait-and-see…) plus an API/REPL cheatsheet,
  grounded in one worked parse. Start here if the vocabulary is new.
- `NOTES-FOR-MARCUS.md` — a one-page orientation for a PARSIFAL expert: what is
  faithful, what was adapted and why, and what is deferred (with the blockers,
  several of them Marcus's own annotations).
- `test/integration/` — the working coverage map: one heavily-commented
  end-to-end test per construction, each tracing the rules that fire and why.
  `clause-grammar.lisp` holds the shared, composable rule groups they draw on.
- `notes/from-marcus/INVENTORY.md` — what each of Marcus's files is and does,
  plus what's still missing
- `notes/from-marcus/MANIFEST.md`, `PFILES_MANIFEST.md` — how each file got
  here, with mail headers preserved for provenance
- `notes/ocr-cleanup-inventory.md` — the full history of cleaning the 1977
  OCR'd corpus, including a translation table for each kind of OCR damage
- `notes/cl-adaptation.md` (planned) — minimal-change ruleset for the
  MacLISP → Common Lisp port, plus per-file porting notes

## Acknowledgments

- **Mitchell Marcus** (University of Pennsylvania) for writing PARSIFAL in
  the 1970s, for retrieving the source from 1987-era archives, for
  permitting public reproduction, and for answering questions throughout
  the porting work.
- **Vaughan Pratt** (Stanford) for inventing top-down operator-precedence
  parsing and for CGOL, the formalism Marcus's grammar-language parser is
  adapted from.
- **Kurt Van Lehn**, whose comments on `case.orig` made the case-frame
  mechanism intelligible across the decades.

## References

- Marcus, M. P. (1980). *A Theory of Syntactic Recognition for Natural
  Language.* MIT Press. The 1977 grammar appendix from this book is the
  source of `notes/pidgin-grammar-rules-1977.text`.
- Pratt, V. R. (1976). *CGOL — an Alternative External Representation For
  LISP Users.* MIT AI Lab Working Paper 121. Included verbatim as
  `notes/from-marcus/CGOL-Pratt.pdf`.
- Pratt, V. R. (1973). Top Down Operator Precedence. *Proceedings of the
  ACM Symposium on Principles of Programming Languages.*

## License

To be determined. Marcus has given informal permission for reproduction
(see *Provenance* above); a formal license will be selected in consultation
with him before this repository is widely advertised.
