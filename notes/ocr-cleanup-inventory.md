# OCR cleanup inventory: pidgin-grammar-rules-1977.text

This catalogs OCR-introduced damage in `notes/pidgin-grammar-rules-1977.text`
(the 1977 corpus, renamed from `pidgin-grammar-rules.text` once the canonical
1987 grammar arrived as `notes/from-marcus/gram4.l`) as it stood after the
rule-frame parser was first run end-to-end against the corpus. The damage falls into four categories graded by how much
human judgment each fix needs.

The four-category split is the unit of work for cleanup: A and C are safe
to apply mechanically; B should be eyeballed once before applying; D
needs reading the surrounding rule.


## Status

| Date | Action |
| --- | --- |
| 2026-06-03 | Initial inventory built; categories A and C applied to the corpus (~44 edits). Parser coverage: 96 → 105 / 115 rule chunks. |
| 2026-06-03 | Second-pass discoveries folded back in: 4 more corpus fixes (tagged `[pass-2]` below) and 1 lexer enhancement (new category E). Pass-2 corpus fixes applied; lexer enhancement applied to `system/core/rule-processing/rule-lexer.lisp`. Coverage: 105 → 111 / 115. |
| 2026-06-03 | Category B applied (5 substitutions, ~46 edits including 38 bare-`"` → `*`) plus the line-684 fix (`[=num : * Is ...]` → `[=num; *  Is ...]`). Coverage: 111 → 114 / 115. Only structural item S-CREATE (Category D) remains. |
| 2026-06-04 | User hand-fixed Category D items S-CREATE, NUMBER-AGREEMENT, and NAME. Parser tolerance improvements (wrapper skips top-level noise, drops `:semi` in normal mode, tracks `()` depth at top level so embedded Lisp is opaque, and uses depth-aware action/pattern body collection). Grammar extended to accept `*` and `*-X` in CREATION CRULE name/node-type slots (Marcus's `*-DUMMY` rule). **End-to-end full-file parse now succeeds: 144 rules in the 1977 corpus.** |
| 2026-06-04 | Marcus-direct 1987 source split into `notes/from-marcus/*.l` (gram4.l = the grammar Marcus actually ran; glang.l = Pratt-style rule-language parser; plus 9 supporting files). See `notes/from-marcus/INVENTORY.md` and `MANIFEST.md`. Missing parts to ask Marcus for: `parse.l`, `case.l`, `com.l`, `gram1-3.l`. |
| 2026-06-04 | CL port of glang.l started under `system/reference/glang-cl/`. Reaches end-to-end: `{RULE FOO IN BAR [t] --> Activate cpool.}` compiles to Marcus's expected `(progn 'compile (rule-index ...) (defun ::pat-of-FOO ...) (defun ::act-of-FOO ...) (featindexify nil))` triple. Pratt loop + fixity macros + denotations for `rule`, `[`, `.`, `activate`, plus `parse-rule-header` (a frame-only parser used for cross-validation). |
| 2026-06-04 | **Cross-validated the cl-yacc-based rule-frame parser against the glang-cl Pratt-based `parse-rule-header` on both corpora. Result: 100% agreement on every parseable chunk.** gram4.l: 29/29 rules agree on kind/name/priority/packets. 1977 corpus: 144/144 parseable chunks agree (the 2 remaining "both failed" chunks are the `(defun find-WH-comp ...)` embedded-Lisp blocks at top level, which neither parser should treat as rules). 0 field mismatches in either corpus. Cross-validation harness lives at `system/reference/glang-cl/cross-validate.lisp`. |
| 2026-06-04 | OCR scan report at `notes/ocr-scan-report.md` flagged 62 likely artifacts in the 1977 corpus across 7 heuristics. User cleaned them all in one pass. Re-scan: 2 remaining `It` matches, both confirmed as legitimate sentence-initial English (in prose comments inside rules, not action language). **OCR cleanup of the 1977 corpus is effectively complete.** |


## A. Mechanical (no judgment — global substitutions)

These are all single-character or single-word OCR-substitution errors
that can be applied globally with no chance of changing meaning.

| Substitution | Count | Note |
| --- | --: | --- |
| `［` → `[` | 7 | fullwidth left bracket (U+FF3B) |
| `］` → `]` | 8 | fullwidth right bracket (U+FF3D) |
| `--›` → `-->` | 3 | unicode angle-quote in arrow (U+203A) |
| `--）` → `-->` | 1 | fullwidth right paren in arrow (U+FF09) |
| `пр` → `np` | 10 | Cyrillic for "noun phrase", always inside `[...]` |
| `n2р` → `n2p`, `n3р` → `n3p` | 3 | lone Cyrillic `р` in feature-name endings (L946, L949) |
| `‹` → `<` | 1 | unicode angle quote (U+2039) inside a comment |
| `KULE` → `RULE` | 1 | L378 rule keyword |
| `Urop c` → `Drop c` | 1 | L848 action verb |
| `Insless` → `tnsless` | 1 | L85 feature name inside a comment |
| `WP-VD` → `WH-VP` | 1 | L365 packet name in rule header |
| `TO-LESS- INF-COMP` → `TO-LESS-INF-COMP` | 1 | L447 stray space inside packet name `[pass-2]` |
| `JUNE - 1ST` → `JUNE-1ST` | 1 | L818 stray spaces around hyphen in rule name `[pass-2]` |
| `] - >` → `] -->` | 1 | L487 split arrow in SUBJECT-IS-DELTA-DIAG `[pass-2]` |


## B. Pattern-marker fixes (eyeball — semantic)

These change pattern semantics, so worth a single scan before applying.

| Substitution | Count | Note |
| --- | --: | --- |
| `［！］` → `[t]` | 1 | L228 EMBEDDED-VP-DONE; the surrounding priority-15/20 idiom is a catch-all wildcard |
| `[•np]`, `[•prep]` → `[=np]`, `[=prep]` | 2 | L299, L338 — bullet for `=` feature-match marker |
| `ad]` → `adj` | 4 | L459, L465 (in patterns: `[..., en, ad]]`); L554, L579 (in actions: `Activate parse-ad].`) |
| `not-modifiable 1` → `not-modifiable]` | 1 | L658 — stray `1` for `]` closes NP-COMPLETE's pattern |
| bare `"` inside `[...]` → `*` | 38 (actual; eyeball estimate was ~24) | Marcus's `*` (current-buffer-position marker) repeatedly OCR'd as `"`. e.g. L72 `[** c; " Is np-quest]`, L712 `[" Is any of ones, "ten, "10, ...]` |
| `[** c: ...` → `[** c; ...` | 1 | L486 SUBJECT-IS-DELTA-DIAG — colon instead of semicolon as pattern-clause separator `[pass-2]` |
| `[=num : * Is ...]` → `[=num; *  Is ...]` | 1 | L684 NUMBER (AS RULE) — colon instead of semicolon + stray whitespace `[user-requested]` |


## C. Comment-line cleanups (unblock top-level parsing)

These don't change rule semantics but stop the parser from choking on
loose tokens between rules.

| Substitution | Count | Lines |
| --- | --: | --- |
| Leading `::` → `;;` | 3 | L201, L517, L519 |
| Leading `:;` → `;;` | 1 | L580 |
| Leading single `;` + text → `;;` | 3 | L270, L282, L290 |


## D. Structural damage (needs your judgment)

These need a human read of the rule to reconstruct intent. Best-guess
fix listed; please confirm against the original Marcus text if uncertain.

| Rule | Lines | Issue / best guess |
| --- | --- | --- |
| `S-CREATE` (CREATION CRULE) | 277–286 | No closing `}`. Embedded `(defun find-WH-comp …)` paren count is off (4 opens, 6 closes). Best guess: change the trailing `)))` on L286 to `))}` — the extra `)` was the missing `}`. |
| `WH-WITH-END-NEXT` | 292–296 | No pattern brackets, no `-->`. Rule body is a pure conditional ("If ... then run X next else run Y next"). Best guess: insert `[t] -->` on its own line after the `IN wh-vp` header. |
| `WH-RESOLVED-1` | 378 | Line opens with `(KULE` instead of `{RULE` (fixed by A), and ends `--Deactivate wh-pool.}` with the arrow merged into the next word. Best guess: split as `--> Deactivate wh-pool.}` and change opening `(` to `{`. |
| `NP-COMPLETE` (NUMBER AGREEMENT) | 659 | Stray closing `%` with no opener (the lexer used to eat ~120 lines from this stray `%` until the comment regex was defended). Best guess: `NUMBER AGREEMENT%` → `%NUMBER AGREEMENT%`. |
| `NAME` (BUILD-NAME) | 1015 | Rule never closes with `}`. The line `!(cons 1st (the names register of c)). %for pa semantics%)` ends with `)` where `}` was intended. Best guess: change the trailing `)` to `}`. **Important:** the chunker silently stops emitting chunks at L1009 because brace depth never returns to 0, so all ~10 rules from NAME through end-of-file are invisible to chunk-by-chunk validation until this is fixed. `[pass-3]` |
| `PP-UNDER-VP-2` | L1095 (approx) | Packet name is truncated: `IN EMBEDDED-S-[=pp] -->` — the suffix word after `EMBEDDED-S-` is missing. The two existing similar packets in the file are `EMBEDDED-S-VP` and `EMBEDDED-S-FINAL`. **Needs hardcopy verification** since both are plausible. Surfaced by cross-validating the rule-frame parser against the glang-cl Pratt port: 143 / 146 chunks agreed on the four header fields, this one disagreed because of the trailing `-`. `[pass-4]` |
| Leading `S;` on line 1 of file | 1 | The 1977 file currently starts with `S;\n` before the first `;;; Pidgin Grammar...` comment, evidently a stray from an editing session. Doesn't break parsing (parser skips top-level noise) but should be removed. `[pass-4]` |


## E. Lexer enhancements (not corpus edits)

Marcus uses identifier shapes that my original lexer's identifier regex
rejected. These are fixes to `system/core/rule-processing/rule-lexer.lisp`,
not edits to the corpus.

| Enhancement | Drives | Note |
| --- | --- | --- |
| Accept digit-led identifiers of shape `<digits>-?<letter>...` | `99S-ATTACH` (L735, chunk 90), `2-OBJ-INF-COMP` (L426/L472, chunks 48 & 55) | `[pass-2]` Added a new rule between POSITION and NUMBER. Identifiers starting with digits had been splitting into `:number` + `:identifier`, breaking rule names and packet names. |
| Wrapper skips top-level noise (`;` comment lines, stray identifiers between rules) and drops `:semi` in normal mode | Marcus's `;...` comment style in gram4.l; commented-out `;{RULE...;}` blocks | `[pass-3]` `;` is only valid inside collected pattern/action bodies, so dropping it from normal mode is safe and recovers `gram4.l` chunks. |
| Wrapper tracks `()` depth at top level so embedded `{...}` Pidgin expressions inside `(defun ...)` aren't mistaken for rules | `find-WH-comp` in 1977 corpus | `[pass-3]` |
| `collect` is depth-aware so nested `{...}` inside an action body don't terminate collection early | None observed in current corpus, but Marcus's actions could contain embedded Pidgin per his style | `[pass-3]` Prevents premature termination if a rule's action body contains an embedded `{...}' form. |
| Grammar accepts `*` and `*-X` in CREATION CRULE name and node-type slots | `*-DUMMY` (L1127, chunk 144) | `[pass-3]` Marcus's `*' node type appears as a rule-name prefix and as a bare node-type. Hyphen in `*-X' is dropped by the lexer and re-added by the grammar action. |


## Totals

- A: ~40 individual edits across 14 substitutions (+3 from pass 2)  — **applied**
- B: ~47 individual edits across 7 substitutions  — **applied** (38 bare-quote subs were higher than the ~24 estimated)
- C: 7 edits  — **applied**
- D: 5 structural fixes — **applied by user** (S-CREATE, NUMBER-AGREEMENT, NAME, PP-UNDER-VP-2, leading `S;`)
- E: 1 lexer enhancement + 4 parser/wrapper enhancements — **applied**
- Pass-4 (heuristic scan): ~60 edits across `capital-Is`, `digit-O-confusion`, `nonword-fragments` — **applied by user**

End state: **1977 corpus parses end-to-end into 144 rules; 1987 gram4.l
parses end-to-end into 29 rules. Both parsers agree on 100% of
parseable chunks across both files.** All known OCR damage cleaned.
