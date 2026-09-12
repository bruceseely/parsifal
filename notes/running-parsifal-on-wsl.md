# Getting and Running Parsifal under WSL

This guide picks up where [`sbcl-on-wsl.md`](sbcl-on-wsl.md) leaves off: you have
Ubuntu under WSL, SBCL installed, and Quicklisp loading automatically. It covers
downloading the Parsifal repository from GitHub, loading it, parsing a sentence,
and running the test suites — all from a plain shell and a plain SBCL REPL. **No
Emacs required.**

There is a short Common Lisp orientation in Section 8 aimed at someone who knows
MacLISP and Franz Lisp but hasn't used Common Lisp much — mostly about packages,
which is the one thing that will bite you immediately.

---

## 1. Getting the repository from GitHub

The repository is at **https://github.com/bruceseely/parsifal**.

You need a (free) GitHub account, and Bruce needs to have added that account as a
collaborator — the repository is private, so an anonymous download won't work.
Once you're added, GitHub emails you an invitation link; accept it, and then
either method below works.

### Option A — download a ZIP (no `git` needed)

The simplest path. In a browser on Windows, signed in to GitHub:

1. Go to https://github.com/bruceseely/parsifal
2. Click the green **Code** button → **Download ZIP**
3. It lands in your Windows `Downloads` folder as `parsifal-main.zip`

Then, in the Ubuntu terminal, move it onto the Linux filesystem and unpack it:

```bash
sudo apt install unzip -y
mkdir -p ~/repo
cd ~/repo
cp /mnt/c/Users/<YourWindowsName>/Downloads/parsifal-main.zip .
unzip parsifal-main.zip
mv parsifal-main parsifal
rm parsifal-main.zip
```

Substitute your actual Windows user name for `<YourWindowsName>`. (If you're not
sure of it, `ls /mnt/c/Users/` will show you.)

### Option B — clone with `git` (better if you want updates later)

```bash
sudo apt install git -y
```

For a private repository, HTTPS cloning needs authentication. The least painful
way is GitHub's own CLI, which handles the login in a browser:

```bash
sudo apt install gh -y
gh auth login
```

Answer: **GitHub.com** → **HTTPS** → **Yes** (authenticate Git with your GitHub
credentials) → **Login with a web browser**. It prints a one-time code; paste it
into the browser page it points you at.

Then:

```bash
mkdir -p ~/repo
cd ~/repo
gh repo clone bruceseely/parsifal
```

or equivalently `git clone https://github.com/bruceseely/parsifal.git`.

To pick up later changes:

```bash
cd ~/repo/parsifal
git pull
```

### Where to put it

Keep the repository in your **Linux** home directory (`~/repo/parsifal`), not
under `/mnt/c/...`. The Windows filesystem is visible from WSL but noticeably
slower, and Lisp compilation touches a lot of small files. The rest of this guide
assumes `~/repo/parsifal`.

The checkout is about 37 MB, most of it the `.git` history and the scanned PDFs
in `notes/`.

---

## 2. One-time setup

Two things, both quick.

**(a) Make the system visible to ASDF.** ASDF is Common Lisp's build system (the
`defsystem` descendant); it finds a system by scanning a few known directories
for `.asd` files. The conventional place is Quicklisp's `local-projects`, and a
symlink is enough:

```bash
mkdir -p ~/quicklisp/local-projects
ln -s ~/repo/parsifal ~/quicklisp/local-projects/parsifal
```

This is what lets `(ql:quickload :parsifal)` and the test runner work from
anywhere. (The `load-parsifal.lisp` helper in Section 3 doesn't strictly need it
— it registers the directory itself — but the test scripts do, so do it once and
forget it.)

**(b) Let Quicklisp fetch the two dependencies.** Parsifal depends on `cl-lex`
and `cl-yacc`; Quicklisp downloads them automatically the first time you load the
system, so this needs a working network connection once:

```bash
cd ~/repo/parsifal
sbcl --noinform --non-interactive --eval '(ql:quickload :parsifal)'
```

The first run compiles everything and prints a fair amount of output; later runs
load from the compiled cache in `~/.cache/common-lisp/` and are fast. If it ends
without dropping into a debugger, you're set.

---

## 3. Loading Parsifal

Start a REPL from the repository directory:

```bash
cd ~/repo/parsifal
rlwrap sbcl
```

(`rlwrap` gives you command history and line editing — see `sbcl-on-wsl.md` §5.
Plain `sbcl` works too, just less pleasantly.)

Then one form loads everything:

```lisp
(load "load-parsifal.lisp")
```

It prints:

```
;; Parsifal loaded: runtime + full grammar registered.
;; Parse with (in-package :parsifal) then
;;   (parse-sentence "..." :initial-rule (intern "INITIAL-RULE" :parsifal))
;; Re-run (pa::load-full-grammar) if the rule table is cleared.
```

That helper does three things that are easy to forget separately: it loads the
runtime (the ASDF system), it loads the grammar file
`system/grammar/clause-grammar.lisp` (deliberately *not* an ASDF component, since
it's a rule source rather than runtime code), and it registers the whole grammar
into the rule table with `load-full-grammar`.

The rule table is image state. A freshly started SBCL has an empty one, and
`parse-sentence` will complain `no action registered for rule INITIAL-RULE`
until a grammar is registered. If you ever clear it, `(pa::load-full-grammar)`
puts it back without reloading everything.

It also works with an absolute path, so you can load it from any directory:

```lisp
(load "~/repo/parsifal/load-parsifal.lisp")
```

---

## 4. Parsing a sentence

Switch into the `parsifal` package so you can type the function names unqualified
(see Section 8 if that phrase is unfamiliar), then parse:

```lisp
(in-package :parsifal)

(parse-sentence "the boy persuaded the girl to go ."
                :initial-rule (intern "INITIAL-RULE" :parsifal))   ; => T
```

`T` means the parse succeeded; `NIL` means it failed. The resulting tree is left
in the global `c` — the root S node.

Notes on input:

- Lower case throughout, and end with a final punctuation mark: `.` for a
  declarative, `?` for a question.
- A space before the final punctuation (`"... to go ."`) is the convention used
  throughout the test suite; the tokenizer accepts it either way.
- Only words in the dictionary parse. The dictionary lives in
  `system/core/runtime/defs-dictionary.dict` and `supplement.dict`; the sentences
  in `test/integration/` are the reliable guide to what's covered.

### Reading the output

Two printers give you the whole parse at a glance:

```lisp
(stree c)                   ; SURFACE tree: constituents, functions, traces
(ctree (daughter 'vp c))    ; CASE tree: the predicate-argument structure
```

For the sentence above, verbatim output:

```
S1  (DECL MAJOR S)                    PRED: PERSUADE
  np: NP1  (NS N3P DEF DET NP)          SPEC: (PAST V-3S AUX)
    det: the                            AGT via SUBJ:
    nbar: NBAR1  (NS N3P NBAR)            PRED: BOY
      noun: boy                         DAT via OBJ:
  vp: VP1  (VP)                           PRED: GIRL
    verb: persuaded                     NEUT via OBJ:
    np: NP2  (NS N3P DEF DET NP)          PRED: GO
      det: the                              SPEC: (INF AUX) to
      nbar: NBAR2  (NS N3P NBAR)            AGT via SUBJ:
        noun: girl                            trace -> the girl
    np: NP4  (NP COMP-NP NOT-MODIFIABLE)
      s: S2  (SEC COMP-S INF-S S)
        aux: AUX2  (INF AUX)
          to: to
        vp: VP2  (VP)
          verb: go
        np: NP3  (NP TRACE NOT-MODIFIABLE DELTA)  -> the girl
  finalpunc: .
  aux: AUX1  (PAST V-3S AUX)
```

The `ctree` reads: *the boy persuaded the girl to go, and it is the girl who does
the going* — `GO`'s agent is a trace bound to `the girl`, which is object
control. `stree` is on the left, `ctree` on the right; both take an optional
stream argument and default to standard output.

`GLOSSARY.md` in the repository root walks through this same example term by term
and has an API cheatsheet for picking the tree apart by hand (`fe`, `getr`,
`daughters`, the case frame itself).

### More sentences in the same image

The whole grammar is loaded, so you can keep going without reloading:

```lisp
(parse-sentence "is there a meeting scheduled for friday ?"
                :initial-rule (intern "INITIAL-RULE" :parsifal))
(stree c)

(parse-sentence "who did you say scheduled the meeting ?"
                :initial-rule (intern "INITIAL-RULE" :parsifal))
(ctree (daughter 'vp c))
```

That last one prints:

```
PRED: SAY
  SPEC: (PAST VSPL AUX) did
  AGT via SUBJ:
    you
  NEUT via OBJ:
    PRED: SCHEDULE
      SPEC: (PAST V-3S AUX)
      AGT via SUBJ:
        who
      NEUT via OBJ:
        PRED: MEETING
```

### Saving yourself the typing

The `:initial-rule` boilerplate gets old. Define a shorthand once you're in the
package:

```lisp
(defun p (sentence)
  (prog1 (parse-sentence sentence :initial-rule (intern "INITIAL-RULE" :parsifal))
    (stree c)
    (ctree (daughter 'vp c))))

(p "the boy scheduled the meeting .")
```

Put that in a file (say `~/scratch.lisp`) and `(load "~/scratch.lisp")` after
`load-parsifal.lisp` if you want it every session.

---

## 5. Running from a file instead of the REPL

Anything you'd type at the REPL can go in a file. Create `try.lisp`:

```lisp
(load "/home/YOURNAME/repo/parsifal/load-parsifal.lisp")
(in-package :parsifal)
(parse-sentence "who did you say scheduled the meeting ?"
                :initial-rule (intern "INITIAL-RULE" :parsifal))
(stree c)
(ctree (daughter 'vp c))
```

and run it:

```bash
sbcl --noinform --non-interactive --load try.lisp
```

`--non-interactive` means "run it and exit"; `--noinform` suppresses the banner.
This is the batch equivalent of the REPL session in Section 4.

---

## 6. Running the tests

**Unit tests** — the runtime primitives, lexicon, morphology, case frames, and
the two rule parsers. From a REPL after loading:

```lisp
(pa::test-all t)
```

Expected: 14 suites, all `passed`.

```
-=- RULE-LEXER-TEST -=-=-=-= passed
-=- RULE-PARSER-TEST =-=-=-= passed
-=- DECLR-TEST =-=-=-=-=-=-= passed
...
-=- DICTIONARY-TEST -=-=-=-= passed
```

**Integration tests** — one end-to-end sentence parse per construction, checked
down to its case roles. There are 62 of them and they're the real coverage map.
From the shell:

```bash
cd ~/repo/parsifal
test/integration/run-integration.sh
```

Takes roughly half a minute. It launches each test file in its own fresh SBCL
process on purpose — the tests register grammar rules and dictionary entries into
global state, so running them in one image cross-contaminates. It prints a
per-file `pass`/`FAIL` line and exits non-zero if anything failed. (If `sbcl`
isn't on your `PATH`, `SBCL=/path/to/sbcl test/integration/run-integration.sh`.)

Any single test can also be run on its own, which is the most useful way to read
one — each file is heavily commented with the rules that fire and why:

```bash
sbcl --noinform --non-interactive \
     --load test/integration/gram1-object-control-test.lisp
```

It ends with `gram1-object-control-test: passed`.

---

## 7. Where things are

```
parsifal/
├── README.md                       status, coverage table, quickstart
├── GLOSSARY.md                     terminology + worked example + API cheatsheet
├── NOTES-FOR-MARCUS.md             what's faithful, what was adapted, what's deferred
├── load-parsifal.lisp              the one-step loader from Section 3
├── parsifal.asd                    system definition
├── system/
│   ├── core/runtime/               the wait-and-see parser (parse.l/case.l/com.l port)
│   ├── core/rule-processing/       cl-yacc rule-frame parser (cross-check tool)
│   ├── reference/glang-cl/         Pratt port of glang.l, the rule-language compiler
│   └── grammar/clause-grammar.lisp the composable rule groups + load-full-grammar
├── test/
│   ├── *-test.lisp                 unit tests
│   └── integration/                one end-to-end sentence per construction
└── notes/
    ├── from-marcus/                your original sources, verbatim, + INVENTORY.md
    ├── pidgin-grammar-*.text       hand-cleaned OCR of the 1977 appendix
    ├── sbcl-on-wsl.md              installing SBCL under WSL
    └── running-parsifal-on-wsl.md  this file
```

Suggested reading order: `NOTES-FOR-MARCUS.md` first, then whichever
`test/integration/` files cover constructions you care about, then
`system/grammar/clause-grammar.lisp` for the rules themselves.

---

## 8. Common Lisp orientation

If your Lisp is MacLISP and Franz, most of Common Lisp will feel familiar and a
few things will trip you immediately. These are the ones that matter here.

**Packages.** This is the big one. Common Lisp has first-class namespaces called
packages, and every symbol belongs to one. Parsifal's symbols live in the
`PARSIFAL` package (nickname `PA`). From the default `CL-USER` package you must
qualify them:

- `pa:parse-sentence` — a colon reaches an *exported* symbol.
- `pa::load-full-grammar` — a double colon reaches *any* symbol, exported or not.
  When in doubt use `::`; it always works.

Or switch packages once and drop the prefix entirely, which is what the examples
above do:

```lisp
(in-package :parsifal)
```

Your REPL prompt changes to `PA>` (or `PARSIFAL>`) to show where you are. Get
back with `(in-package :cl-user)`.

**Symbols read as upper case.** `(parse-sentence ...)` and `(PARSE-SENTENCE ...)`
name the same symbol, because the reader upcases by default. This is why you see
`(intern "INITIAL-RULE" :parsifal)` with the string in capitals — `intern` takes
the name literally, with no upcasing, so it has to be spelled the way the reader
would have produced it.

**The debugger.** An error drops you into SBCL's debugger, which lists numbered
restarts:

```
debugger invoked on a SIMPLE-ERROR:
  ...
restarts (invokable by number or by possibly-abbreviated name):
  0: [ABORT] Exit debugger, returning to top level.
```

Type `0` then Enter to get back to the REPL. `backtrace` shows the stack.
Ctrl-D also exits the debugger level. It is not an error state you have to
recover the image from — you just return to top level and keep working.

**Leaving.** `(sb-ext:exit)` or Ctrl-D at the top-level prompt.

**Useful at the REPL:**

```lisp
*                          ; the last value printed (** and *** go back further)
(describe 'pa::parse-sentence)
(apropos "PARSE" :parsifal)         ; find symbols by substring
(documentation 'pa::stree 'function)
```

**Loading and compiling.** `(load "foo.lisp")` loads source. ASDF/Quicklisp
compile to `.fasl` files cached under `~/.cache/common-lisp/`, keyed by source
timestamp, so edits are picked up automatically on the next load. If you ever
suspect a stale cache, `rm -rf ~/.cache/common-lisp/` is safe — it just forces a
recompile.

**Things that no longer exist:** `sstatus`, `nointerrupt`, the MacLISP reader
macros, `getchar`/`explodec` and friends as standard functions. Where the port
needed those, it defines them itself in `system/core/runtime/maclisp-chars.lisp`
and `primitives.lisp`, so the ported grammar code still reads the way you wrote
it. `notes/cl-adaptation.md` and the inline comments in the runtime record what
was changed and why.

---

## 9. Troubleshooting

| Symptom | Cause / fix |
|---|---|
| `Component :PARSIFAL not found` | ASDF can't see the repo. Redo the symlink in Section 2(a), or `(load "~/repo/parsifal/load-parsifal.lisp")`, which registers it itself. |
| `no action registered for rule INITIAL-RULE` | No grammar in this image. Run `(pa::load-full-grammar)`, or reload `load-parsifal.lisp`. |
| `Package "QL" does not exist` | Quicklisp isn't loading at startup. Re-run `(ql:add-to-init-file)` from `sbcl-on-wsl.md` §4. |
| Download of `cl-lex` / `yacc` fails | No network from inside WSL. Check with `ping -c1 github.com`; a VPN or corporate proxy on the Windows side is the usual culprit. |
| `parse-sentence` returns `NIL` | The sentence isn't covered — an unknown word, or a construction outside the grammar. Try one from `test/integration/` to confirm the setup is fine, then compare. |
| Odd stale behavior after editing sources | `rm -rf ~/.cache/common-lisp/` and reload. |
| Everything is very slow | The repo is probably under `/mnt/c/`. Move it to `~/repo/`. |
| `run-integration.sh: Permission denied` | `chmod +x test/integration/run-integration.sh` (the ZIP download doesn't preserve the execute bit). |

---

## Summary

| Step | Command |
|---|---|
| Clone | `gh repo clone bruceseely/parsifal` (in `~/repo`) |
| Make visible to ASDF | `ln -s ~/repo/parsifal ~/quicklisp/local-projects/parsifal` |
| Start REPL | `cd ~/repo/parsifal && rlwrap sbcl` |
| Load everything | `(load "load-parsifal.lisp")` |
| Enter the package | `(in-package :parsifal)` |
| Parse | `(parse-sentence "the boy scheduled the meeting ." :initial-rule (intern "INITIAL-RULE" :parsifal))` |
| See the surface tree | `(stree c)` |
| See the case tree | `(ctree (daughter 'vp c))` |
| Unit tests | `(pa::test-all t)` |
| Integration tests | `test/integration/run-integration.sh` |
| Quit | `(sb-ext:exit)` or Ctrl-D |
