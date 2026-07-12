#!/usr/bin/env bash
#
# run-integration.sh -- run the parsifal grammar integration tests
# (test/integration/*-test.lisp), EACH IN ITS OWN fresh SBCL process.
#
# These files are standalone self-running scripts: loading one defines its
# <basename> test function and immediately runs it, printing a summary line
# "<name>: passed" or "<name>: FAILED <<<". They mutate global grammar,
# dictionary, and parser state, so batching them in one image cross-contaminates
# -- a later test sees a dirty grammar and fails though it passes alone (verified
# 2026-07-11: 37/52 in one image vs. all green in isolation). Hence one process
# per file. Pass/fail is read from the printed summary marker.
#
# Usage:  test/integration/run-integration.sh
# Env:    SBCL=/path/to/sbcl   (default: sbcl on PATH)
# Exit:   0 iff every integration file passed; 1 otherwise.

set -u
SBCL="${SBCL:-sbcl}"
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

pass=0
fail=0
for f in "$DIR"/*-test.lisp; do
  name="$(basename "$f" .lisp)"
  out="$("$SBCL" --non-interactive --disable-debugger \
          --eval '(ql:quickload :parsifal :silent t)' \
          --eval "(handler-bind ((warning (function muffle-warning))) (load \"$f\"))" \
          2>&1)"
  # A run passes iff it printed its "…: passed" summary and no "FAILED <<<"
  # (a crash prints neither -> treated as failure).
  if printf '%s\n' "$out" | grep -q "FAILED <<<" \
     || ! printf '%s\n' "$out" | grep -q ": passed"; then
    fail=$((fail + 1))
    printf "  FAIL <<<  %s\n" "$name"
  else
    pass=$((pass + 1))
    printf "  pass  %s\n" "$name"
  fi
done

total=$((pass + fail))
echo "  ${pass}/${total} passed"
[ "$fail" -eq 0 ]
