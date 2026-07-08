;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-too-many-nps-test.lisp
;;;
;;; Regression: the TOO-MANY-NPS guard must FAIL GRACEFULLY, not crash.
;;;
;;; TOO-MANY-NPS (gram2:174, *ditransitive-wh-rules*) is an overflow guard:
;;; when an analysis would put more NPs into a verb than it can hold, its
;;; "loses" branch signals that this wait-and-see analysis is a dead end.
;;; Marcus wrote that branch as `!(warn 1 too-many-nps loses)'. Two porting
;;; problems surfaced when a sentence actually reached it under the WHOLE
;;; grammar (the curated per-construction tests never load this rule):
;;;
;;;   1. glang-cl emits the `!'-escaped warn verbatim, so it became a
;;;      CL:WARN call with `1' as the datum -- a type error (CL:WARN cannot
;;;      take Marcus's numeric severity level). Fixed: the branch now calls
;;;      WARNER (the macro Marcus's own `warn' macro rewrites to), which
;;;      routes to CL:WARN with a real string datum.
;;;   2. the guard consumes nothing, so after merely warning it re-fired
;;;      forever (heap exhaustion). Fixed: "loses" now calls PARSE-LOSES,
;;;      which throws to the CATCH in PARSE-LOOP and returns NIL -- a clean
;;;      parse failure.
;;;
;;; So a sentence that drives the full grammar into this guard now returns
;;; NIL (a normal "no parse") instead of erroring. This test pins that down:
;;; the once-crashing sentence parses without signaling a condition, and an
;;; ordinary sentence still succeeds.
;;;
;;; (Separately, "Is there a meeting scheduled for friday?" DOES parse under
;;; the curated existential rule set -- see gram1-existential-passive-test;
;;; that it derails into TOO-MANY-NPS under the full grammar is a known
;;; rule-interaction limitation, distinct from the crash fixed here.)
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-too-many-nps-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-too-many-nps-test (&optional verbose)
  (let ((results t))
    (flet ((check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%"
                           expected actual)))
               (setf results (and pass results))))
           ;; Parse with warnings muffled; return :ERROR if a condition is
           ;; signalled, otherwise the parse result (T / NIL).
           (safe-parse (s)
             (handler-case
                 (handler-bind ((warning #'muffle-warning))
                   (parse-sentence s :initial-rule (intern "INITIAL-RULE" :parsifal)))
               (error () :error))))

      (load-full-grammar)

      ;; The once-crashing sentence: must come back without erroring.
      (let ((r (safe-parse "is there a meeting scheduled for friday ?")))
        (check "TOO-MANY-NPS sentence does not crash (returns T or NIL, not :ERROR)"
               (and (member r '(t nil)) t) t)
        (when verbose (format t "    (result was ~s)~%" r)))

      ;; The user's exact spacing/case, too.
      (check "user's exact string does not crash"
             (and (member (safe-parse "Is there a meeting scheduled for friday?")
                          '(t nil))
                  t)
             t)

      ;; A normal sentence still parses under the same full grammar.
      (check "an ordinary sentence still succeeds"
             (safe-parse "the boy meets the boy .") t))

    (format t "~&gram1-too-many-nps-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-too-many-nps-test t)
