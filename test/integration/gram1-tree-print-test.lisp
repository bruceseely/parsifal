;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-tree-print-test.lisp
;;;
;;; The TREE PRINTERS (util.lisp): `stree' (surface-structure / constituent
;;; tree) and `ctree' (case tree -- the semantics), ports of Marcus's
;;; util.l `tree'/`short-ctree' with depth-based indentation instead of the
;;; unported cursorpos terminal control. They are debugging aids for
;;; following a parse; this test parses real sentences and checks that the
;;; rendered text contains the structure each printer is supposed to show.
;;;
;;; Two sentences exercise the interesting cases:
;;;   - "the boy persuaded the girl to go ." -- OBJECT CONTROL: stree must
;;;     show the embedded clause and the bound DELTA trace; ctree must show
;;;     PERSUADE over an embedded GO whose AGENT is the trace bound to the
;;;     girl (the girl does the going).
;;;   - "john should have scheduled the meeting ." -- the MODAL+PERFECT aux:
;;;     stree must show the aux with its `modal' (should) and `perf' (have)
;;;     daughters; ctree's SPEC line must surface that aux.
;;;
;;; Assertions match structural fragments (e.g. "PRED: GO", "trace -> the
;;; girl"), not exact whitespace or the generated node ids, so they are
;;; robust to numbering.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-tree-print-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-tree-print-test (&optional verbose)
  (let ((results t))
    (flet ((contains (test-name haystack needle)
             (let ((pass (and (search needle haystack) t)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    not found: ~s~%    in:~%~a~%" needle haystack)))
               (setf results (and pass results)))))

      (load-full-grammar)

      ;; ---- "the boy persuaded the girl to go ." : object control --------
      (parse-sentence "the boy persuaded the girl to go ."
                      :initial-rule (intern "INITIAL-RULE" :parsifal))
      (progn
        (when verbose (format t "~&[object control]~%"))
        (let ((s (with-output-to-string (out) (stree c out))))
          (when verbose (format t "~a~%" s))
          (contains "stree: top S is a major declarative" s "(DECL MAJOR S)")
          (contains "stree: shows the subject NP function"  s "np: ")
          (contains "stree: shows the VP function"          s "vp: ")
          (contains "stree: main verb is persuaded"         s "verb: persuaded")
          (contains "stree: embedded infinitive clause"     s "(SEC COMP-S INF-S S)")
          (contains "stree: embedded verb is go"            s "verb: go")
          (contains "stree: the DELTA subject is a trace"   s "TRACE")
          (contains "stree: the trace is bound to the girl" s "-> the girl"))
        (let ((s (with-output-to-string (out) (ctree (daughter 'vp c) out))))
          (when verbose (format t "~a~%" s))
          (contains "ctree: matrix predicate is PERSUADE"      s "PRED: PERSUADE")
          (contains "ctree: agent filled via the subject"      s "AGT via SUBJ:")
          (contains "ctree: dative filled via an object"       s "DAT via OBJ:")
          (contains "ctree: neutral (the clause) via an object" s "NEUT via OBJ:")
          (contains "ctree: the complement predicate is GO"    s "PRED: GO")
          (contains "ctree: GO's agent is the trace -> the girl" s "trace -> the girl")))

      ;; ---- "john should have scheduled the meeting ." : modal+perfect ----
      (parse-sentence "john should have scheduled the meeting ."
                      :initial-rule (intern "INITIAL-RULE" :parsifal))
      (when verbose (format t "~&[modal + perfect]~%"))
      (let ((s (with-output-to-string (out) (stree c out))))
        (when verbose (format t "~a~%" s))
        (contains "stree: aux is labelled perf+modal" s "(PERF MODAL")
        (contains "stree: aux has a modal daughter `should'" s "modal: should")
        (contains "stree: aux has a perf daughter `have'"    s "perf: have")
        (contains "stree: main verb is the participle scheduled" s "verb: scheduled"))
      (let ((s (with-output-to-string (out) (ctree (daughter 'vp c) out))))
        (when verbose (format t "~a~%" s))
        (contains "ctree: predicate is SCHEDULE"            s "PRED: SCHEDULE")
        (contains "ctree: SPEC surfaces the perf+modal aux" s "SPEC: (PERF MODAL")
        (contains "ctree: neutral object is the meeting"    s "PRED: MEETING")))

    (format t "~&gram1-tree-print-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-tree-print-test t)
