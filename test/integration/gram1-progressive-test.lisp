;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-progressive-test.lisp
;;;
;;; The PROGRESSIVE aux (gram1:115), verbatim Marcus, newly registered here:
;;;
;;;     {RULE PROGRESSIVE IN BUILD-AUX
;;;      [=*be] [=ing] --> Attach 1st to c as prog.  Label c prog.}
;;;
;;; "The girl is eating pie ." -- the be+participle verb group. STARTAUX makes
;;; the aux and copies the tense off `is'; PROGRESSIVE then consumes `is' into
;;; the aux as its `prog' daughter and labels the aux prog, leaving `eating' in
;;; the buffer so MAIN-VERB takes the genuine main verb.
;;;
;;; The rule group was MISSING from this port until 2026-09-04, and its absence
;;; is why no progressive sentence parsed: with no BUILD-AUX rule matching
;;; [*be][ing], AUX-COMPLETE (priority 15) dropped the aux, AUX-ATTACH attached
;;; it, and MAIN-VERB then took `is' ITSELF as the verb -- stranding `eating'
;;; with no rule that accepts it, three words from the end. The last check below
;;; is that regression guard: the SAME sentence, same grammar minus
;;; *progressive-rules*, must fail.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-progressive-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-progressive-test (&optional verbose)
  (let ((results t))
    (flet ((check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%"
                           expected actual)))
               (setf results (and pass results))))
           (truthy (test-name actual)
             (let ((pass (and actual t)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass (format t "    expected non-NIL, got NIL~%")))
               (setf results (and pass results))))
           (noun-of (np) (and np (getr 'word (daughter 'noun (daughter 'nbar np))))))

      ;; Fully dict-driven: the/girl/is/eating/pie/. -- `is' -> *be/auxverb via
      ;; defs.l's irreg, `eating' -> *eat/ing/part through morpho's -ing strip.
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*
                        *qp1-done-rule* *progressive-rules*)

      (let ((ok (parse-sentence "the girl is eating pie ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds: the girl is eating pie ." ok t)
        (truthy "top S is a major declarative" (subsetp '(decl major s) (fe c)))
        (check "the subject is \"the girl\"" (noun-of (daughter 'np c)) 'girl)

        (let ((aux (daughter 'aux c))
              (vp  (daughter 'vp c)))
          ;; The aux is the progressive: labelled prog, with `is' as its prog
          ;; daughter, and STARTAUX's tense/agreement still on it.
          (truthy "an aux attached to S" aux)
          (truthy "the aux is labelled prog" (and aux (member 'prog (fe aux))))
          (check "the aux's prog daughter is `is'"
                 (and aux (getr 'word (daughter 'prog aux))) 'is)
          (truthy "the aux keeps the present tense off `is'"
                  (and aux (member 'pres (fe aux))))
          (check "the aux was NOT read as a perfect" (and aux (member 'perf (fe aux))) nil)

          ;; ...so the participle, not the auxiliary, is the main verb.
          (truthy "a VP attached to S" vp)
          (check "the main verb is the participle `eating'"
                 (and vp (getr 'word (daughter 'verb vp))) 'eating)
          (truthy "the main verb carries `ing'"
                  (and vp (member 'ing (fe (daughter 'verb vp)))))
          (check "the object is \"pie\"" (and vp (noun-of (daughter 'np vp))) 'pie)
          (truthy "final punctuation attached to S" (daughter 'finalpunc c))

          ;; The case frame is the ordinary active transitive one: eat's agent is
          ;; the girl and its neutral the pie -- the progressive changes the aux,
          ;; not the argument structure.
          (let ((cf (and vp (getr 'caseframe vp))))
            (truthy "the VP has a case frame" cf)
            (check "the predicate is eat" (and cf (get cf 'pred)) 'eat)
            (closeframe openframe)
            (let ((filled (cadr (first (and cf (get cf 'hypo-slots))))))
              (truthy "the frame has filled cases" filled)
              (let ((agt (assoc 'agt filled)) (neut (assoc 'neut filled)))
                (truthy "an agent case was filled" agt)
                (check "the agent is `the girl'" (noun-of (cadr agt)) 'girl)
                (truthy "a neutral case was filled" neut)
                (check "the neutral is `pie'" (noun-of (cadr neut)) 'pie))))))

      ;; PAST progressive: same rule, the tense comes off `was'.
      (let ((ok (parse-sentence "the girl was eating pie ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds: the girl was eating pie ." ok t)
        (let ((aux (daughter 'aux c)))
          (truthy "the past-progressive aux is labelled prog"
                  (and aux (member 'prog (fe aux))))
          (truthy "the past-progressive aux carries `past'"
                  (and aux (member 'past (fe aux))))
          (check "its prog daughter is `was'"
                 (and aux (getr 'word (daughter 'prog aux))) 'was)))

      ;; A passive is still a passive: [*be][en] belongs to PASSIVE-AUX, and
      ;; PROGRESSIVE (keyed on [ing]) must not shadow it.
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule* *qp1-done-rule*
                        *progressive-rules* *raising-rules* *pp-rules*)
      (let ((ok (parse-sentence "a meeting is scheduled for friday ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "a passive still parses alongside PROGRESSIVE" ok t)
        (let ((aux (daughter 'aux c)))
          (check "the passive's aux is NOT labelled prog"
                 (and aux (member 'prog (fe aux))) nil)
          (check "the passive's aux has `is' as its passive daughter"
                 (and aux (getr 'word (daughter 'passive aux))) 'is)))

      ;; The regression guard: WITHOUT the rule group, the same sentence dies.
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule* *qp1-done-rule*)
      (let ((ok (handler-bind ((warning #'muffle-warning))
                  (parse-sentence "the girl is eating pie ."
                                  :initial-rule (intern "INITIAL-RULE" :parsifal)))))
        (check "without *progressive-rules* the progressive fails to parse" ok nil)))

    (format t "~&gram1-progressive-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-progressive-test t)
