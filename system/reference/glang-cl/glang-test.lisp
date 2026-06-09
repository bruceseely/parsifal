;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/reference/glang-cl/glang-test.lisp
;;;
;;; End-to-end tests for the FOO vertical slice. Exercises the
;;; tokenizer + Pratt + denotations + compiler pipeline against
;;; expected intermediate and emitted forms.

(in-package #:glang-cl)


(defun glang-test (&optional verbose)
  (let ((results t))
    (flet ((check (name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%"
                           expected actual)))
               (setf results (and pass results)))))

      ;; --- helpers in isolation ---

      (check "tokenize the canonical FOO rule"
             (tokenize "{RULE FOO IN BAR [t] --> Activate cpool.}")
             '(|{| rule foo in bar |[| t |]|
                   -->
                   activate cpool |.| |}|))


      ;; --- intermediate form (BUILD-RULE output) ---

      (let ((intermediate
              (parse-rule "{RULE FOO IN BAR [t] --> Activate cpool.}")))

        (check "intermediate: tag is `rule'"
               (first intermediate)
               'rule)

        (check "intermediate: rule name"
               (second intermediate)
               'foo)

        (check "intermediate: rule kind"
               (first (third intermediate))
               'normal)

        (check "intermediate: priority defaults to 10"
               (second (third intermediate))
               10)

        (check "intermediate: packets"
               (third (third intermediate))
               '(bar))

        (check "intermediate: feature info (no index, no extra pfeats)"
               (fourth (third intermediate))
               '(noindexf))

        (check "intermediate: pattern body for [t] in first position"
               (fourth intermediate)
               't)

        (check "intermediate: action body"
               (fifth intermediate)
               '(progn (activate '(cpool)))))


      ;; --- emitted form (compile-rule output) ---

      (let ((emitted (compile-rule "{RULE FOO IN BAR [t] --> Activate cpool.}")))

        (check "emitted: starts with (progn 'compile ...)"
               (list (first emitted) (second emitted))
               '(progn 'compile))

        (check "emitted: rule-index call shape"
               (let ((ri (third emitted)))
                 (list (first ri) (second ri) (third ri) (fourth ri)
                       ;; the trailing '(priority pat-name name act-name) list
                       (cadr (fifth ri))))
               '(rule-index 'normal '(bar) 'noindexf
                 (10 |::PAT-OF-FOO| foo |::ACT-OF-FOO|)))

        (check "emitted: defun for pattern"
               (let ((pdef (fourth emitted)))
                 (list (first pdef) (second pdef) (third pdef) (fourth pdef)))
               '(defun |::PAT-OF-FOO| () t))

        (check "emitted: defun for action"
               (let ((adef (fifth emitted)))
                 (list (first adef) (second adef) (third adef) (fourth adef)))
               '(defun |::ACT-OF-FOO| () (progn (activate '(cpool)))))

        (check "emitted: featindexify nil (no pfeats)"
               (sixth emitted)
               '(featindexify nil)))


      ;; --- surface-structure crules (CREATION / ATTACHMENT) ---

      ;; A creation crule: node-spec is the lone node-type.
      (let ((emitted (compile-rule
                      "{CREATION CRULE S-CREATE S Activate cpool.}")))
        (check "crule(creation): (progn 'compile ...)"
               (list (first emitted) (second emitted))
               '(progn 'compile))
        (check "crule(creation): crule-index call shape"
               (third emitted)
               '(crule-index 'creation
                 '(s |::CRULE-OF-S-CREATE| s-create)))
        (check "crule(creation): defun ::crule-of-NAME with body"
               (fourth emitted)
               '(defun |::CRULE-OF-S-CREATE| () (progn (activate '(cpool))))))

      ;; An attachment crule: node-spec is the dotted (UPPER . LOWER).
      (let ((emitted (compile-rule
                      "{ATTACHMENT CRULE VP-VERB VP OVER VERB Activate cpool.}")))
        (check "crule(attachment): crule-index files (upper . lower)"
               (third emitted)
               '(crule-index 'attachment
                 '((vp . verb) |::CRULE-OF-VP-VERB| vp-verb)))
        (check "crule(attachment): defun ::crule-of-NAME with body"
               (fourth emitted)
               '(defun |::CRULE-OF-VP-VERB| () (progn (activate '(cpool))))))

      ;; The intermediate before compilation: (crule NAME TYPE NODES BODY).
      (check "crule intermediate: creation shape"
             (let ((i (parse-rule "{CREATION CRULE NP-START NP Activate npool.}")))
               (list (first i) (second i) (third i) (fourth i)))
             '(crule np-start creation (np)))
      (check "crule intermediate: attachment nodes are (upper lower)"
             (fourth (parse-rule
                      "{ATTACHMENT CRULE NP-PP NP OVER PP Activate npool.}"))
             '(np pp))


      ;; --- crule-body verbs: real grammar crules compile end-to-end ---
      ;; `upper'/`lower' -> fnode/snode; associate / fills / case-frame-of /
      ;; finalize / indirect compose with the ported set / binding / current.

      (flet ((body (text) (fifth (parse-rule text))))

        ;; gram1.l VP-NP: the lone attachment statement.
        (check "fills: `lower ... fills an obj slot of the upper'"
               (body "{ATTACHMENT CRULE VP-NP VP OVER NP
                      The lower node fills an obj slot of the upper.}")
               '(progn (fillslot snode 'obj fnode)))

        ;; gram5.l NBAR-PP: two statements chained by `.'.
        (check "associate new mod cf, then fills mod slot"
               (body "{ATTACHMENT CRULE NBAR-PP NBAR OVER PP
                      Associate a new mod case frame with the lower node.
                      The lower node fills a mod slot of the upper node.}")
               '(progn (associate-cf (newcf 'mod) snode)
                       (fillslot snode 'mod fnode)))

        ;; `case frame of X' accessor + `current s'.
        (check "associate `the case frame of the upper node' with current s"
               (body "{ATTACHMENT CRULE T1 S OVER NP
                      Associate the case frame of the upper node
                      with the current s.}")
               '(progn (associate-cf (getr 'caseframe fnode) (current-s))))

        ;; `finalize the cf of X'.
        (check "finalize the cf of the lower node"
               (body "{ATTACHMENT CRULE T2 S OVER NP
                      Finalize the cf of the lower node.}")
               '(progn (finalize-frame snode)))

        ;; `indirect object of X' composing with the ported `set'/`binding'.
        (check "set the binding to the indirect object of upper"
               (body "{ATTACHMENT CRULE T3 S OVER NP
                      Set the binding of lower to the indirect object of upper.}")
               '(progn (setr 'binding (io fnode) snode)))

        ;; gram2.l NP-S: `!'(inf-comp)' -- Marcus's `!' read-macro escapes
        ;; to Lisp syntax for a literal datum, inside an `if ... then'.
        (check "!'(...) literal-Lisp quote inside an if/then"
               (body "{ATTACHMENT CRULE NP-S NP OVER S
                      If lower is inf-s
                       then set the markers register of upper to !'(inf-comp).}")
               '(progn (cond ((is snode '(inf-s))
                              (setr 'markers '(inf-comp) fnode)))))

        ;; gram1.l VP-NP's delta-subject guard: `it isn't true that X'.
        (check "it isn't true that <clause>  -->  (not <clause>)"
               (body "{ATTACHMENT CRULE T4 S OVER NP
                      If it isn't true that there is a binding of lower
                       then finalize the cf of lower.}")
               '(progn (cond ((not (setq *it* (binding snode)))
                              (finalize-frame snode))))))

      ;; The `!' read-macro at the tokenizer level: read one Lisp form.
      (check "tokenizer: !'(a b) reads one literal Lisp datum"
             (tokenize "!'(a b)")
             '((quote (a b))))
      (check "tokenizer: !(foo) reads a bare list datum"
             (tokenize "!(foo)")
             '((foo)))


      ;; --- new denotations: action verbs ---

      (flet ((action-of (text)
               (fifth (parse-rule text))))

        (check "Deactivate verb"
               (action-of "{RULE X IN P [t] --> Deactivate cpool.}")
               '(progn (deactivate '(cpool))))

        (check "Activate with multiple packets"
               (action-of "{RULE X IN P [t] --> Activate cpool, ss-start.}")
               '(progn (activate '(cpool ss-start))))

        (check "Restore the buffer"
               (action-of "{RULE X IN P [t] --> Restore the buffer.}")
               '(progn (bufrestore)))

        (check "Run X next"
               (action-of "{RULE X IN P [t] --> Run aux-inversion next.}")
               '(progn (setq *nextrule* 'aux-inversion)))

        (check "Parse is finished"
               (action-of "{RULE X IN P [t] --> Parse is finished.}")
               '(progn (setq *parsecomplete* t)))


        ;; --- new denotations: nilfix atoms ---
        ;; Atoms are tested as standalone expressions via PARSE-STRING.
        ;; (Embedding them in an action verb that uses EAT-TOKEN would
        ;; capture the literal symbol instead of invoking the atom's nud.)

        ;; `last' emits `(last*)', not Marcus's `(:last)': the runtime
        ;; renamed the function (`:last' is a keyword, not a legal CL
        ;; function name). See buffer-ops.lisp's LAST* and the
        ;; denotations.lisp `last' nilfix.
        (check "`last' atom"
               (parse-string "last")
               (list 'last*))

        (check "`it' atom"
               (parse-string "it")
               '*it*)

        (check "`wh-comp' atom"
               (parse-string "wh-comp")
               '(wh-comp))

        (check "`current s' atom (consumes literal `s')"
               (parse-string "current s")
               '(current-s))


        ;; --- new denotations: pattern feature match `=' ---

        (let* ((r (parse-rule "{RULE X IN P [=np] --> Drop c.}"))
               (pat (fourth r)))
          (check "[=np] compiles via fast-is"
                 pat
                 '(fast-is 0 (np))))

        (let* ((r (parse-rule "{RULE X IN P [=np, finalpunc] --> Drop c.}"))
               (pat (fourth r)))
          (check "[=np, finalpunc] -- multiple features"
                 pat
                 '(fast-is 0 (np finalpunc))))


        ;; --- new denotations: relational infixes ---
        ;; (tested inside pattern bodies, which is where `is/not/none/any'
        ;; are mostly used)

        (let* ((r (parse-rule "{RULE X IN P [* is np] --> Drop c.}"))
               (pat (fourth r)))
          (check "[* is np]"
                 pat
                 '(is |1ST| '(np))))

        (let* ((r (parse-rule "{RULE X IN P [* is not np] --> Drop c.}"))
               (pat (fourth r)))
          (check "[* is not np] -> is-not-all-of"
                 pat
                 '(is-not-all-of |1ST| '(np))))

        (let* ((r (parse-rule "{RULE X IN P [* is any of np, verb] --> Drop c.}"))
               (pat (fourth r)))
          (check "[* is any of ...] -> is-any-of"
                 pat
                 '(is-any-of |1ST| '(np verb))))

        (let* ((r (parse-rule "{RULE X IN P [* is none of np, verb] --> Drop c.}"))
               (pat (fourth r)))
          (check "[* is none of ...] -> is-none-of"
                 pat
                 '(is-none-of |1ST| '(np verb))))


        ;; --- `;' as `and' inside pattern ---

        (let* ((r (parse-rule "{RULE X IN P [=np; * is not verb] --> Drop c.}"))
               (pat (fourth r)))
          (check "[=np ; * is not verb] -> (and ...)"
                 pat
                 '(and (fast-is 0 (np))
                       (is-not-all-of |1ST| '(verb)))))


        ;; --- node-op verbs ---

        (check "Attach 1st to c as np"
               (action-of "{RULE X IN P [t] --> Attach 1st to c as np.}")
               '(progn (attach c |1ST| 'np)))

        (check "Attach c as foo (uses attach1)"
               (action-of "{RULE X IN P [t] --> Attach c as foo.}")
               '(progn (attach1 c c 'foo)))

        (check "Drop c"
               (action-of "{RULE X IN P [t] --> Drop c.}")
               '(progn (drop 0)))

        (check "Drop c into the buffer"
               (action-of "{RULE X IN P [t] --> Drop c into the buffer.}")
               '(progn (drop 0)))

        (check "Drop c before 2nd"
               (action-of "{RULE X IN P [t] --> Drop c before 2nd.}")
               '(progn (drop 1)))

        (check "Insert NODE into the buffer before 1st"
               (action-of "{RULE X IN P [t] --> Insert foo into the buffer before 1st.}")
               '(progn (insert-node foo 0)))

        (check "Label NODE feat1, feat2"
               (action-of "{RULE X IN P [t] --> Label c quant, det.}")
               '(progn (addf1 c '(quant det))))

        (check "Remove features f1, f2 from X"
               (action-of "{RULE X IN P [t] --> Remove features quant, det from c.}")
               '(progn (remf1 '(quant det) c)))

        (check "Transfer features f1, f2 from SRC to DST"
               (action-of "{RULE X IN P [t] --> Transfer features pres, past from 1st to c.}")
               '(progn (transfer '(pres past) |1ST| c)))

        ;; `features' is a prefix; test in pattern context where pratt-parse
        ;; sees it as the head of an expression (Activate's arg goes through
        ;; get-var-list, which doesn't invoke nuds).
        (let ((r (parse-rule "{RULE X IN P [** c; features of c is np] --> Drop c.}")))
          (check "Features of X -> (fe X)"
                 (fourth r)
                 '(is (fe c) '(np))))


        ;; --- tree-access infixes ---

        (let ((act (action-of "{RULE X IN P [t] --> Label the quant of c with foo.}")))
          (check "X of Y -> (find-node 'X Y) via right-side parse"
                 act
                 '(progn (addf1 (find-node 'quant c) '(foo)))))

        (let ((r (parse-rule "{RULE X IN P [** c; the binding of c is np] --> Drop c.}")))
          (check "Binding of c (in pattern)"
                 (fourth r)
                 '(is (binding c) '(np))))

        ;; Tested in pattern context, same reason as `features' above.
        (let ((r (parse-rule "{RULE X IN P [** c; the quant register of 1st is foo] --> Drop c.}")))
          (check "X register of Y -> (getr 'X Y)"
                 (fourth r)
                 '(is (getr 'quant |1ST|) '(foo))))


        ;; --- function-call `(' ---
        ;; Tested via Set (its `val' parses through `right', which
        ;; handles function-call syntax).

        ;; (avoid `a' and `b' as argument names -- ADVANCE filters out
        ;; the articles `the', `an', `a' as Marcus intends)
        (let ((act (action-of "{RULE X IN P [t] --> Set foo of c to plus(x, y).}")))
          (check "f(x, y) -> (f x y)"
                 act
                 '(progn (setr 'foo (plus x y) c))))

        (let ((act (action-of "{RULE X IN P [t] --> Set foo of c to plus(1st, 2nd).}")))
          (check "f with positional args"
                 act
                 '(progn (setr 'foo (plus |1ST| |2ND|) c))))


        ;; --- set ---

        (let ((act (action-of "{RULE X IN P [t] --> Set the quant of c to foo.}")))
          (check "Set the X of Y to Z"
                 act
                 '(progn (setr 'quant foo c))))


        ;; --- if/then/else ---

        (let ((act (action-of "{RULE X IN P [t] --> If c is np then Drop c.}")))
          (check "If COND then ACTION"
                 act
                 '(progn (cond ((is c '(np)) (drop 0))))))

        (let ((act (action-of "{RULE X IN P [t] -->
                                If c is np then Drop c else Activate cpool.}")))
          (check "If COND then A else B"
                 act
                 '(progn (cond ((is c '(np)) (drop 0))
                               ((activate '(cpool)))))))


        ;; --- node creation: new and create ---

        (check "Label a new TYPE node FEATS"
               (action-of "{RULE X IN P [t] --> Label a new num node 99s.}")
               '(progn (addf1 (newnode 'num nil) '(|99S|))))

        (check "Label a new TYPE node (no features)"
               (action-of "{RULE X IN P [t] --> Label a new time node.}")
               '(progn (addf1 (newnode 'time nil) 'nil)))

        (check "Create [new] TYPE node labelled FEATS"
               (action-of
                "{RULE X IN P [t] --> Create a new num node labelled bignumg.}")
               '(progn (newnode 'num '(bignumg))))

        (check "Create TYPE node (no labelled clause)"
               (action-of "{RULE X IN P [t] --> Create a num node.}")
               '(progn (newnode 'num nil)))


        ;; --- there is / there is not ---

        (check "`there is X' compiles to (setq *it* X)"
               (action-of
                "{RULE X IN P [t] --> If there is a conj of c then Drop c.}")
               '(progn (cond ((setq *it* (find-node 'conj c))
                              (drop 0)))))

        (check "`there is not X' compiles to (null X)"
               (action-of
                "{RULE X IN P [t] -->
                 If there is not a subj of c then Drop c.}")
               '(progn (cond ((null (find-node 'subj c)) (drop 0)))))


        ;; --- two-clause pattern across positions ---
        ;; Stripped-down version of gram4.l's NINETY-NINE; we don't have
        ;; `new num node' as a denotation yet, so we use a simple `Drop c'
        ;; action to keep the test self-contained.

        (let* ((text "{RULE NINETY-NINE-LIKE IN BUILD-NUMBER
                      [=tens] [=ones] --> Drop c.}")
               (r (parse-rule text))
               (pat (fourth r)))
          (check "two pattern positions: [=tens] [=ones] -> (and ...)"
                 pat
                 '(and (fast-is 0 (tens))
                       (and (or |2ND| (set* 1)) (fast-is 1 (ones))))))))

    (when verbose
      (format t "~&glang-test: ~:[FAILED~;passed~]~%" results))
    results))
