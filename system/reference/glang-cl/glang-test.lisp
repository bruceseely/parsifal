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
               '(progn (setq :nextrule 'aux-inversion)))

        (check "Parse is finished"
               (action-of "{RULE X IN P [t] --> Parse is finished.}")
               '(progn (setq :parsecomplete t)))


        ;; --- new denotations: nilfix atoms ---
        ;; Atoms are tested as standalone expressions via PARSE-STRING.
        ;; (Embedding them in an action verb that uses EAT-TOKEN would
        ;; capture the literal symbol instead of invoking the atom's nud.)

        (check "`last' atom"
               (parse-string "last")
               (list :last))

        (check "`it' atom"
               (parse-string "it")
               :it)

        (check "`wh-comp' atom"
               (parse-string "wh-comp")
               '(wh-comp))

        (check "`current s' atom (consumes literal `s')"
               (parse-string "current s")
               '(current-s))


        ;; --- new denotations: pattern feature match `=' ---

        (let* ((r (parse-rule "{RULE X IN P [=np] --> Drop.}"))
               (pat (fourth r)))
          (check "[=np] compiles via fast-is"
                 pat
                 '(fast-is 0 (np))))

        (let* ((r (parse-rule "{RULE X IN P [=np, finalpunc] --> Drop.}"))
               (pat (fourth r)))
          (check "[=np, finalpunc] -- multiple features"
                 pat
                 '(fast-is 0 (np finalpunc))))


        ;; --- new denotations: relational infixes ---
        ;; (tested inside pattern bodies, which is where `is/not/none/any'
        ;; are mostly used)

        (let* ((r (parse-rule "{RULE X IN P [* is np] --> Drop.}"))
               (pat (fourth r)))
          (check "[* is np]"
                 pat
                 '(is |1ST| '(np))))

        (let* ((r (parse-rule "{RULE X IN P [* is not np] --> Drop.}"))
               (pat (fourth r)))
          (check "[* is not np] -> is-not-all-of"
                 pat
                 '(is-not-all-of |1ST| '(np))))

        (let* ((r (parse-rule "{RULE X IN P [* is any of np, verb] --> Drop.}"))
               (pat (fourth r)))
          (check "[* is any of ...] -> is-any-of"
                 pat
                 '(is-any-of |1ST| '(np verb))))

        (let* ((r (parse-rule "{RULE X IN P [* is none of np, verb] --> Drop.}"))
               (pat (fourth r)))
          (check "[* is none of ...] -> is-none-of"
                 pat
                 '(is-none-of |1ST| '(np verb))))


        ;; --- `;' as `and' inside pattern ---

        (let* ((r (parse-rule "{RULE X IN P [=np; * is not verb] --> Drop.}"))
               (pat (fourth r)))
          (check "[=np ; * is not verb] -> (and ...)"
                 pat
                 '(and (fast-is 0 (np))
                       (is-not-all-of |1ST| '(verb)))))))

    (when verbose
      (format t "~&glang-test: ~:[FAILED~;passed~]~%" results))
    results))
