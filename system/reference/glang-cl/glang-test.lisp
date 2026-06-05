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
               '(featindexify nil))))

    (when verbose
      (format t "~&glang-test: ~:[FAILED~;passed~]~%" results))
    results))
