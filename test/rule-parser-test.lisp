;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-

(in-package :parsifal)

;;; Tests for system/core/rule-processing/rule-parser.lisp.
;;;
;;; (rule-parser-test)   -> t if all pass, nil otherwise
;;; (rule-parser-test t) -> same, but prints every case as it runs


(defun rule-parser-test (&optional verbose)
  (let ((results t))
    (flet ((check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%"
                           expected actual)))
               (setf results (and pass results))))
           (one-rule (text)
             (first (parse-rule-text text))))

      ;; --- empty input ---

      (check "empty input parses to empty list"
             (parse-rule-text "")
             nil)


      ;; --- minimal rule ---

      (let ((r (one-rule "{RULE FOO IN BAR [t] --> Drop c.}")))
        (check "minimal rule: kind"     (getf r :kind)     :rule)
        (check "minimal rule: name"     (getf r :name)     "FOO")
        (check "minimal rule: priority" (getf r :priority) nil)
        (check "minimal rule: packets"  (getf r :packets)  '("BAR"))
        (check "minimal rule: one pattern"
               (length (getf r :patterns)) 1)
        (check "minimal rule: pattern class"
               (getf (first (getf r :patterns)) :class)
               :wildcard)
        (check "minimal rule: action body preserved"
               (getf r :action)
               '((:identifier "Drop") (:identifier "c") (:period "."))))


      ;; --- priority with trailing period ---

      (let ((r (one-rule "{RULE FOO PRIORITY: 15. IN BAR [t] --> Drop c.}")))
        (check "priority parsed as number"
               (getf r :priority)
               15))


      ;; --- priority without trailing period ---

      (let ((r (one-rule "{RULE FOO PRIORITY: 5 IN BAR [t] --> Drop c.}")))
        (check "priority without trailing period"
               (getf r :priority)
               5))


      ;; --- packet list ---

      (let ((r (one-rule "{RULE FOO IN P1, P2, P3 [t] --> Drop c.}")))
        (check "comma-separated packet list"
               (getf r :packets)
               '("P1" "P2" "P3")))


      ;; --- no patterns (allowed by grammar; corpus has a couple) ---

      (let ((r (one-rule "{RULE FOO IN BAR --> Drop c.}")))
        (check "rule with no patterns"
               (getf r :patterns)
               nil))


      ;; --- multiple patterns, classified ---

      (let* ((r (one-rule
                 "{RULE FOO IN BAR
                  [=np] [=*to, auxverb] [* is any of X] [** c; cond1; cond2]
                  --> Drop c.}"))
             (classes (mapcar (lambda (p) (getf p :class))
                              (getf r :patterns))))
        (check "four patterns, classes in order"
               classes
               '(:feature-match :feature-match :test :context)))

      (let* ((r (one-rule "{RULE FOO IN BAR [t] [=np] --> Drop c.}"))
             (p1 (first  (getf r :patterns)))
             (p2 (second (getf r :patterns))))
        (check "wildcard pattern :raw"
               (getf p1 :raw)
               '((:identifier "t")))
        (check "feature-match :raw includes the EQUALS and identifier"
               (getf p2 :raw)
               '((:equals "=") (:identifier "np"))))


      ;; --- AS RULE / NR RULE ---

      (let ((r (one-rule "{AS RULE STARTNP IN CPOOL [t] --> Drop c.}")))
        (check "AS RULE kind" (getf r :kind) :as-rule)
        (check "AS RULE name" (getf r :name) "STARTNP"))

      (let ((r (one-rule "{NR RULE NBAR-COMPLETE IN CPOOL [t] --> Drop c.}")))
        (check "NR RULE kind" (getf r :kind) :nr-rule)
        (check "NR RULE name" (getf r :name) "NBAR-COMPLETE"))


      ;; --- attachment / creation case rules ---

      (let ((r (one-rule
                "{ATTACHMENT CRULE VP-VERB VP OVER VERB
                 Associate a new case frame with the upper node.}")))
        (check "attachment crule kind"
               (getf r :kind) :attachment-crule)
        (check "attachment crule name"
               (getf r :name) "VP-VERB")
        (check "attachment crule upper / lower"
               (list (getf r :upper) (getf r :lower))
               '("VP" "VERB")))

      (let ((r (one-rule "{CREATION CRULE S-CREATE S Set the foo.}")))
        (check "creation crule kind"
               (getf r :kind) :creation-crule)
        (check "creation crule name"
               (getf r :name) "S-CREATE")
        (check "creation crule node-type"
               (getf r :node-type) "S"))

      ;; Marcus's `*' node type appears as both a rule-name prefix
      ;; (`*-DUMMY') and a bare node-type in CREATION CRULE.
      (let ((r (one-rule "{CREATION CRULE *-DUMMY * Set the father of c to t.}")))
        (check "creation crule with *-name"
               (getf r :name) "*-DUMMY")
        (check "creation crule with bare * node-type"
               (getf r :node-type) "*"))


      ;; --- multiple rules ---

      (let ((rs (parse-rule-text
                 "{RULE FOO IN A [t] --> Drop c.}
                  {RULE BAR IN B [t] --> Drop c.}")))
        (check "two rules parse to a list of two"
               (length rs) 2)
        (check "second rule's name"
               (getf (second rs) :name) "BAR"))


      ;; --- top-level noise tolerance (Marcus's gram4.l style) ---

      (let ((rs (parse-rule-text
                 ";this is a single-semi comment line
                  ;and another
                  {RULE FOO IN A [t] --> Drop c.}")))
        (check "single-; top-level comments are skipped"
               (length rs) 1)
        (check "rule after top-level noise still parses"
               (getf (first rs) :name) "FOO"))

      (let ((rs (parse-rule-text
                 "{RULE FOO IN A [t] --> Drop c.}
                  ;between rules
                  {RULE BAR IN B [t] --> Drop c.}")))
        (check "single-; noise between rules is skipped"
               (length rs) 2))

      ;; Embedded Lisp at top level is data, not a rule. The inner
      ;; `{If ...}' is a Pidgin expression embedded in Lisp; the parser
      ;; must not mistake it for a top-level rule.
      (let ((rs (parse-rule-text
                 "(defun find-WH-comp (node-variable)
                    {If there is not a node-variable then nil})
                  {RULE FOO IN A [t] --> Drop c.}")))
        (check "embedded Lisp form before a rule is ignored"
               (length rs) 1)
        (check "rule after embedded Lisp still parses"
               (getf (first rs) :name) "FOO"))

      ;; A rule whose action body contains a nested `{...}' must not
      ;; terminate at the inner `}'. The whole nested form belongs to
      ;; the action body.
      (let* ((rs (parse-rule-text
                  "{RULE FOO IN A [t] -->
                    Set the foo of c to {If x then y else z}.}
                   {RULE BAR IN B [t] --> Drop c.}"))
             (foo (first rs))
             (bar (second rs)))
        (check "two rules parsed despite nested {} in action body"
               (length rs) 2)
        (check "first rule's name is FOO"
               (getf foo :name) "FOO")
        (check "second rule's name is BAR (not eaten by FOO)"
               (getf bar :name) "BAR")
        (check "FOO's action body includes the nested { and }"
               (let ((act (getf foo :action)))
                 (list (not (null (find :lbrace act :key #'first)))
                       (not (null (find :rbrace act :key #'first)))))
               '(t t)))


      ;; --- rule-summary helper ---

      (let* ((r (one-rule
                 "{RULE FOO PRIORITY: 7 IN BAR, BAZ [=np] [t] --> Drop c.}"))
             (s (rule-summary r)))
        (check "rule-summary"
               s
               '(:kind     :rule
                 :name     "FOO"
                 :priority 7
                 :packets  ("BAR" "BAZ")
                 :patterns (:feature-match :wildcard)))))

    (when verbose
      (format t "~&rule-parser-test: ~:[FAILED~;passed~]~%" results))
    results))
