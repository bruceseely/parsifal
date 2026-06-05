;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-

(in-package :parsifal)

;;; Tests for system/core/rule-processing/rule-lexer.lisp.
;;;
;;; (rule-lexer-test)   -> t if all pass, nil otherwise
;;; (rule-lexer-test t) -> same, but prints every case as it runs


(defun rule-lexer-test (&optional verbose)
  (let ((results t))
    (flet ((check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%"
                           expected actual)))
               (setf results (and pass results)))))

      ;; --- whitespace and comment skipping ---

      (check "all whitespace yields nothing"
             (tokenize-rule-text "   ")
             '())

      (check "trailing newline only"
             (tokenize-rule-text (format nil "  ~%~%"))
             '())

      (check ";; line comment is skipped"
             (tokenize-rule-text ";;  this is a comment")
             '())

      (check "single ; is a SEMI separator, not a comment"
             (tokenize-rule-text ";")
             '((:semi ";")))

      (check "%...% inline comment is skipped"
             (tokenize-rule-text "%foo bar%")
             '())

      (check "(COMMENT ...) section is skipped"
             (tokenize-rule-text "(COMMENT FRAGMENT UTTERANCES)")
             '())

      (check "lowercase (comment ...) is skipped"
             (tokenize-rule-text "(comment hi)")
             '())


      ;; --- punctuation and operators ---

      (check "all single-char punctuation"
             (tokenize-rule-text "{}[](),:;.")
             '((:lbrace "{") (:rbrace "}")
               (:lbracket "[") (:rbracket "]")
               (:lparen "(") (:rparen ")")
               (:comma ",") (:colon ":") (:semi ";") (:period ".")))

      (check "arrow and pattern operators"
             (tokenize-rule-text "--> = * ** !")
             '((:arrow "-->")
               (:equals "=")
               (:star "*")
               (:starstar "**")
               (:bang "!")))

      (check "** is greedy: ** stays one token, not two stars"
             (tokenize-rule-text "**")
             '((:starstar "**")))


      ;; --- numeric and ordinal literals ---

      (check "integer literal"
             (tokenize-rule-text "15")
             '((:number 15)))

      (check "decimal literal"
             (tokenize-rule-text "4.5")
             '((:number 4.5)))

      (check "ordinal positions"
             (tokenize-rule-text "1st 2nd 3rd 4th")
             '((:position "1st") (:position "2nd")
               (:position "3rd") (:position "4th")))


      ;; --- quoted lexical items ---

      (check "single-quoted word strips quotes"
             (tokenize-rule-text "'you'")
             '((:literal "you")))

      (check "double-quoted word strips quotes"
             (tokenize-rule-text "\"that\"")
             '((:literal "that")))


      ;; --- identifiers and frame keywords ---

      (check "plain identifier"
             (tokenize-rule-text "foo")
             '((:identifier "foo")))

      (check "hyphenated identifier stays whole"
             (tokenize-rule-text "ss-start parse-subj WH-comp")
             '((:identifier "ss-start")
               (:identifier "parse-subj")
               (:identifier "WH-comp")))

      (check "identifier with + and ?"
             (tokenize-rule-text "bignum+ what?")
             '((:identifier "bignum+") (:identifier "what?")))

      (check "digit-led identifier with hyphen (2-OBJ-INF-COMP)"
             (tokenize-rule-text "2-OBJ-INF-COMP")
             '((:identifier "2-OBJ-INF-COMP")))

      (check "digit-led identifier without hyphen (99S-ATTACH)"
             (tokenize-rule-text "99S-ATTACH")
             '((:identifier "99S-ATTACH")))

      (check "plain number still wins when no letter follows"
             (tokenize-rule-text "15")
             '((:number 15)))

      (check "ordinal still wins over digit-led identifier"
             (tokenize-rule-text "1st")
             '((:position "1st")))

      (check "frame keywords are typed (any case)"
             (tokenize-rule-text "RULE crule As nr ATTACHMENT Creation IN over Priority")
             '((:rule "RULE") (:crule "crule") (:as "As") (:nr "nr")
               (:attachment "ATTACHMENT") (:creation "Creation")
               (:in "IN") (:over "over") (:priority "Priority")))


      ;; --- splitting around -->  ---

      (check "identifier and arrow split with no space between"
             (tokenize-rule-text "foo-->")
             '((:identifier "foo") (:arrow "-->")))


      ;; --- short rule-frame examples ---

      (check "IMPERATIVE rule header"
             (tokenize-rule-text "{RULE IMPERATIVE IN SS-START")
             '((:lbrace "{")
               (:rule "RULE")
               (:identifier "IMPERATIVE")
               (:in "IN")
               (:identifier "SS-START")))

      (check "PRIORITY with colon and number"
             (tokenize-rule-text "PRIORITY: 15")
             '((:priority "PRIORITY") (:colon ":") (:number 15)))

      (check "wildcard pattern [t]"
             (tokenize-rule-text "[t]")
             '((:lbracket "[")
               (:identifier "t")
               (:rbracket "]")))

      (check "feature pattern [=np]"
             (tokenize-rule-text "[=np]")
             '((:lbracket "[")
               (:equals "=")
               (:identifier "np")
               (:rbracket "]")))

      (check "literal-word pattern [=*to, auxverb]"
             (tokenize-rule-text "[=*to, auxverb]")
             '((:lbracket "[")
               (:equals "=")
               (:star "*")
               (:identifier "to")
               (:comma ",")
               (:identifier "auxverb")
               (:rbracket "]")))

      (check "context pattern [** c; the np of c is trace]"
             (tokenize-rule-text "[** c; the np of c is trace]")
             '((:lbracket "[")
               (:starstar "**")
               (:identifier "c")
               (:semi ";")
               (:identifier "the")
               (:identifier "np")
               (:identifier "of")
               (:identifier "c")
               (:identifier "is")
               (:identifier "trace")
               (:rbracket "]")))


      ;; --- full small rule end-to-end ---

      (check "complete IMPERATIVE rule"
             (tokenize-rule-text
              "{RULE IMPERATIVE IN SS-START
              [=tnsless] -->
              Label c imper, major.
              Insert the word 'you' into the buffer.
              Deactivate ss-start. Activate parse-subj.}")
             '((:lbrace "{")
               (:rule "RULE")
               (:identifier "IMPERATIVE")
               (:in "IN")
               (:identifier "SS-START")
               (:lbracket "[")
               (:equals "=")
               (:identifier "tnsless")
               (:rbracket "]")
               (:arrow "-->")
               (:identifier "Label")
               (:identifier "c")
               (:identifier "imper")
               (:comma ",")
               (:identifier "major")
               (:period ".")
               (:identifier "Insert")
               (:identifier "the")
               (:identifier "word")
               (:literal "you")
               (:identifier "into")
               (:identifier "the")
               (:identifier "buffer")
               (:period ".")
               (:identifier "Deactivate")
               (:identifier "ss-start")
               (:period ".")
               (:identifier "Activate")
               (:identifier "parse-subj")
               (:period ".")
               (:rbrace "}")))


      ;; --- attachment crule header ---

      (check "ATTACHMENT CRULE header"
             (tokenize-rule-text "{ATTACHMENT CRULE VP-VERB VP OVER VERB")
             '((:lbrace "{")
               (:attachment "ATTACHMENT")
               (:crule "CRULE")
               (:identifier "VP-VERB")
               (:identifier "VP")
               (:over "OVER")
               (:identifier "VERB"))))

    (when verbose
      (format t "~&rule-lexer-test: ~:[FAILED~;passed~]~%" results))
    results))
