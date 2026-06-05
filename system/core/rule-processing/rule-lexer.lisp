;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-

(in-package :parsifal)

;;; Lexer for the PIDGIN grammar rule language used by PARSIFAL.
;;;
;;; Scope of this first cut: the rule "frame" (header, packet list,
;;; priority, opening/closing braces) and the pattern language inside
;;; [...]. Action-body words are also tokenized to individual atoms
;;; so the same token stream can later feed an action parser, but the
;;; lexer makes no commitment about the action grammar.
;;;
;;; Each call to the closure returned by (pidgin-rule-lexer string)
;;; yields (values TYPE VALUE) or NIL at end of input. TYPE is a
;;; keyword chosen to drop straight into a cl-yacc grammar.
;;;
;;; See notes/pidgin-grammar-rules-1977.text for the corpus this lexer was
;;; designed against, notes/pidgin-grammar-intro.text for context, and
;;; notes/pidgin-grammar.pdf for Marcus's full description.


(cl-lex:define-string-lexer pidgin-rule-lexer

  ;; ---- skipped: whitespace and comments --------------------------------
  ;; A bare regex string (no body) means "match and discard".
  "[ \\t\\r\\n]+"

  ;; Line comments require >= 2 semicolons so a single `;' can still be
  ;; used as a separator inside patterns like [** c; ...].
  ";{2,}[^\\n]*"

  ;; %...% inline commentary, used by Marcus inside action bodies.
  ;; We forbid `{' and `}' inside the comment body so that an unbalanced
  ;; `%' caused by OCR damage cannot eat across rule boundaries (this is
  ;; a real hazard in the corpus -- see e.g. the stray `%' on the
  ;; NUMBER-AGREEMENT line of NP-COMPLETE).
  "%[^%{}]*%"

  ;; (COMMENT ...) paren-form section headers between rules.
  "\\([cC][oO][mM][mM][eE][nN][tT][^)]*\\)"


  ;; ---- multi-character operators (must precede single-char forms) ------
  ("-->"    (return (values :arrow    $@)))
  ("\\*\\*" (return (values :starstar $@)))


  ;; ---- single-character punctuation and operators ----------------------
  ("\\{"  (return (values :lbrace   $@)))
  ("\\}"  (return (values :rbrace   $@)))
  ("\\["  (return (values :lbracket $@)))
  ("\\]"  (return (values :rbracket $@)))
  ("\\("  (return (values :lparen   $@)))
  ("\\)"  (return (values :rparen   $@)))
  (","    (return (values :comma    $@)))
  (";"    (return (values :semi     $@)))
  (":"    (return (values :colon    $@)))
  ("\\."  (return (values :period   $@)))
  ("\\*"  (return (values :star     $@)))
  ("="    (return (values :equals   $@)))
  ("!"    (return (values :bang     $@)))


  ;; ---- ordinals (1st, 2nd, 3rd, 4th, ...) ------------------------------
  ;; Must precede the plain integer rule so `1st' is one token, not 1+st.
  ;; The capture group is required by cl-lex's regex parser (it does not
  ;; accept non-capturing (?:...) groups); $1 is unused.
  ("[0-9]+(st|nd|rd|th|ST|ND|RD|TH)"
   (return (values :position $@)))


  ;; ---- digit-led identifiers -------------------------------------------
  ;; Marcus uses identifier shapes that start with digits: rule names like
  ;; `99S-ATTACH' and packet names like `2-OBJ-INF-COMP'. These would
  ;; otherwise split into :NUMBER + (skipped hyphen) + :IDENTIFIER. Must
  ;; precede the plain integer rule. The hyphen after the digits is
  ;; optional so both `99S-ATTACH' and `2-OBJ-INF-COMP' match.
  ("[0-9]+-?[A-Za-z](-?[A-Za-z0-9_+?])*"
   (return (values :identifier $@)))


  ;; ---- numeric literals ------------------------------------------------
  ("[0-9]+\\.[0-9]+" (return (values :number (read-from-string $@))))
  ("[0-9]+"          (return (values :number (read-from-string $@))))


  ;; ---- quoted lexical items: 'word' or "word" --------------------------
  ;; The capture group strips the quotes from the returned value.
  ;; We forbid `{', `}', `[', `]', and newlines inside literals so an
  ;; unbalanced quote caused by OCR damage cannot span across structural
  ;; boundaries (compare the corresponding defense in the %...% rule).
  ("'([^'{}\\[\\]\\n]*)'"     (return (values :literal $1)))
  ("\"([^\"{}\\[\\]\\n]*)\""  (return (values :literal $1)))


  ;; ---- identifiers and frame keywords ----------------------------------
  ;; A leading letter, then alphanumerics, +, ?, _, and single hyphens
  ;; between runs of those (so `wh-comp' is one identifier but `foo-->'
  ;; still splits cleanly into `foo' and `-->'). The body re-types
  ;; frame-level keywords; everything else is a generic :identifier.
  ;; The inner group is capturing only because cl-lex does not accept
  ;; non-capturing (?:...) groups; $1 is unused.
  ("[A-Za-z](-?[A-Za-z0-9_+?])*"
   (let ((up (string-upcase $@)))
     (cond
       ((string= up "RULE")       (return (values :rule       $@)))
       ((string= up "CRULE")      (return (values :crule      $@)))
       ((string= up "AS")         (return (values :as         $@)))
       ((string= up "NR")         (return (values :nr         $@)))
       ((string= up "ATTACHMENT") (return (values :attachment $@)))
       ((string= up "CREATION")   (return (values :creation   $@)))
       ((string= up "IN")         (return (values :in         $@)))
       ((string= up "OVER")       (return (values :over       $@)))
       ((string= up "PRIORITY")   (return (values :priority   $@)))
       (t                         (return (values :identifier $@)))))))


;;; -------- convenience helpers --------------------------------------

(defun tokenize-rule-text (string)
  "Run the lexer over STRING and collect a list of (type value) pairs.
   Useful for inspection from the REPL and from the test suite."
  (let ((lex (pidgin-rule-lexer string))
        (out '()))
    (loop
      (multiple-value-bind (type value) (funcall lex)
        (unless type (return (nreverse out)))
        (push (list type value) out)))))

(defun tokenize-rule-file (path)
  "Tokenize the text in PATH and return the list of (type value) pairs."
  (with-open-file (in path :direction :input)
    (let ((buf (make-string (file-length in))))
      (read-sequence buf in)
      (tokenize-rule-text buf))))
