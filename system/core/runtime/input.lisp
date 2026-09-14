;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/core/runtime/input.lisp
;;;
;;; Sentence input and the top-level parse driver from com.l / parse.l,
;;; in non-interactive form. Turns a sentence STRING into the
;;; *wstring* of word-nodes the parser consumes, then runs the
;;; wait-and-see loop.
;;;
;;; What this covers:
;;;   nodify, nodify*, wordify  (com.l 281-310) -- word -> buffer node
;;;   tokenize, read-sentence   -- the non-interactive replacement for
;;;                                com.l's interactive `sentin' (12-83)
;;;   reset-parser-state, parse-string  -- the `parse' driver
;;;                                (parse.l 103) adapted to a string
;;;
;;; Deviations:
;;;
;;; - No TTY. Marcus's `sentin' is a character-at-a-time terminal line
;;;   editor (tyi/cursorpos/rubout/$c/$s). We read a whole string and
;;;   tokenize it. `tokenize' also peels trailing punctuation into its
;;;   own tokens, standing in for the character-level `*nextmorph*'
;;;   re-feed `sentin' uses to split a word from its punctuation.
;;;
;;; - Full-sentence parsing of the shipped grammars (gram1.l ...) needs
;;;   the case-frame machinery (case.l) and the NP/clause rules in
;;;   gram2.l/gram3.l, neither yet ported. `parse-string' itself is
;;;   grammar-agnostic: give it a loaded grammar's initial rule and it
;;;   drives the loop. The end-to-end test uses a small self-contained
;;;   grammar to exercise the whole string -> parse pipeline.

(in-package :parsifal)


(defvar *ordercounter* 0
  "Input-position counter stamped onto each word-node (com.l :ordercounter).")


;;; ===========================================================
;;; Word -> node (com.l 281-310)
;;; ===========================================================

(defun nodify (word)
  "Build a buffer node (a (SYMBOL . FLAGS) cons) for the canonical
   WORD: its feature list becomes the node symbol's value, and its
   markers / special registers / input position go on the plist.
   Mirrors com.l 286.

   NOTE: faithful to the source, the fresh node's `daughters' register
   is initialised to the symbol WORD (`(list 'daughters 'word ...)').
   Leaf word-nodes don't normally become attachment parents, so this
   placeholder is inert; flagged in case it ever matters."
  (let ((node (makesym 'word)))
    (setf (symbol-plist node)
          (list 'daughters 'word
                'word      word
                'inputpos  (incf *ordercounter*)
                'markers   (get word 'markers)))
    (setf (symbol-value node) (get word 'features))
    (mapc (lambda (regtype)
            (let ((v (get word regtype)))
              (when v (setf (get node regtype) v))))
          *specregs*)
    (cons node 0)))

(defun nodify* (wstring)
  "Turn a list of canonical words (WSTRING) into buffer nodes,
   collapsing any multi-token phrase registered in the word-tree into
   its single canonical word along the way. Mirrors com.l 304."
  (do ((wtail wstring (cdr wtail))
       (result nil (cons (car wtail) result))
       (temp))
      ((null wtail) (mapcar #'nodify (nreverse result)))
    (when (setq temp (get-string wtail *wstring-tree*))
      (expandsim (car temp))
      (setq wtail temp))))

(defun wordify (word)
  "Build a single node for WORD directly (expanding it first). Mirrors
   com.l 281."
  (setf (get word 'origword) word)
  (expandsim word)
  (let ((*ordercounter* 1000))
    (nodify word)))


;;; ===========================================================
;;; Tokenizing + sentence reading (non-interactive sentin)
;;; ===========================================================

(defun punctuation-char-p (ch)
  (member ch '(#\. #\, #\? #\! #\; #\:)))

(defun whitespacep (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return #\Linefeed #\Page)))

(defun tokenize (string)
  "Split STRING into tokens on whitespace, peeling any trailing
   punctuation characters off into their own tokens (so `cats.' ->
   `cats' `.'). This stands in for sentin's character-level
   *nextmorph* splitting."
  (let ((tokens nil)
        (run    (make-string-output-stream)))
    (flet ((flush ()
             (let ((s (get-output-stream-string run)))
               (when (plusp (length s)) (push s tokens)))))
      (loop for ch across string do
        (cond ((whitespacep ch) (flush))
              ((punctuation-char-p ch)
               (flush)
               (push (string ch) tokens))
              (t (write-char ch run))))
      (flush))
    (nreverse tokens)))

(defvar *warn-unknown-words* t
  "When non-nil, READ-SENTENCE warns whenever it drops a token MORPHO
   cannot resolve (a word missing from the lexicon). Bind to NIL to
   silence -- e.g. when a test deliberately parses an out-of-vocabulary
   sentence.")

(define-condition unknown-words-warning (warning)
  ((words :initarg :words :reader unknown-words-of))
  (:report (lambda (c stream)
             ;; Name a FULL path, and the right file. A bare
             ;; "system/core/runtime/supplement.dict" leaves you guessing
             ;; which checkout it means, and sends you into this repository
             ;; even when you have a project lexicon of your own -- which is
             ;; where your corpus's vocabulary belongs. See LOAD-USER-LEXICON.
             (format stream
                     "read-sentence dropped unknown word(s) not in the ~
                      lexicon: ~{~a~^ ~}. Add them to ~a."
                     (unknown-words-of c)
                     (or (car (last *user-lexicon-files*))
                         (ignore-errors
                          (asdf:system-relative-pathname
                           :parsifal "system/core/runtime/supplement.dict"))
                         "parsifal's system/core/runtime/supplement.dict"))))
  (:documentation
   "Signalled by READ-SENTENCE when it drops out-of-vocabulary tokens.
    A dedicated WARNING subclass (not a SIMPLE-WARNING) so it survives a
    `(setf sb-ext:*muffled-warnings* 'simple-warning)' in the user's
    init file -- a dropped word is the usual reason a grammatical
    sentence won't parse, so this should be seen."))

(defun unknown-words (string)
  "The tokens in STRING that MORPHO cannot resolve -- i.e. words the
   lexicon doesn't have, which READ-SENTENCE silently drops (so the parse
   usually fails for lack of them). NIL means every token is known. Use
   this to tell a real grammar gap from a missing-word failure; add any
   words it reports to system/core/runtime/supplement.dict."
  (loop for tok in (tokenize string)
        unless (morpho (reverse (map 'list #'char-code tok)))
          collect tok))

(defun read-sentence (string)
  "Non-interactive `sentin' (com.l 12): tokenize STRING, run MORPHO on
   each token to get its canonical word, then build *wstring* via
   NODIFY*. Tokens MORPHO can't resolve are dropped (and, unless
   *WARN-UNKNOWN-WORDS* is NIL, warned about -- a dropped word is the
   usual reason an otherwise-grammatical sentence fails to parse).
   Returns (and sets) *wstring*. Sets *sent* to the canonical word list."
  (let ((sent nil) (dropped nil))
    (dolist (tok (tokenize string))
      (let ((chars (reverse (map 'list #'char-code tok))))
        (if (morpho chars)
            (push *wrd* sent)
            (push tok dropped))))
    (when (and *warn-unknown-words* dropped)
      (warn 'unknown-words-warning :words (nreverse dropped)))
    (setq *sent* (nreverse sent))
    (setq *ordercounter* 0)
    (setq *wstring* (nodify* *sent*))))


;;; ===========================================================
;;; Parse driver (parse.l 103, adapted)
;;; ===========================================================

(defun reset-parser-state ()
  "Bring the per-parse globals to a clean baseline before a parse.
   The startup half of Marcus's `parse' (parse.l 104-140), minus the
   TTY / timing chatter."
  (setq *activepackets*  nil
        *activerule*     nil
        *activenodestak* nil
        *nextrule*       nil
        *parsecomplete*  nil
        *deriv*          nil
        *bufpntr*        0
        *bufmax*        -1
        *bufpntrstak*    nil
        *current-s*      nil
        *wh-comp*        nil
        s                nil
        c                nil
        nth              nil
        |1ST| nil |2ND| nil |3RD| nil
        *1stfeat* nil *2ndfeat* nil *3rdfeat* nil)
  (unless (arrayp *buffer*)
    (setq *buffer* (make-array 10 :initial-element nil)))
  (dotimes (i (length *buffer*)) (setf (aref *buffer* i) nil))
  ;; Zero the three buffer-position feature vectors. SETUP** clears a
  ;; position's old bits using the cached *NthFEAT* index list, so niling
  ;; *NthFEAT* above without also clearing the bit array would break the
  ;; matcher's invariant: stale feature bits from a previous parse (e.g. a
  ;; `verb' left in 2nd position) would survive and spuriously satisfy a
  ;; later `[=verb]' pattern. Re-zeroing keeps *NthFEAT*=nil consistent with
  ;; an all-zero *NthFVEC*, so a fresh PARSE-SENTENCE in a reused image
  ;; behaves like the first one. (No effect on the first parse -- the arrays
  ;; are allocated zeroed.)
  (dolist (fvec (list *1stfvec* *2ndfvec* *3rdfvec*))
    (when (typep fvec '(array bit)) (fill fvec 0)))
  ;; Close any frame left open by a previous parse and clear the cached
  ;; case-frame specials (hypo-slots / pred / objs-needed), so thematic-role
  ;; filling starts clean too. (OPENFRAME / CLEARCF live in case-frame.lisp,
  ;; loaded after this file; the call resolves at run time.)
  (openframe nil))

(defun parse-sentence (string &key (initial-rule 'initial-rule))
  "Parse a sentence STRING end to end: reset state, read it into
   *wstring*, install the grammar's INITIAL-RULE as the active rule,
   and run the wait-and-see loop. Returns what PARSE-LOOP returns (T on
   success). The grammar (and INITIAL-RULE) must already be loaded via
   the rule compiler. Mirrors the shape of Marcus's `parse' (parse.l
   103) with the interactive `(sentin)' replaced by READ-SENTENCE.

   Named `parse-sentence' rather than `parse-string' because glang-cl
   (which :uses :parsifal) defines its own `parse-string'; an exported
   collision would let its definition clobber this one."
  (reset-parser-state)
  (read-sentence string)
  (setq *activerule* (list initial-rule (act-of-rule initial-rule)))
  (parse-loop))
