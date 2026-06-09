;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/core/runtime/lexicon.lisp
;;;
;;; The lexicon-loading half of com.l: the word-tree, the definer
;;; forms the dictionary (defs.l 85-876) is written in, and the
;;; definition-expansion machinery that turns a word's stored
;;; properties into the feature list / markers / case-frame the parser
;;; reads. No TTY input and no morphology yet (those are the next
;;; com.l increments).
;;;
;;; What this file covers (com.l line numbers in comments):
;;;   word-tree        *wstring-tree*, *wstring-list*, get-string,
;;;                    put-string
;;;   definers         df, df1, df+, jlike, abbrev   (MacLISP fexprs,
;;;                    ported as macros that quote their literal args)
;;;   word building    buildword, buildnumber, buildirregword,
;;;                    mod-features
;;;   expansion        expandsim, expanddef, expandm, add-redunds,
;;;                    expandcf, modcasef, after
;;;
;;; Deviations worth knowing:
;;;
;;; - Case. CL's reader interns unescaped symbols UPPER, so the lexicon
;;;   source `(df run ...)' yields RUN. `lowcaseify' therefore folds to
;;;   upper (Marcus folds to lower) so symbols rebuilt from raw input
;;;   characters stay EQ to the reader-built ones. The whole runtime is
;;;   thus uppercase-canonical, consistent with the feature symbols
;;;   defs.lisp already interns.
;;;
;;; - `redund'. defs.lisp already ports REDUND as a macro that records
;;;   feature implications in *REDUND-TABLE*. com.l's `redund' fexpr
;;;   stored them on each feature's :redunds plist, which `add-redunds'
;;;   read. We keep the *REDUND-TABLE* mechanism and point `add-redunds'
;;;   at it; com.l's duplicate `redund' is dropped.
;;;
;;; - The word-tree node. Marcus uses a `(ncons nil)' cons as a
;;;   "disembodied plist" -- car holds the word that ends here, the cdr
;;;   is a char->subtree plist. CL's GET needs a symbol, so each node
;;;   is a gensym (as ATTACH does for `daughters'): children live on
;;;   its plist keyed by char-symbol, the terminal word under :tree-word.
;;;
;;; - `mod' (com.l 534) is renamed `mod-features' -- the MacLISP name
;;;   shadows CL:MOD.
;;;
;;; - `modcasef'/`after' need a `*caseorder*' (the standard case
;;;   ordering) that no delivered source sets; until case.l/defs supply
;;;   it, *caseorder* is NIL and modcasef's ordering is degenerate
;;;   (added cases land at the end). Only the rare `cases' override
;;;   path in expandsim uses it.

(in-package :parsifal)


;;; ===========================================================
;;; Case folding + digit class (needed by df+/expandsim)
;;; ===========================================================

(defun lowcaseify (code)
  "Fold character CODE to one canonical case and return it as a
   one-char symbol (via ASCII). Marcus (com.l 221) folds to lower; we
   fold to UPPER to match CL's reader case (see file header)."
  (ascii (if (<= (char-code #\a) code (char-code #\z))
             (- code 32)
             code)))

(defvar *numbers*
  (map 'list (lambda (ch) (intern (string ch) :parsifal)) "0123456789")
  "Digit char-symbols, used to recognize numeric words. The other
   char-class lists (*vowels* etc.) arrive with morpho.")

(defvar *always-expand* nil
  "Marcus's :always-expand (parse.l 35) -- forces re-expansion of
   `jlike'/`irreg' words on every lookup. Off by default.")

(defvar *caseorder* nil
  "Standard ordering of case-frame slots, consulted by MODCASEF/AFTER.
   No delivered source sets it; NIL gives degenerate ordering until
   case.l/defs supply the real order.")


;;; ===========================================================
;;; Order-preserving set ops on feature lists
;;; ===========================================================
;;; Marcus's union1/setminus (from util/set) preserve element order;
;;; CL's UNION/SET-DIFFERENCE don't. Features occasionally care about
;;; order, so use these stable helpers (per the macros2.l disposition).

(defun feat-minus (a b)
  "A with every element of B removed, order preserved."
  (remove-if (lambda (x) (member x b)) a))

(defun feat-union (a b)
  "A followed by the elements of B not already in A."
  (append a (remove-if (lambda (x) (member x a)) b)))


;;; ===========================================================
;;; Word-tree (com.l 245-260)
;;; ===========================================================

(defvar *wstring-list* nil
  "Flat list of every word/token registered in the word-tree.")

(defun make-wtree () (make-symbol "WTREE"))

(defvar *wstring-tree* (make-wtree)
  "Root of the word-tree trie (a gensym; see file header).")

(defun reset-lexicon ()
  "Forget the word-tree (not the per-symbol word definitions). Useful
   between test scenarios and grammar reloads."
  (setf *wstring-tree* (make-wtree)
        *wstring-list* nil))

(defun get-string (string tree)
  "Find the longest word in TREE that is a prefix of STRING (a list of
   char-symbols). Returns (WORD . REMAINING-STRING) or NIL. Mirrors
   com.l 249."
  (cond ((null tree) nil)
        ((get-string (cdr string) (get tree (car string))))
        ((get tree :tree-word) (cons (get tree :tree-word) string))))

(defun put-string (string tree word)
  "Register WORD in TREE under the path STRING (a list of char-symbols).
   Mirrors com.l 254."
  (cond ((null string) (setf (get tree :tree-word) word))
        (t (let ((child (or (get tree (car string))
                            (setf (get tree (car string)) (make-wtree)))))
             (put-string (cdr string) child word)))))


;;; ===========================================================
;;; Definition expansion (com.l 388-537)
;;; ===========================================================

(defun expandm (prop)
  "Expand an abbreviation: if PROP is a symbol with an :abbrev, return
   that; otherwise PROP itself. Mirrors com.l 527."
  (or (and (symbolp prop) (get prop :abbrev))
      prop))

(defun add-redunds (feats)
  "Close FEATS under the redundant-feature implications (parse.l's
   REDUND declarations). Marcus reads each feature's :redunds plist
   (com.l 506); we read *REDUND-TABLE* (populated by defs.lisp)."
  (do ((fset feats)
       (newfs feats
              (do ((fs newfs (cdr fs)) (result))
                  ((null fs) result)
                (mapc (lambda (f)
                        (unless (member f fset)
                          (push f result)
                          (push f fset)))
                      (gethash (car fs) *redund-table*)))))
      ((null newfs) fset)))

(defun expandcf (cf)
  "Normalize a case-frame: a bare case becomes (CASE OBLIG), a
   one-element case (CASE) becomes (CASE OPT), a full case is left
   alone. Mirrors com.l 519."
  (mapcar (lambda (cs)
            (cond ((null cs) cs)
                  ((atom cs) (list cs 'oblig))
                  ((null (cdr cs)) (list (car cs) 'opt))
                  (t cs)))
          cf))

(defun after (case1 case2 standard)
  "T if CASE2 does not come at-or-after CASE1 in STANDARD. Mirrors
   com.l 490."
  (not (member case2 (member case1 standard))))

(defun modcasef (basecf addcs remcs)
  "Splice ADDCS into BASECF in *caseorder* order, dropping any case in
   REMCS. Mirrors com.l 477. With *caseorder* NIL the ordering is
   degenerate (added cases land at the end)."
  (let (result)
    (mapc (lambda (cs)
            (do () ((after (caar addcs) (car cs) *caseorder*))
              (push (car addcs) result)
              (setq addcs (cdr addcs)))
            (unless (member (car cs) remcs)
              (push cs result)))
          basecf)
    (append (nreverse result) addcs)))

(defun expanddef (word)
  "Expand WORD's stored `feats'/`markers'/`cf'/`neut'/`preps' into the
   parser-facing `features'/`markers'/`case-frame'/... properties.
   Mirrors com.l 388."
  (block expanddef
    (let (temp)
      (when (and (get word 'features) (not (get word 'feats)))
        (return-from expanddef t))
      (when (setq temp (get word 'markers))
        (setf (get word 'markers) (add-redunds (expandm temp))))
      (when (setq temp (get word 'feats))
        (let ((features (cons (implode (cons '* (explodec word)))
                              (add-redunds (expandm temp)))))
          (setf (get word 'features) features)
          (let ((poses (intersection features *parts-of-speech*)))
            (when (and (null poses) *carefulsw*)
              (warn "~a is not a known part of speech." word))
            (setf (get word 'type) (car poses)))))
      (when (setq temp (get word 'cf))
        (setf (get word 'case-frame) (expandcf (expandm temp))))
      (remprop word 'feats)
      (remprop word 'cf)
      (when (setq temp (get word 'neut))
        (setf (get word 'neut) (expandm temp)))
      (when (setq temp (get word 'preps))
        (setf (get word 'preps) (cons nil temp)))
      t)))

(defun expandsim (word)
  "Ensure WORD has expanded `features' etc., building them on demand
   from a `feats' definition, an `irreg' form, a `jlike' similar word
   (inheriting its features/markers/registers, then applying `except'
   overrides), or numeric recognition. Returns T on success, NIL if
   WORD can't be resolved. Mirrors com.l 416."
  (block expandsim
    (let (simw temp)
      (cond
        ((and *always-expand*
              (or (and (setq simw (get word 'jlike)) (expandsim simw))
                  (and (get word 'irreg)
                       (buildirregword word) (return-from expandsim t))
                  (and (get word 'root) (return-from expandsim nil)))))
        ((get word 'feats) (expanddef word) (return-from expandsim t))
        ((get word 'features) (return-from expandsim t))
        ((get word 'irreg) (buildirregword word) (return-from expandsim t))
        ((and (setq simw (get word 'jlike)) (expandsim simw)))
        ((and (member (getchar word (flatc word)) *numbers*)
              (setq temp (readlist (explodec word)))
              (numberp temp)
              (buildnumber word temp)
              (return-from expandsim t)))
        (t (return-from expandsim nil)))
      ;; Fell through the COND: WORD is `jlike' SIMW -- inherit.
      (setf (get word 'expanded) t)
      (setf (get word 'features)
            (cons (implode (cons '* (explodec word)))
                  (feat-minus (get simw 'features)
                              (list (implode (cons '* (explodec simw)))))))
      (setf (get word 'markers) (get simw 'markers))
      (mapc (lambda (regtype)
              (let ((v (get simw regtype)))
                (when v (setf (get word regtype) v))))
            *specregs*)
      ;; Apply per-word `except' overrides.
      (do ((plist (get word 'except) (cddr plist))
           (prop) (value))
          ((null plist))
        (setq prop (car plist))
        (cond
          ((eq prop 'feats)
           (setq value (cadr plist))
           (setf (get word 'features)
                 (if (eq (cadr value) 'all)
                     (car value)
                     (feat-minus (feat-union (car value) (get word 'features))
                                 (cadr value)))))
          ((eq prop 'cf)
           (setf (get word 'case-frame) (expandcf (expandm (cadr plist)))))
          ((eq prop 'cases)
           (setq value (cadr plist))
           (setf (get word 'case-frame)
                 (modcasef (get simw 'case-frame) (car value) (cadr value))))
          (t (setf (get word prop) (cadr plist)))))
      t)))


;;; ===========================================================
;;; Word building (com.l 320-358, 534)
;;; ===========================================================

(defun mod-features (a b)
  "Modify feature list A by spec B = (ADDS REMOVES): drop REMOVES, add
   ADDS. The `ed=en' marker pulls in `en part' when B adds `ed'.
   Mirrors com.l 534 (`mod', renamed to avoid shadowing CL:MOD)."
  (when (and (member 'ed=en a) (member 'ed (car b)))
    (setq a (append a '(en part))))
  (feat-union (feat-minus a (cadr b)) (car b)))

(defun buildword (word feat markers root)
  "Define WORD with the given FEAT/MARKERS, inheriting ROOT's special
   registers. Mirrors com.l 324."
  (expandsim root)
  (setf (get word 'features) feat
        (get word 'markers)  markers
        (get word 'root)     root)
  (mapc (lambda (regtype)
          (let ((v (get root regtype)))
            (when v (setf (get word regtype) v))))
        *specregs*))

(defun buildnumber (numword number)
  "Define NUMWORD as the numeral NUMBER, computing its size feature
   (ones/tens/99s/...) and number features. Mirrors com.l 334."
  (buildword numword
             (cons (cond ((< number 10) 'ones)
                         ((zerop (mod number 10)) 'tens)
                         ((= number 100) '*hundred)
                         ((< number 100) '99s)
                         ((< number 1000) '999s))
                   (cons (if (= number 1) 'ns 'npl)
                         (add-redunds '(1234num num adj))))
             nil
             nil)
  (setf (get numword 'quant) number))

(defun buildirregword (word)
  "Build an irregular form from its `irreg' = (ROOT add-feats rem-feats)
   spec, inheriting ROOT's features/markers. Mirrors com.l 349."
  (let* ((irregl (get word 'irreg))
         (root   (car irregl)))
    (expandsim root)
    (buildword word
               (mod-features (get root 'features) (cdr irregl))
               (get root 'markers)
               root)
    word))


;;; ===========================================================
;;; Definers (com.l 263, 360-384, 530) -- MacLISP fexprs
;;; ===========================================================
;;; Ported as macros that quote their unevaluated argument list and
;;; hand it to a worker function (fexpr semantics in CL).

(defun %df (l)
  "Worker for DF: walk the (PROP VALUE)* tail of L, storing each on the
   word (car L). Mirrors com.l 360."
  (do ((word (car l))
       (plist (cdr l) (cddr plist)))
      ((null plist) word)
    (setf (get word (car plist)) (cadr plist))))

(defmacro df (&rest l)
  "Define a word: (df WORD PROP VALUE PROP VALUE ...)."
  `(%df ',l))

(defmacro df1 (&rest l)
  "A disabled definition -- Marcus comments a `df' out by writing `df1'.
   Expands to NIL (com.l 364)."
  (declare (ignore l))
  nil)

(defun %df+ (l)
  "Worker for DF+: register the multi-token phrase (car L) in the
   word-tree under a canonical word, then DF that word with the rest.
   Mirrors com.l 263."
  (let* ((wlist     (mapcar #'lowcaseify (exploden (car l))))
         (canonword (implode wlist))
         (wlist1    (readlist (append '(|(|) wlist '(|)|))))
         (wlist1    (if (listp wlist1) wlist1 (list wlist1))))
    (put-string wlist1 *wstring-tree* canonword)
    (setq *wstring-list* (feat-union wlist1 *wstring-list*))
    (setf (get canonword 'origword) canonword)
    (%df (cons canonword (cdr l)))))

(defmacro df+ (&rest l)
  "Define a multi-token phrase: (df+ |the phrase| PROP VALUE ...)."
  `(%df+ ',l))

(defun %jlike (l)
  "Worker for JLIKE: mark a word (or word+irregulars) as similar to
   another, optionally with `except' overrides. Mirrors com.l 367."
  (let (nword)
    (cond ((atom (car l))
           (setf (get (setq nword (car l)) 'jlike) (cadr l)))
          (t
           (setf (get (setq nword (caar l)) 'jlike) (caadr l))
           (mapc (lambda (newirreg oirreg)
                   (setf (get newirreg 'irreg)
                         (cons (caar l) (cdr (get oirreg 'irreg)))))
                 (cdar l) (cdadr l))))
    (when (cddr l)
      (setf (get nword 'except) (cddr l)))
    nword))

(defmacro jlike (&rest l)
  "Define a word as inflecting/behaving like another:
   (jlike WORD MODEL) or ((WORD . irregs) (MODEL . irregs) except...)."
  `(%jlike ',l))

(defun %abbrev (l)
  (setf (get (car l) :abbrev) (cadr l)))

(defmacro abbrev (&rest l)
  "Define an abbreviation expanded by EXPANDM: (abbrev NAME EXPANSION)."
  `(%abbrev ',l))
