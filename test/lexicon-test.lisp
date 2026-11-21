;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-

(in-package :parsifal)

;;; Tests for system/core/runtime/lexicon.lisp.
;;;
;;; (lexicon-test)   -> t if all pass, nil otherwise
;;; (lexicon-test t) -> verbose
;;;
;;; Word definitions live on symbol plists (global), so each block
;;; clears the plists of the test words it uses; the word-tree blocks
;;; rebind *wstring-tree* / *wstring-list* for isolation.


(defun lexicon-test (&optional verbose)
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
               (setf results (and pass results)))))

      ;; --- word-tree (get-string / put-string) -----------------------

      (let ((*wstring-tree* (make-symbol "T"))
            (*wstring-list* nil))
        (put-string (explodec 'cat) *wstring-tree* 'cat-word)
        (check "get-string returns longest match + remaining chars"
               (get-string (append (explodec 'cat) (explodec 's))
                           *wstring-tree*)
               (cons 'cat-word (explodec 's)))
        (check "get-string returns NIL when no word ends on the path"
               (get-string (explodec 'ca) *wstring-tree*)
               nil))

      ;; --- df / df1 --------------------------------------------------

      (setf (symbol-plist 'tw-run) nil)
      (df tw-run feats (verb tnsless) cf ((agt) (obj)))
      (check "df stores feats" (get 'tw-run 'feats) '(verb tnsless))
      (check "df stores cf"    (get 'tw-run 'cf)    '((agt) (obj)))

      (setf (symbol-plist 'tw-disabled) nil)
      (df1 tw-disabled feats (verb tnsless))
      (check "df1 is a no-op (defines nothing)"
             (get 'tw-disabled 'feats)
             nil)

      ;; --- expandsim / expanddef on a `feats' word -------------------

      ;; `(redund verb (pres past future tnsless))' means a TENSED word
      ;; is a verb (each tense implies verb), so a `(mainverb past v-3s)'
      ;; word closes up to include `verb'.
      (setf (symbol-plist 'tw-sit) nil)
      (df tw-sit feats (mainverb past v-3s))
      (truthy "expandsim succeeds on a feats word" (expandsim 'tw-sit))
      (check "expanddef makes the *WORD canonical the first feature"
             (car (get 'tw-sit 'features))
             (implode (cons '* (explodec 'tw-sit))))
      (truthy "expanddef keeps the declared features"
              (subsetp '(mainverb past v-3s) (get 'tw-sit 'features)))
      (truthy "expanddef closes under redund (past -> verb)"
              (member 'verb (get 'tw-sit 'features)))
      (check "expanddef sets type to the part of speech"
             (get 'tw-sit 'type)
             'verb)
      (check "expanddef consumes feats (now expanded)"
             (get 'tw-sit 'feats)
             nil)

      ;; --- add-redunds directly --------------------------------------

      (truthy "add-redunds: a tense feature implies verb (past -> verb)"
              (member 'verb (add-redunds '(past))))
      (truthy "add-redunds: det implies ngstart"
              (member 'ngstart (add-redunds '(det))))

      ;; --- buildnumber -----------------------------------------------

      (setf (symbol-plist 'tw-30) nil)
      (buildnumber 'tw-30 30)
      (truthy "buildnumber: 30 is a tens word"
              (member 'tens (get 'tw-30 'features)))
      (truthy "buildnumber: 30 is npl (not singular)"
              (member 'npl (get 'tw-30 'features)))
      (check "buildnumber: quant register holds the value"
             (get 'tw-30 'quant)
             30)
      (setf (symbol-plist 'tw-1) nil)
      (buildnumber 'tw-1 1)
      (truthy "buildnumber: 1 is ones + ns (singular)"
              (and (member 'ones (get 'tw-1 'features))
                   (member 'ns   (get 'tw-1 'features))))

      ;; --- jlike inheritance via expandsim ---------------------------

      (setf (symbol-plist 'tw-model) nil
            (symbol-plist 'tw-like)  nil)
      (df tw-model feats (noun ns))
      (expandsim 'tw-model)
      (jlike tw-like tw-model)
      (truthy "expandsim succeeds on a jlike word" (expandsim 'tw-like))
      (truthy "jlike word inherits the model's substantive features"
              (member 'noun (get 'tw-like 'features)))
      (truthy "jlike word gets its OWN *WORD canonical feature"
              (member (implode (cons '* (explodec 'tw-like)))
                      (get 'tw-like 'features)))
      (check "jlike word does NOT inherit the model's *WORD canonical"
             (member (implode (cons '* (explodec 'tw-model)))
                     (get 'tw-like 'features))
             nil)

      ;; --- df+ multi-token phrase ------------------------------------

      (let ((*wstring-tree* (make-symbol "T"))
            (*wstring-list* nil))
        (setf (symbol-plist (intern "NEW YORK" :parsifal)) nil)
        (df+ |new york| feats (noun ns name))
        (check "df+ registers the phrase under its token path"
               (car (get-string (list (intern "NEW" :parsifal)
                                       (intern "YORK" :parsifal))
                                 *wstring-tree*))
               (intern "NEW YORK" :parsifal))
        (check "df+ then DFs the canonical word with the rest"
               (get (intern "NEW YORK" :parsifal) 'feats)
               '(noun ns name))
        (truthy "df+ records the phrase tokens in *wstring-list*"
                (and (member (intern "NEW" :parsifal) *wstring-list*)
                     (member (intern "YORK" :parsifal) *wstring-list*))))

      ;; --- redefinition reaches the words built from a word ----------
      ;; An irregular form and a jlike word copy their root's case-frame
      ;; when built, and EXPANDSIM never rebuilt a word that had features --
      ;; so redefining the root (a re-read project lexicon) left them on
      ;; the old frame.

      (dolist (w '(tw-go tw-went tw-trek tw-acc))
        (setf (symbol-plist w) nil))
      (df tw-go feats (mainverb tnsless) cf (neut agt) neut (|#| hanim |#|))
      (df tw-went irreg (tw-go (past) (tnsless)))
      (jlike tw-trek tw-go)
      (expandsim 'tw-went)
      (expandsim 'tw-trek)
      (check "an irregular form copies its root's case-frame"
             (get 'tw-went 'case-frame) '((neut oblig) (agt oblig)))
      (check "so does a jlike word"
             (get 'tw-trek 'case-frame) '((neut oblig) (agt oblig)))
      (check "the root records the words built from it"
             (sort (copy-list (get 'tw-go 'derived-words)) #'string< :key #'symbol-name)
             '(tw-trek tw-went))

      (df tw-go feats (mainverb tnsless) cf (ins agt))
      (check "redefining the root forgets the irregular form's copy"
             (get 'tw-went 'features) nil)
      (check "  and the jlike word's" (get 'tw-trek 'features) nil)
      (check "  but keeps their own definitions"
             (list (car (get 'tw-went 'irreg)) (get 'tw-trek 'jlike))
             '(tw-go tw-go))
      (expandsim 'tw-went)
      (expandsim 'tw-trek)
      (check "the irregular form is rebuilt on the new frame"
             (get 'tw-went 'case-frame) '((ins oblig) (agt oblig)))
      (check "  and the jlike word"
             (get 'tw-trek 'case-frame) '((ins oblig) (agt oblig)))
      (check "the root's case-frame is the new one"
             (get 'tw-go 'case-frame) '((ins oblig) (agt oblig)))
      (check "a property deleted from the entry is gone, not left over"
             (get 'tw-go 'neut) nil)

      (df tw-went irreg (tw-go (past vspl) (tnsless)))
      (check "redefining a built word itself forgets its build"
             (get 'tw-went 'features) nil)
      (expandsim 'tw-went)
      (truthy "  and it is rebuilt from its new definition"
              (member 'vspl (get 'tw-went 'features)))

      ;; Before any expansion, definitions still accumulate: Marcus's
      ;; dictionary defines `time' as a markerset and then as a noun.
      (df tw-acc markerset (|#| time |#|))
      (df tw-acc feats (noun ns))
      (check "unexpanded definitions accumulate, as in Marcus"
             (list (get 'tw-acc 'markerset) (get 'tw-acc 'feats))
             '((|#| time |#|) (noun ns))))

    results))
