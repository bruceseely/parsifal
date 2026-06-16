;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-supplement-test.lisp
;;;
;;; The SUPPLEMENTARY lexicon (system/core/runtime/supplement.dict, loaded
;;; by `load-supplement' right after Marcus's verbatim dictionary). It adds
;;; common words Marcus never entered -- without touching his source -- so
;;; everyday example sentences can parse.
;;;
;;; The trigger was "I saw the man with the red hair ." returning NIL: the
;;; parser and grammar were fine (adjectives in an NP and clause-level PPs
;;; both already work), but `hair' was simply absent from the lexicon, so
;;; morpho dropped it and the parse died. `hair' is supplied here as a MASS
;;; noun (feature `massn'), so a determiner-less NP like "red hair" -- were
;;; it startable -- would be a valid `massnp', and bare "hair" parses.
;;;
;;; This test checks (1) the supplement words are registered -- `hair' is a
;;; df'd mass noun (feats include `massn'), while `tree'/`friend' are jlike
;;; pointers (stored lazily, so `(get 'tree 'jlike)' => BLOCK) -- and (2)
;;; the once-failing sentence now parses, with `red'/`hair' sitting in the
;;; PP's NP. (Note Marcus's grammar attaches "with the red hair" at the
;;; CLAUSE level, a sister of the VP, not as a modifier inside the `man' NP
;;; -- high PP attachment. And a bare det-less "red hair" -- adjective-
;;; initial -- is NOT startable: adj is not `ngstart' in Marcus's grammar,
;;; so STARTNP never begins a noun group on the leading `red'.)
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-supplement-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-supplement-test (&optional verbose)
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

      ;; (1) supplement.dict loaded: `hair' is a df'd mass noun; tree/friend
      ;; are jlike pointers (recorded lazily).
      (check "supplement: hair is a mass noun (massn)"
             (and (member 'massn (get 'hair 'feats)) t) t)
      (check "supplement: hair is a noun"
             (and (member 'noun (get 'hair 'feats)) t) t)
      (check "supplement: tree  is jlike block" (get 'tree 'jlike)   'block)
      (check "supplement: friend is jlike boy"  (get 'friend 'jlike) 'boy)

      ;; (2) the once-failing sentence now parses.
      (load-full-grammar)
      (let ((ok (parse-sentence "i saw the man with the red hair ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds: I saw the man with the red hair ." ok t)
        (truthy "top S is a major declarative" (subsetp '(decl major s) (fe c)))
        (check "main verb is `saw'"
               (getr 'word (daughter 'verb (daughter 'vp c))) 'saw)

        ;; "with the red hair" is a clause-level PP (sister of the VP).
        (let ((pp (daughter 'pp c)))
          (truthy "a PP attached at the clause level" pp)
          (check "the PP preposition is `with'"
                 (and pp (getr 'word (daughter 'prep pp))) 'with)
          (let ((np (and pp (daughter 'np pp))))
            (truthy "the PP has an NP object" np)
            (check "the PP-NP adjective is `red'"
                   (and np (getr 'word (daughter 'adj np))) 'red)
            (check "the PP-NP head noun is the supplied word `hair'"
                   (and np (getr 'word (daughter 'noun (daughter 'nbar np))))
                   'hair)))))

    (format t "~&gram1-supplement-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-supplement-test t)
