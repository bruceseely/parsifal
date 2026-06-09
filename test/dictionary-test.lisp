;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-

(in-package :parsifal)

;;; Tests for the defs.l lexicon, loaded from defs-dictionary.dict by
;;; dictionary.lisp's LOAD-DICTIONARY (run at system-load time).
;;;
;;; (dictionary-test)   -> t if all pass, nil otherwise
;;; (dictionary-test t) -> verbose

(defun dictionary-test (&optional verbose)
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

      ;; The lexicon is global mutable state and other suites (e.g.
      ;; morpho-test) destructively redefine shared words like `run' /
      ;; `walk'. Reload the canonical dictionary so this test verifies
      ;; the load result regardless of run order.
      (load-dictionary)

      ;; --- df: a verb's feats / case-frame / markers -----------------
      (check "df go: feats" (get 'go 'feats) '(mainverb pres tnsless v-3s))
      (check "df go: cf"    (get 'go 'cf)    '(nil (pfrom) (pto) agt))
      (check "df go: markers" (get 'go 'markers) 'act)

      ;; --- df: a noun ------------------------------------------------
      (check "df time: feats" (get 'time 'feats) '(noun ns n3p))
      (check "df time: markers" (get 'time 'markers) '(abstr-ob))

      ;; --- jlike: similarity is recorded as a pointer (lazy) ---------
      (check "jlike walk -> go" (get 'walk 'jlike) 'go)
      (check "jlike run  -> go" (get 'run 'jlike)  'go)

      ;; --- irreg: stored as (root add-feats rem-feats) ---------------
      (check "irreg gone" (get 'gone 'irreg) '(go (en) (tnsless v-3s)))
      (check "irreg went" (get 'went 'irreg) '(go (past vspl) (tnsless v-3s pres)))

      ;; --- df+: a multi-word phrase registers its canonical word -----
      (truthy "df+ |pa meetings| registered in the word tree"
              (get-string (list (intern "PA" :parsifal)
                                (intern "MEETINGS" :parsifal))
                          *wstring-tree*))

      ;; --- the `#' marker separator reads as the symbol |#| ----------
      ;; (meet's `neut' restriction is `(# hanim # anim)'.)
      (check "neut markers of `meet' keep the # separators"
             (get 'meet 'neut) '(|#| hanim |#| anim))
      (check "the separator is EQ to the |#| SMQVAL splits on"
             (eq (car (get 'meet 'neut)) '|#|) t)

      ;; --- a bare `:' punctuation word reads as the symbol |:| -------
      ;; (defs.l 756/759: `(jlike - :)' -- `-' is similar to `:'.)
      (check "bare-colon word: `-' is jlike `|:|'"
             (get '- 'jlike) '|:|))

    results))
