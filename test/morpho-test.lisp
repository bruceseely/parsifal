;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-

(in-package :parsifal)

;;; Tests for system/core/runtime/morpho.lisp.
;;;
;;; (morpho-test)   -> t if all pass, nil otherwise
;;; (morpho-test t) -> verbose
;;;
;;; morpho takes a word's characters as CODES in REVERSE order (the way
;;; sentin collects them); REV builds that from a string. Each test
;;; defines the roots it needs in the lexicon first.


(defun morpho-test (&optional verbose)
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
               (setf results (and pass results))))
           (rev (s) (reverse (map 'list #'char-code s))))

      ;; A small lexicon for the cases below.
      (dolist (w '(cat run walk fast quick do))
        (setf (symbol-plist w) nil))
      (df cat   feats (noun ns))
      (df run   feats (verb tnsless))
      (df walk  feats (verb tnsless))
      (df fast  feats (adj))
      (df quick feats (adj))
      (df do    feats (verb tnsless))
      (dolist (w '(cat run walk fast quick do)) (expandsim w))

      ;; --- direct hit ------------------------------------------------

      (truthy "morpho resolves a known root directly" (morpho (rev "cat")))
      (check  "morpho sets *wrd* to the canonical word" *wrd* 'cat)

      ;; --- plural (-s) -----------------------------------------------

      (truthy "morpho resolves a plural" (morpho (rev "cats")))
      (check  "plural surface word is *wrd*" *wrd* 'cats)
      (check  "plural records its root" (get 'cats 'root) 'cat)
      (truthy "plural carries npl" (member 'npl (get 'cats 'features)))
      (check  "plural drops ns" (member 'ns (get 'cats 'features)) nil)

      ;; --- -ing with consonant doubling (running -> run) -------------

      (truthy "morpho undoes consonant doubling for -ing"
              (morpho (rev "running")))
      (check  "-ing root is run" (get 'running 'root) 'run)
      (truthy "-ing carries ing" (member 'ing (get 'running 'features)))

      ;; --- -ed (walked -> walk) --------------------------------------

      (truthy "morpho resolves a past tense" (morpho (rev "walked")))
      (check  "-ed root is walk" (get 'walked 'root) 'walk)
      (truthy "-ed carries past" (member 'past (get 'walked 'features)))

      ;; --- comparative (-er) -----------------------------------------

      (truthy "morpho resolves a comparative" (morpho (rev "faster")))
      (check  "-er root is fast" (get 'faster 'root) 'fast)
      (truthy "-er carries comp" (member 'comp (get 'faster 'features)))

      ;; --- adverb (-ly) ----------------------------------------------
      ;; This exercises morpho's -ly DECOMPOSITION (quickly -> quick + adv),
      ;; so `quickly' must not be a direct lexicon entry -- otherwise the
      ;; direct-hit path short-circuits before stripping. supplement.dict
      ;; now defines `quickly' as a manner adverb, so clear that plist for
      ;; the duration of this check and restore it afterward.
      (let ((saved-plist (symbol-plist 'quickly)))
        (setf (symbol-plist 'quickly) nil)
        (truthy "morpho resolves an -ly adverb" (morpho (rev "quickly")))
        (check  "-ly root is quick" (get 'quickly 'root) 'quick)
        (truthy "-ly carries adv" (member 'adv (get 'quickly 'features)))
        (setf (symbol-plist 'quickly) saved-plist))

      ;; --- contraction (don't -> do, stripping n't) ------------------

      (truthy "morpho strips a contraction" (morpho (rev "don't")))
      (check  "contraction resolves to its base word" *wrd* 'do)

      ;; --- ordinal numeral (5th) -------------------------------------

      (truthy "morpho resolves an ordinal numeral" (morpho (rev "5th")))
      (truthy "ordinal carries ord" (member 'ord (get *wrd* 'features)))

      ;; --- failure ---------------------------------------------------

      (check "morpho returns NIL for an unknown word" (morpho (rev "qxz")) nil)

      ;; --- a bare doubled consonant is not an inflection -------------
      ;; Marcus undid the doubling even with no ending stripped, so an
      ;; unknown `catt' came back as `cat' -- and `ann' as the article `an'.
      ;; The doubling is still undone when an ending was stripped (running,
      ;; above).

      (check "a bare word ending in a doubled consonant is not shortened"
             (morpho (rev "catt")) nil)
      (truthy "but a doubled consonant before an ending still is"
              (morpho (rev "catting")))

      ;; --- an inflected form follows its root's redefinition ---------
      ;; MORPHO builds `zooms' with a COPY of zoom's case-frame; redefining
      ;; zoom (a re-read project lexicon) used to leave `zooms' on the old
      ;; frame for the rest of the session.

      (dolist (w '(zoom zooms)) (setf (symbol-plist w) nil))
      (df zoom feats (mainverb pres tnsless v-3s) cf (neut agt))
      (truthy "morpho builds an inflected verb" (morpho (rev "zooms")))
      (check  "  with its root's case-frame"
              (get 'zooms 'case-frame) '((neut oblig) (agt oblig)))
      (df zoom feats (mainverb pres tnsless v-3s) cf (ins agt))
      (truthy "after the root is redefined, morpho resolves it again"
              (morpho (rev "zooms")))
      (check  "  on the root's new case-frame"
              (get 'zooms 'case-frame) '((ins oblig) (agt oblig)))

      ;; --- origcase --------------------------------------------------

      (setf (get 'tw-oc 'origword) 'orig-cat)
      (check "origcase reads the stored original form"
             (origcase 'tw-oc)
             'orig-cat)
      (check "origcase maps over a list"
             (origcase '(tw-oc tw-oc))
             '(orig-cat orig-cat)))

    results))
