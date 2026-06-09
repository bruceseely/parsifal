;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/core/runtime/morpho.lisp
;;;
;;; The morphology engine from com.l: `morpho' takes the characters of
;;; a surface word and finds its canonical entry, stripping inflections
;;; (-s, -ed, -ing, -er, -est, -ly, ordinals, contractions) and undoing
;;; English spelling changes (consonant doubling, final -e, y/i, etc.)
;;; to reconstruct a root it can look up via `expandsim'. On success it
;;; sets *wrd* to the surface word (now carrying the inflected feature
;;; set) and returns T.
;;;
;;; Faithful to com.l, with these notes:
;;;
;;; - Reversed character lists. Marcus's `sentin' collects a word's
;;;   characters in reverse order (newest cons first), and the whole
;;;   algorithm runs on that reversed list: *rt* is the reversed root,
;;;   `ends-in'/`sta' match a suffix against the FRONT of *rt* (= the
;;;   END of the word), and the final lookup is `(implode (reverse ...))'.
;;;   We keep that contract: MORPHO's INPUT is a list of character codes
;;;   in reverse order.
;;;
;;; - Case. The char-class lists below are upper-case symbols; that's
;;;   what `lowcaseify' (which folds to upper, see lexicon.lisp) produces
;;;   and what CL's reader interns. EQ comparisons line up.
;;;
;;; - Three faithful reproductions of what look like errata in Marcus's
;;;   canonical source, flagged inline: the `(member (cdddr *rt*) ...)'
;;;   at the doubled-vowel test (a list is never EQ a vowel), the
;;;   `(get 'features x)' in TRY's irregular branch (apparently a
;;;   transposed `(get x 'features)'), and STRIP-IF-ANY never testing
;;;   its last suffix. Preserved as-is; revisit if Increment 4 needs them.

(in-package :parsifal)


;;; ===========================================================
;;; Character-class globals (com.l 555-572)
;;; ===========================================================
;;; (*numbers* lives in lexicon.lisp; expandsim needs it.)

(defvar *vowels*   '(a e i o u y))
(defvar *consos*   '(b c d f g h j k l m n p q r s t v w x z))
(defvar *liquids*  '(l r s z v))
(defvar *noend*    '(c g s j v z))
(defvar *endpuncs* '(|.| ? !))
(defvar *puncs*    '(|.| |,| ? |:| |;| ! -))


;;; ===========================================================
;;; Shared specials (com.l top `declare')
;;; ===========================================================

(defvar *rt*        nil "Reversed character list of the root being reconstructed.")
(defvar *word*      nil "Reversed character-symbol list of the surface word.")
(defvar *wrd*       nil "The canonical surface word symbol morpho resolved.")
(defvar *modfelist* nil "(ADD-FEATURES REMOVE-FEATURES) accumulated by MODFE.")
(defvar *nextmorph* nil "Suffix peeled off by STRIP-IF, for SENTIN to re-feed.")


;;; ===========================================================
;;; Suffix matching (com.l 217-240, 312, 320)
;;; ===========================================================

(defun origcase (stuff)
  "The original-case form(s) stored under 'origword. Mirrors com.l 217."
  (cond ((atom stuff) (get stuff 'origword))
        (t (mapcar #'origcase stuff))))

(defun sta (l)
  "If *rt* (a reversed char list) ends in suffix L, return
   (MATCHED-SUFFIX . ROOT-REMAINDER); NIL if it doesn't, or if the root
   would be empty (same length as the ending). Mirrors com.l 312."
  (do ((rtail *rt* (cdr rtail))
       (ltail (reverse l) (cdr ltail))
       (tail nil (cons (car ltail) tail)))
      ((null ltail) (when rtail (cons tail rtail)))
    (unless (eq (car rtail) (car ltail)) (return nil))))

(defun ends-in (l)
  "If *rt* ends in suffix L, strip it (set *rt* to the root) and return
   the root; else NIL. Mirrors com.l 226."
  (let ((temp (sta l)))
    (when temp (setq *rt* (cdr temp)))))

(defun strip-if (l)
  "If *rt* ends in L, peel it off: record it in *nextmorph* and set both
   *rt* and *word* to the remainder. Mirrors com.l 236."
  (let ((temp (sta l)))
    (when temp
      (setq *nextmorph* (reverse (car temp)))
      (setq *rt* (setq *word* (cdr temp))))))

(defun strip-if-any (suffixes)
  "Strip the first of SUFFIXES that *rt* ends in. Mirrors com.l 230.
   NOTE: faithful to the source, the DO never tests the LAST element of
   SUFFIXES (it steps past it as the end-test fires)."
  (do ((nxt (car suffixes) (car tail))
       (tail (cdr suffixes) (cdr tail)))
      ((null tail) nil)
    (when (strip-if nxt) (return t))))

(defun modfe (adds removes)
  "Accumulate ADDS / REMOVES into *modfelist*. Mirrors com.l 320."
  (setq *modfelist* (list (feat-union (car *modfelist*) adds)
                          (feat-union (cadr *modfelist*) removes))))

(defun try ()
  "Reconstruct the root from *rt*, look it up, and if it resolves, build
   *wrd* as that root inflected by *modfelist*. Returns T/NIL. Mirrors
   com.l 539."
  (let (x root features)
    (expandsim (setq root (implode (reverse *rt*))))
    (if (setq features
              (or (get root 'features)
                  (and (setq x (get root 'irreg))
                       (setq root (car x))
                       ;; NOTE: com.l 546 has (get 'features x) -- looks
                       ;; like a transposed (get x 'features); preserved.
                       (mod-features (get 'features x) (cdr x)))))
        (progn
          (buildword *wrd*
                     (mod-features features *modfelist*)
                     (get root 'markers)
                     root)
          t)
        nil)))


;;; ===========================================================
;;; morpho (com.l 89)
;;; ===========================================================

(defun morpho (input)
  "Resolve INPUT -- a list of character codes in REVERSE order (as
   SENTIN collects them) -- to a canonical word, stripping inflection
   and undoing spelling changes. Sets *wrd* and returns T on success,
   NIL otherwise. Mirrors com.l 89."
  (block morpho
    (let ((*word* (mapcar #'lowcaseify input))
          (*rt* nil)
          (*modfelist* nil)
          first secnd third)
      (setq *rt* *word*)
      (tagbody
         ;; Direct hit, or hit after stripping a contraction / trailing
         ;; punctuation.
         (cond ((expandsim (setq *wrd* (implode (reverse *word*))))
                (return-from morpho t))
               ((strip-if-any '((n |'| t) (|'| s) (|'| d) (|'| m)
                                (|'| l l) (|'|) (|,|) (|.|) (?) (!)
                                (|:|) (|;|) (-)))
                (when (expandsim (setq *wrd* (implode (reverse *word*))))
                  (return-from morpho t))))
         (setq *modfelist* '(nil nil))
         ;; Recognise and strip an inflectional ending.
         (cond
           ((ends-in '(s))
            (modfe '(npl pres v3s) '(ns tnsless v-3s))
            (if (try) (return-from morpho t) (go esend)))
           ((ends-in '(l y)) (go lyend))
           ((ends-in '(i n g)) (modfe '(ing part adj) '(tnsless past pres)))
           ((ends-in '(e d)) (modfe '(past ed) '(tnsless pres)))
           ((ends-in '(e n)) (modfe '(en part adj) '(tnsless past pres)))
           ((ends-in '(e r)) (modfe '(comp) nil))
           ((ends-in '(e s t)) (modfe '(supr) nil))
           ((and (member (caddr *rt*) *numbers*)
                 (or (ends-in '(s t)) (ends-in '(n d))
                     (ends-in '(r d)) (ends-in '(t h))))
            (modfe '(ord) nil)))
         (when (try) (return-from morpho t))
         ;; Undo the spelling change that the ending triggered, then
         ;; retry at TEST.
         (setq first (car *rt*) secnd (cadr *rt*))
         (cond
           ((member first *vowels*)
            (cond ((eq first 'i) (setq *rt* (cons 'y (cdr *rt*))) (go test))
                  ((eq first 'y) (go test))
                  ((not (eq first 'e)) (go adde))
                  ((eq secnd 'e) (go test))
                  (t (if (try) (return-from morpho t) (go adde)))))
           ((eq first 'h)
            (unless (eq secnd 't) (go test))
            (if (try) (return-from morpho t) (go adde)))
           ((eq first secnd)
            (when (and (member first *liquids*) (try)) (return-from morpho t))
            (setq *rt* (cdr *rt*))
            (go test))
           ((member secnd *vowels*)
            ;; NOTE: (cdddr *rt*) is a list and never EQ a vowel, so this
            ;; always falls through to ADDE -- faithful to com.l 139.
            (unless (member (cdddr *rt*) *vowels*) (go adde))
            (when (member first *noend*) (go adde))
            (go test))
           ((member first *liquids*)
            (when (and (eq first 'l) (eq secnd 'r)) (go test))
            (go adde))
           ((member first *noend*) (go adde))
           (t (go test)))
       adde
         (setq *rt* (cons 'e *rt*))
         (go test)
       esend
         (setq first (car *rt*) secnd (cadr *rt*) third (caddr *rt*))
         (unless (eq first 'e) (go test))
         (cond
           ((eq secnd 'i) (setq *rt* (cons 'y (cddr *rt*))) (go test))
           ((eq secnd 'h) (unless (eq third 't) (go ecut)) (go test))
           ((eq secnd 'x) (go ecut))
           ((member secnd '(s z)) (when (eq third secnd) (go ecut)) (go test))
           ((eq secnd 'v)
            (setq *rt* (cons 'f (cddr *rt*)))
            (when (and (member (cadr *rt*) *vowels*)
                       (not (member (caddr *rt*) *vowels*)))
              (setq *rt* (cons 'e *rt*)))
            (go test))
           (t (return-from morpho nil)))
       ecut
         (setq *rt* (cdr *rt*))
         (go test)
       lyend
         (modfe '(adv manner) '(adj ns npl ngstart))
         (when (eq (car *rt*) 'i)
           (setq *rt* (cons 'y (cdr *rt*)))
           (go test))
         (when (try) (return-from morpho t))
         (setq *rt* (append '(e l) *rt*))
         (go test)
       test
         (when (try) (return-from morpho t)))
      nil)))
