;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/core/runtime/defs.lisp
;;;
;;; CL port of the feature-ontology head of Marcus's defs.l
;;; (lines 1-84). Populates the canonical feature lists the parser
;;; checks against:
;;;
;;;   *nr-types*, *as-types* -- trigger NR and AS rules
;;;   *parts-of-speech*, *sentence-types*, *specregs*
;;;   refillables
;;;
;;; Plus a port of Marcus's REDUND macro -- the redundant-feature
;;; implication declarations. We record them in *REDUND-TABLE*; the
;;; expansion logic that consults the table lives in the parser
;;; (and is mostly a future-work concern).
;;;
;;; Deferred: the lexicon half of defs.l (lines 85-876) -- verb,
;;; noun, and adjective definitions via the (df ...), (df1 ...),
;;; (df+ ...), (jlike ...), (abbrev ...), (irreg ...) family. Those
;;; only matter once we're parsing real strings, which needs com.l's
;;; sentin / morpho ported. For now we have the structural slots that
;;; the parser consults at runtime, not the per-word data.
;;;
;;; Exports: we re-export the feature symbols from :parsifal so that
;;; glang-cl-tokenized grammar source (which lives in :glang-cl with
;;; (:use #:cl #:parsifal)) finds the same symbol identity, no
;;; matter which side of the package boundary first names a feature.

(in-package :parsifal)


;;; ===========================================================
;;; The canonical feature symbols
;;; ===========================================================
;;;
;;; Defined via DEFCONSTANT-style intern + export so that any feature
;;; mentioned below is a :parsifal symbol that glang-cl source will
;;; see via inheritance. Listed roughly in the same groups Marcus's
;;; source uses, with feature symbols that recur in gram4.l number
;;; rules pulled forward.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export
   (mapcar (lambda (n) (intern (string n) :parsifal))
           '(;; NR / AS types
             np nbar
             noun pronoun verb adj num quant det ord prep poss-np
             ;; Parts of speech
             name punc
             ;; Sentence types
             decl ynquest imper whquest inf-s
             ;; Specregs (parser-internal register names)
             type cases-marked-by case-frame neut ess markers
             ;; Number-builder features (gram4.l)
             tens ones complete-num npl ns
             *hundred bignum hundred+ bignumg bignum+ |99S|
             ;; Verb/clause features (defs.l redund table)
             ngstart pres past future tnsless auxverb modal
             comp-obj inf-obj that-obj
             subj-less-inf-obj 2-obj-inf-obj
             to-be-less-inf-obj no-subj to-less-inf-obj
             obj-binds-delta subj-binds-delta mainverb
             ;; Miscellaneous referenced from defs.l line 1-84
             time refillables))
   :parsifal))


;;; ===========================================================
;;; Feature lists Marcus's parse-loop consults at runtime
;;; ===========================================================

(setf refillables       '(time)
      *nr-types*        '(np nbar s)
      *as-types*        '(noun pronoun verb adj num quant det ord prep poss-np)
      *parts-of-speech* '(noun verb adj det quant num ord name punc prep pronoun)
      *sentence-types*  '(decl ynquest imper whquest inf-s))

;;; *specregs* -- registers the parser treats as semantically special.
;;; Not yet referenced by anything we've ported, but declared so the
;;; symbol is bound when downstream code lands.
(defvar *specregs*
  '(quant type cases-marked-by case-frame neut ess markers))


;;; ===========================================================
;;; REDUND -- redundant-feature implications
;;; ===========================================================

(defvar *redund-table* (make-hash-table :test #'eq)
  "Map a feature SYMBOL to the features it implies: a node carrying the
   key feature also (redundantly) carries each feature in its value list.
   `add-redunds' (lexicon.lisp) closes a feature set under this table.
   E.g. `(redund ngstart (det noun ...))' puts NGSTART on DET's, NOUN's,
   ... entries, so a determiner is also a noun-group start.")

(defmacro redund (feature implications)
  "Record that each of IMPLICATIONS implies FEATURE -- a node carrying
   any implication feature also gains FEATURE. Mirrors Marcus's REDUND
   (com.l 494), which adds FEATURE to each implication's `:redunds'
   plist; we add it to each implication's *REDUND-TABLE* entry. So
   `(redund ngstart (det noun quant num ord pronoun))' makes a det /
   noun / ... node an NGSTART, and `(redund verb (pres past future
   tnsless))' makes a tensed word a VERB. (The earlier port stored this
   backwards -- feature -> implications -- so dictionary words never
   picked up their implied features.)"
  `(dolist (impl ',implications)
     (pushnew ',feature (gethash impl *redund-table*))))


(redund ngstart        (det noun quant num ord pronoun))
(redund verb           (pres past future tnsless))
(redund auxverb        (modal))
(redund comp-obj       (inf-obj that-obj))
(redund inf-obj        (subj-less-inf-obj 2-obj-inf-obj
                        to-be-less-inf-obj no-subj to-less-inf-obj))
(redund 2-obj-inf-obj  (obj-binds-delta subj-binds-delta))
(redund mainverb       (comp-obj))
