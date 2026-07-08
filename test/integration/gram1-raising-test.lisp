;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-raising-test.lisp
;;;
;;; Subject-raising with a passive predicate complement, parsed end to end:
;;; "The jar seems broken." -- one of Marcus's hardest constructions, and a
;;; canonical example sentence (notes/pidgin-grammar-intro.text).
;;;
;;; `seem' is a NO-SUBJ raising verb whose object is a TO-BE-LESS infinitive
;;; complement, so the bare predicate `broken' is rewritten to `to be broken'
;;; (INSERT-TO-BE-1) and parsed as an embedded PASSIVE infinitive clause whose
;;; subject is a trace BOUND to the surface subject `the jar' -- i.e.
;;;
;;;     the jar_i seems [ t_i to be broken ]
;;;
;;; Derivation highlights: MAIN-VERB(seems) activates to-be-less-inf-comp +
;;; inf-comp + no-subj; INSERT-TO-BE-1 inserts "to be"; SEEMS hands off to
;;; PASSIVE, which preposes `the jar' and drops a bound trace into the buffer;
;;; INF-S-START1 takes the trace as the embedded subject; the embedded clause
;;; builds a passive aux (PASSIVE-AUX) + verb `broken'; then the committed
;;; complement-attachment layer (EMBEDDED-S-DONE -> COMP-TO-NP -> OBJECTS ->
;;; VP-NP) attaches the embedded clause as the matrix verb's object.
;;;
;;; NB: `broken' is analysed here as a PASSIVE PARTICIPLE (feature `en') --
;;; the reading that works. Marcus flagged the pure predicate-ADJECTIVE
;;; reading as not yet supported ("won't work for adjectives until the
;;; interpreter is changed"); this test deliberately exercises the passive
;;; path, not that one.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-raising-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-raising-test (&optional verbose)
  (let ((results t)
        (dot (intern "." :parsifal)))
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
           (word-of (np)
             (and np (getr 'word (daughter 'noun (daughter 'nbar np))))))

      ;; Small hand-built lexicon: the construction turns on seem's verb-type
      ;; features (to-be-less-inf-obj, no-subj) and broken's `en'.
      (reset-rule-table)
      (reset-lexicon)
      (dolist (w (list 'the 'jar 'seem 'broken 'to 'be dot))
        (setf (symbol-plist w) nil))
      (df the feats (det ngstart def ns npl))
      (df jar feats (noun ns n3p) markers (physob inanim))
      (df seem feats (verb mainverb pres tnsless v-3s that-obj to-be-less-inf-obj no-subj)
                cf (neut) neut (|#| inf-comp that-comp |#|) markers (state))
      (df broken feats (verb mainverb en v-3s tnsless)
                cf (neut (ins) (agt)) markers (act))
      (df to feats (prep auxverb *to))
      (df be feats (tnsless auxverb *be) cf (ess neut) neut (all) markers (state))
      (%df (list dot 'feats '(finalpunc punc)))
      (dolist (w (list 'the 'jar 'seem 'broken 'to 'be dot)) (expandsim w))

      (register-grammar *np-rules* *clause-rules* *vp-np-rule*
                        *inf-complement-rules* *raising-rules*)

      (let ((ok (parse-sentence "the jar seems broken ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"the jar seems broken .\"" ok t)

        ;; Matrix clause: subject preposed, verb seems.
        (truthy "matrix S is a major declarative" (subsetp '(decl major s) (fe c)))
        (truthy "matrix S is np-preposed (raising)" (member 'np-preposed (fe c)))
        (check "the surface subject is \"jar\"" (word-of (daughter 'np c)) 'jar)

        (let* ((vp     (daughter 'vp c))
               (compnp (and vp (daughter 'np vp))))
          (truthy "a VP attached to S" vp)
          (check "matrix verb is \"seem\""
                 (and vp (getr 'word (daughter 'verb vp))) 'seems)

          ;; The complement is the embedded clause, wrapped as a comp-np.
          (truthy "the matrix object is a comp-np" (and compnp (member 'comp-np (fe compnp))))
          (check "the comp-np is marked inf-comp"
                 (and compnp (getr 'markers compnp)) '(inf-comp))

          (let* ((embs  (and compnp (daughter 's compnp)))
                 (esubj (and embs (daughter 'np embs)))
                 (eaux  (and embs (daughter 'aux embs)))
                 (evp   (and embs (daughter 'vp embs))))
            (truthy "the comp-np dominates an embedded S" embs)
            (truthy "the embedded S is a comp-s / inf-s"
                    (and embs (subsetp '(sec comp-s inf-s s) (fe embs))))
            (truthy "the embedded S is itself np-preposed (passive)"
                    (and embs (member 'np-preposed (fe embs))))

            ;; The raising: the embedded subject is a trace bound to the
            ;; surface subject `the jar'.
            (truthy "the embedded subject is a trace" (and esubj (member 'trace (fe esubj))))
            (check "the embedded trace is bound to the surface subject NP"
                   (and esubj (getr 'binding esubj)) (daughter 'np c))

            ;; The embedded clause is a passive infinitive.
            (truthy "the embedded aux is an inf aux" (and eaux (member 'inf (fe eaux))))
            (check "the embedded aux carries `to'"
                   (and eaux (getr 'word (daughter 'to eaux))) 'to)
            (truthy "the embedded aux is passive" (and eaux (daughter 'passive eaux)))
            (check "the embedded verb is \"broken\""
                   (and evp (getr 'word (daughter 'verb evp))) 'broken))

          (truthy "final punctuation attached to S" (daughter 'finalpunc c))

          ;; Matrix case frame: seem is a no-subj verb (no agent); the
          ;; embedded clause fills its NEUTRAL/object case.
          (let ((cf (and vp (getr 'caseframe vp))))
            (truthy "matrix VP has a case frame" cf)
            (check "matrix predicate is seem" (and cf (get cf 'pred)) 'seem)
            (closeframe openframe)
            (let ((filled (cadr (first (get cf 'hypo-slots)))))
              (truthy "the matrix frame has filled cases" filled)
              (check "no agent case was filled (seem is no-subj)"
                     (assoc 'agt filled) nil)
              (let ((neut (assoc 'neut filled)))
                (truthy "a neutral case was filled" neut)
                (check "the neutral case is the comp-np (the embedded clause)"
                       (cadr neut) compnp)
                (check "the neutral case was filled via the obj function"
                       (caddr neut) 'obj)))))))

    (format t "~&gram1-raising-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-raising-test t)
