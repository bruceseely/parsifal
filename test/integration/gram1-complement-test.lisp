;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-complement-test.lisp
;;;
;;; Embedded infinitive complement with an explicit subject, parsed end to
;;; end: "the boy wants the boy to go ." The matrix verb `want' is an
;;; inf-obj verb, so MAIN-VERB activates INF-COMP; INF-S-START1 then builds
;;; an embedded sec/comp-s/inf-s clause ("the boy to go") with its own
;;; subject, aux (TO-INFINITIVE) and verb.
;;;
;;; The crux this test pins down is the CLAUSAL-COMPLEMENT ATTACHMENT: once
;;; the embedded S is finished and dropped to the buffer, COMP-TO-NP wraps
;;; it in a `comp-np' node (marked inf-comp via the NP-S crule) and drops
;;; THAT to the buffer, where the ordinary clause-layer OBJECTS rule attaches
;;; it as the matrix VP's object -- and VP-NP fills the matrix verb's NEUTRAL
;;; case with it. So the embedded clause becomes a first-class object
;;; argument of the matrix verb.
;;;
;;; Result tree:
;;;   [S decl/major [NP the boy] [aux] [VP [verb wants]
;;;       [NP comp-np [S sec/comp-s/inf-s [NP the boy] [aux to] [VP [verb go]]]]]
;;;       [finalpunc .]]
;;;
;;; Case frame of the matrix VP: pred WANT, AGT = matrix subject (via subj),
;;; NEUT = the comp-np (via obj).
;;;
;;; A small hand-built lexicon is used deliberately: the construction turns
;;; on the verb-type feature `inf-obj' and want's case frame / markersets,
;;; which are clearest stated explicitly here.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-complement-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-complement-test (&optional verbose)
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
           (word-of (np)            ; head noun's word under an NP
             (and np (getr 'word (daughter 'noun (daughter 'nbar np))))))

      ;; Small hand-built lexicon (clear any dictionary-loaded plists first).
      (reset-rule-table)
      (reset-lexicon)
      (dolist (w (list 'the 'boy 'want 'to 'go dot)) (setf (symbol-plist w) nil))
      (df the feats (det ngstart def ns npl))
      (df boy feats (noun ns n3p) markers (hanim physob))
      (df want feats (verb mainverb pres tnsless v-3s inf-obj)
                cf (neut agt) neut (|#| inf-comp hanim |#| anim) markers (act))
      (df to feats (prep auxverb *to))
      (df go feats (verb mainverb pres tnsless v-3s) cf (nil (pfrom) (pto) agt) markers (act))
      (%df (list dot 'feats '(finalpunc punc)))
      (dolist (w (list 'the 'boy 'want 'to 'go dot)) (expandsim w))

      (register-grammar *np-rules* *clause-rules* *vp-np-rule*
                        *inf-complement-rules*)

      (let ((ok (parse-sentence "the boy wants the boy to go ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"the boy wants the boy to go .\"" ok t)
        (truthy "S is a major declarative clause"
                (subsetp '(decl major s) (fe c)))
        (check "matrix subject is \"boy\"" (word-of (daughter 'np c)) 'boy)

        (let* ((vp  (daughter 'vp c))
               (obj (and vp (daughter 'np vp))))
          (truthy "a VP attached to S" vp)
          (check "matrix verb is \"want\""
                 (and vp (getr 'word (daughter 'verb vp))) 'wants)

          ;; The object of the matrix VP is the embedded clause, wrapped as a
          ;; comp-np marked inf-comp.
          (truthy "the matrix VP has an object NP" obj)
          (truthy "the object NP is a comp-np"
                  (and obj (member 'comp-np (fe obj))))
          (check "the comp-np is marked inf-comp"
                 (and obj (getr 'markers obj)) '(inf-comp))

          (let* ((embs (and obj (daughter 's obj)))
                 (evp  (and embs (daughter 'vp embs))))
            (truthy "the comp-np dominates an embedded S" embs)
            (truthy "the embedded S is a comp-s / inf-s"
                    (and embs (subsetp '(sec comp-s inf-s s) (fe embs))))
            (check "the embedded subject is \"boy\""
                   (word-of (daughter 'np embs)) 'boy)
            (check "the embedded clause has an inf aux with `to'"
                   (let ((aux (and embs (daughter 'aux embs))))
                     (and aux (member 'inf (fe aux))
                          (getr 'word (daughter 'to aux))))
                   'to)
            (check "the embedded verb is \"go\""
                   (and evp (getr 'word (daughter 'verb evp))) 'go))

          (truthy "final punctuation attached to S" (daughter 'finalpunc c))

          ;; The matrix verb's case frame: subject -> AGENT, the embedded
          ;; clause (comp-np) -> NEUTRAL/object.
          (let ((cf (and vp (getr 'caseframe vp))))
            (truthy "matrix VP has a case frame" cf)
            (check "matrix predicate is want" (and cf (get cf 'pred)) 'want)
            (closeframe openframe)
            (let ((filled (cadr (first (get cf 'hypo-slots)))))
              (truthy "the matrix frame has filled cases" filled)
              (let ((agt (assoc 'agt filled)) (neut (assoc 'neut filled)))
                (truthy "an agent case was filled" agt)
                (check "the agent is the matrix subject NP"
                       (cadr agt) (daughter 'np c))
                (truthy "a neutral case was filled" neut)
                (check "the neutral case is the comp-np (the embedded clause)"
                       (cadr neut) obj)
                (check "the neutral case was filled via the obj function"
                       (caddr neut) 'obj)))))))

    (format t "~&gram1-complement-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-complement-test t)
