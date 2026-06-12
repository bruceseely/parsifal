;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-subject-control-test.lisp
;;;
;;; Subject-control complement, parsed end to end and entirely from the LOADED
;;; defs.l dictionary: "the boy promised the girl to go ."
;;;
;;; This is the other half of the famous Marcus control minimal pair (see
;;; gram1-object-control-test, "the boy persuaded the girl to go ."). `promise'
;;; and `persuade' are structurally identical -- both 2-obj-inf-obj verbs that
;;; take an NP object plus an infinitive complement with a DELTA subject -- but
;;; they bind that delta to opposite controllers:
;;;
;;;     the boy_i promised  the girl_j [ delta_i to go ]   (promise: i = boy)
;;;     the boy_j persuaded the girl_i [ delta_i to go ]   (persuade: i = girl)
;;;
;;; In the dictionary `promise' is `jlike persuade' with subj-binds-delta
;;; replacing obj-binds-delta. subj-binds-delta still `redund'-implies
;;; 2-obj-inf-obj -> inf-obj, so the derivation is byte-for-byte the persuade
;;; derivation: MAIN-VERB activates 2-obj-inf-comp; OBJECTS attaches the first
;;; object (`the girl', DAT); CREATE-DELTA-SUBJ (*two-object-inf-rules*) drops a
;;; trace born `delta' and hands off to inf-comp; INF-S-START1 builds the
;;; embedded inf-S; DELTA-SUBJ-S-DONE drops it unfinalized. The ONLY thing that
;;; differs is which arm of the COMPLETE VP-NP crule fires: for subj-binds-delta
;;; it binds the embedded delta to "the np of the current s" -- the matrix
;;; SUBJECT (`the boy') -- not the indirect object. So no new rules at all are
;;; needed here; this test pins down the subj-binds-delta arm of
;;; *vp-np-full-rule*, exercised for the first time.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-subject-control-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-subject-control-test (&optional verbose)
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
           (word-of (np)
             (and np (getr 'word (daughter 'noun (daughter 'nbar np))))))

      ;; Dictionary-driven, and SAME rule groups as the persuade test: the
      ;; object/subject-control split is in the dictionary (which feature) and
      ;; in *vp-np-full-rule* (which arm), not in any packet rule.
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *inf-complement-rules*
                        *delta-complement-rules* *two-object-inf-rules*
                        *vp-np-full-rule*)

      ;; Sanity: `promise' is `jlike persuade', but its feature override flips
      ;; the control: subj-binds-delta in, obj-binds-delta out. It is still
      ;; 2-obj-inf-obj -> inf-obj, so MAIN-VERB still activates 2-obj-inf-comp.
      (check "dict `promise' is `jlike persuade'" (get 'promise 'jlike) 'persuade)
      (expandsim 'promise)
      (let ((feats (get 'promise 'features)))
        (truthy "expanded `promise' is subj-binds-delta" (member 'subj-binds-delta feats))
        (truthy "expanded `promise' is NOT obj-binds-delta"
                (not (member 'obj-binds-delta feats)))
        (truthy "...still redund-implies 2-obj-inf-obj" (member '2-obj-inf-obj feats))
        (truthy "...and inf-obj" (member 'inf-obj feats)))

      (let ((ok (parse-sentence "the boy promised the girl to go ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"the boy promised the girl to go .\"" ok t)
        (truthy "matrix S is a major declarative" (subsetp '(decl major s) (fe c)))
        (check "matrix subject is \"boy\"" (word-of (daughter 'np c)) 'boy)

        (let* ((vp      (daughter 'vp c))
               ;; Two np daughters, stored most-recent-first: comp-np is car,
               ;; the direct object `the girl' is cadr.
               (nps     (and vp (daughters 'np vp)))
               (compnp  (car nps))
               (objnp   (cadr nps)))
          (truthy "a VP attached to S" vp)
          (check "matrix verb is \"promise\""
                 (and vp (getr 'word (daughter 'verb vp))) 'promised)
          (check "the matrix VP has exactly two NP objects" (length nps) 2)
          (check "the first (direct) object is \"girl\"" (word-of objnp) 'girl)
          (truthy "the second object is a comp-np"
                  (and compnp (member 'comp-np (fe compnp))))
          (check "the comp-np is marked inf-comp"
                 (and compnp (getr 'markers compnp)) '(inf-comp))

          (let* ((embs  (and compnp (daughter 's compnp)))
                 (esubj (and embs (daughter 'np embs)))
                 (evp   (and embs (daughter 'vp embs))))
            (truthy "the comp-np dominates an embedded S" embs)
            (truthy "the embedded S is a comp-s / inf-s"
                    (and embs (subsetp '(sec comp-s inf-s s) (fe embs))))
            (check "the embedded verb is \"go\""
                   (and evp (getr 'word (daughter 'verb evp))) 'go)

            ;; The control: the embedded delta is bound to the matrix SUBJECT
            ;; (the boy) -- NOT the object (the girl). This is the mirror image
            ;; of the persuade case.
            (truthy "the embedded subject is a trace" (and esubj (member 'trace (fe esubj))))
            (truthy "the embedded subject is labelled delta"
                    (and esubj (member 'delta (fe esubj))))
            (check "the embedded delta is bound to the matrix SUBJECT (subject control)"
                   (and esubj (getr 'binding esubj)) (daughter 'np c))
            (truthy "...and NOT to the matrix object (the girl)"
                    (not (eq (getr 'binding esubj) objnp))))

          (truthy "final punctuation attached to S" (daughter 'finalpunc c))

          ;; Matrix case frame: subject -> AGENT (and controller), `the girl' ->
          ;; DAT, embedded clause -> NEUTRAL.
          (let ((cf (and vp (getr 'caseframe vp))))
            (truthy "matrix VP has a case frame" cf)
            (check "matrix predicate is promise" (and cf (get cf 'pred)) 'promise)
            (closeframe openframe)
            (let ((filled (cadr (first (get cf 'hypo-slots)))))
              (truthy "the matrix frame has filled cases" filled)
              (let ((agt  (assoc 'agt filled))
                    (dat  (assoc 'dat filled))
                    (neut (assoc 'neut filled)))
                (truthy "an agent case was filled" agt)
                (check "the agent is the matrix subject NP (the boy)"
                       (cadr agt) (daughter 'np c))
                (truthy "a dative case was filled" dat)
                (check "the dative is the direct object NP (the girl)" (cadr dat) objnp)
                (truthy "a neutral case was filled" neut)
                (check "the neutral case is the comp-np (the embedded clause)"
                       (cadr neut) compnp)
                ;; The controller is the AGENT (the boy), NOT the dative -- the
                ;; exact opposite of persuade.
                (check "the embedded delta's controller is the matrix agent"
                       (getr 'binding (daughter 'np (daughter 's compnp)))
                       (cadr agt))))))))

    (format t "~&gram1-subject-control-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-subject-control-test t)
