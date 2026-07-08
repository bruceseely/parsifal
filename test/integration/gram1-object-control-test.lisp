;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-object-control-test.lisp
;;;
;;; Object-control complement, parsed end to end and entirely from the LOADED
;;; defs.l dictionary: "the boy persuaded the girl to go ."
;;;
;;; This is the famous Marcus contrast with the subject-control `want' case
;;; (gram1-control-test). `persuade' is a 2-obj-inf-obj verb (obj-binds-delta):
;;; it takes TWO objects, an ordinary NP and an infinitive complement whose
;;; subject is a DELTA -- a base-generated trace bound not by syntax but by
;;; later processing. Where `want' binds its embedded delta to the matrix
;;; SUBJECT, `persuade' binds it to the matrix OBJECT:
;;;
;;;     the boy_j persuaded the girl_i [ delta_i to go ]    (i = girl, not boy)
;;;
;;; Derivation path: `persuade' is obj-binds-delta, which `redund'-implies
;;; 2-obj-inf-obj -> inf-obj, so MAIN-VERB activates 2-obj-inf-comp (and, for
;;; the major clause, ss-vp). OBJECTS attaches the first object `the girl' (it
;;; fits persuade's DAT slot). Then, with [to][go] in the buffer,
;;; CREATE-DELTA-SUBJ (the new rule, IN 2-OBJ-INF-COMP) drops a trace ALREADY
;;; labelled `delta' for the missing embedded subject and hands off to inf-comp;
;;; INF-S-START1 builds the embedded sec/comp-s/inf-s ("delta to go"). Because
;;; the trace is born `delta', SUBJECT-IS-DELTA-DIAG is a no-op and
;;; DELTA-SUBJ-S-DONE drops the embedded S without finalizing its frame.
;;; COMP-TO-NP wraps it in a comp-np; OBJECTS attaches it as the matrix verb's
;;; second object; and the COMPLETE VP-NP crule's obj-binds-delta arm binds the
;;; embedded delta to the matrix verb's INDIRECT object -- `the girl' -- then
;;; fills the embedded subj slot and finalizes the embedded frame.
;;;
;;; The single new rule vs. the `want' case is *two-object-inf-rules*
;;; (CREATE-DELTA-SUBJ). Everything else composes: *inf-complement-rules*,
;;; *delta-complement-rules* (for DELTA-SUBJ-S-DONE), and *vp-np-full-rule*
;;; (whose obj-binds-delta arm already existed, exercised here for the first
;;; time).
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-object-control-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-object-control-test (&optional verbose)
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

      ;; Dictionary-driven: persuade/girl/boy/go/the all come from the loaded
      ;; defs.l.
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *inf-complement-rules*
                        *delta-complement-rules* *two-object-inf-rules*
                        *vp-np-full-rule*)

      ;; Sanity: in the dictionary `persuade' is `jlike tell', so it inherits
      ;; tell's obj-binds-delta; expanding the word (as lookup does) closes that
      ;; under `redund' to 2-obj-inf-obj -> inf-obj, which is what makes
      ;; MAIN-VERB activate the 2-obj-inf-comp packet.
      (check "dict `persuade' is `jlike tell'" (get 'persuade 'jlike) 'tell)
      (truthy "base `tell' is obj-binds-delta"
              (member 'obj-binds-delta (get 'tell 'feats)))
      (expandsim 'persuade)
      (let ((feats (get 'persuade 'features)))
        (truthy "expanded `persuade' is obj-binds-delta" (member 'obj-binds-delta feats))
        (truthy "...redund-implies 2-obj-inf-obj" (member '2-obj-inf-obj feats))
        (truthy "...and inf-obj" (member 'inf-obj feats)))

      (let ((ok (parse-sentence "the boy persuaded the girl to go ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"the boy persuaded the girl to go .\"" ok t)
        (truthy "matrix S is a major declarative" (subsetp '(decl major s) (fe c)))
        (check "matrix subject is \"boy\"" (word-of (daughter 'np c)) 'boy)

        (let* ((vp      (daughter 'vp c))
               ;; The VP has TWO np daughters: stored most-recent-first, so
               ;; the comp-np is car and the direct object `the girl' is cadr
               ;; (= the indirect object the crule binds to).
               (nps     (and vp (daughters 'np vp)))
               (compnp  (car nps))
               (objnp   (cadr nps)))
          (truthy "a VP attached to S" vp)
          (check "matrix verb is \"persuade\""
                 (and vp (getr 'word (daughter 'verb vp))) 'persuaded)
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

            ;; The control: the embedded subject is a delta trace with NO
            ;; explicit lexical subject, bound to the matrix OBJECT (the girl),
            ;; NOT the matrix subject (the boy).
            (truthy "the embedded subject is a trace" (and esubj (member 'trace (fe esubj))))
            (truthy "the embedded subject is labelled delta"
                    (and esubj (member 'delta (fe esubj))))
            (check "the embedded delta is bound to the matrix OBJECT (object control)"
                   (and esubj (getr 'binding esubj)) objnp)
            (truthy "...and NOT to the matrix subject"
                    (not (eq (getr 'binding esubj) (daughter 'np c)))))

          (truthy "final punctuation attached to S" (daughter 'finalpunc c))

          ;; Matrix case frame: subject -> AGENT, `the girl' -> DAT (recipient),
          ;; embedded clause -> NEUTRAL.
          (let ((cf (and vp (getr 'caseframe vp))))
            (truthy "matrix VP has a case frame" cf)
            (check "matrix predicate is persuade" (and cf (get cf 'pred)) 'persuade)
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
                ;; The controller is the DATIVE (the girl), distinct from the agent.
                (check "the embedded delta's controller is the matrix dative"
                       (getr 'binding (daughter 'np (daughter 's compnp)))
                       (cadr dat))))))))

    (format t "~&gram1-object-control-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-object-control-test t)
