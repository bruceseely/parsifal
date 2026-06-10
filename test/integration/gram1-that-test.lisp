;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-that-test.lisp
;;;
;;; That-clause (tensed sentential) complement, parsed end to end and
;;; entirely from the LOADED defs.l dictionary:
;;; "the boy believes that the lecture meets ."
;;;
;;; `believe' is a that-obj verb. `that' is det\comp-ambig in the lexicon, so
;;; THAT-DIAG-1 first diagnoses it as a complementizer (the following NP `the
;;; lecture' already has a determiner, so `that' can't be its determiner ->
;;; comp); THAT-S-START then opens an embedded sec/comp-s/that-s clause with
;;; `that' as comp and `the lecture' as subject; the embedded clause parses as
;;; an ordinary tensed clause; then the committed complement-attachment layer
;;; (EMBEDDED-S-DONE -> COMP-TO-NP -> OBJECTS -> VP-NP) attaches it as the
;;; matrix verb's object. NP-S marks the comp-np `that-comp' (not inf-comp).
;;;
;;;     the boy believes [ that the lecture meets ]
;;;
;;; Unlike the infinitive complements, there is NO trace/delta here: the
;;; embedded clause has its own overt tensed subject. This reuses the same
;;; comp-attachment machinery as the infinitive complements with no new
;;; attachment rule -- only the that-diagnosis + that-s-start front end.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-that-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-that-test (&optional verbose)
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

      ;; Dictionary-driven: believe/that/the/boy/lecture/meet all from defs.l.
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*
                        *inf-complement-rules* *that-complement-rules*)

      ;; Sanity: `believe' really is a that-obj verb, `that' is the ambiguous
      ;; det/comp word.
      (truthy "dict `believe' is that-obj"
              (member 'that-obj (get 'believe 'feats)))

      (let ((ok (parse-sentence "the boy believes that the lecture meets ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"the boy believes that the lecture meets .\"" ok t)
        (truthy "matrix S is a major declarative" (subsetp '(decl major s) (fe c)))
        (check "matrix subject is \"boy\"" (word-of (daughter 'np c)) 'boy)

        (let* ((vp     (daughter 'vp c))
               (compnp (and vp (daughter 'np vp))))
          (truthy "a VP attached to S" vp)
          (check "matrix verb is \"believe\""
                 (and vp (getr 'word (daughter 'verb vp))) 'believes)
          (truthy "the matrix object is a comp-np"
                  (and compnp (member 'comp-np (fe compnp))))
          ;; that-clause complement is marked that-comp (not inf-comp)
          (check "the comp-np is marked that-comp"
                 (and compnp (getr 'markers compnp)) '(that-comp))

          (let* ((embs (and compnp (daughter 's compnp)))
                 (evp  (and embs (daughter 'vp embs))))
            (truthy "the comp-np dominates an embedded S" embs)
            (truthy "the embedded S is a comp-s / that-s"
                    (and embs (subsetp '(sec comp-s that-s s) (fe embs))))
            (check "the embedded complementizer is \"that\""
                   (and embs (getr 'word (daughter 'comp embs))) 'that)
            (check "the embedded subject is \"lecture\""
                   (word-of (daughter 'np embs)) 'lecture)
            (check "the embedded verb is \"meet\""
                   (and evp (getr 'word (daughter 'verb evp))) 'meets))

          (truthy "final punctuation attached to S" (daughter 'finalpunc c))

          ;; Matrix case frame: subject -> AGENT, that-clause -> NEUTRAL.
          (let ((cf (and vp (getr 'caseframe vp))))
            (truthy "matrix VP has a case frame" cf)
            (check "matrix predicate is believe" (and cf (get cf 'pred)) 'believe)
            (closeframe openframe)
            (let ((filled (cadr (first (get cf 'hypo-slots)))))
              (truthy "the matrix frame has filled cases" filled)
              (let ((agt (assoc 'agt filled)) (neut (assoc 'neut filled)))
                (truthy "an agent case was filled" agt)
                (check "the agent is the matrix subject NP" (cadr agt) (daughter 'np c))
                (truthy "a neutral case was filled" neut)
                (check "the neutral case is the comp-np (the that-clause)"
                       (cadr neut) compnp)
                (check "the neutral case was filled via the obj function"
                       (caddr neut) 'obj)))))))

    (format t "~&gram1-that-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-that-test t)
