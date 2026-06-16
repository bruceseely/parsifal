;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-ditransitive-that-test.lisp
;;;
;;; DITRANSITIVE that-clause complements: "I told that boy that boys should
;;; do it ." -- a 2-object that-obj verb (`tell') with a FULL-NP recipient
;;; (`that boy') followed by a that-clause (`that boys should do it').
;;;
;;; The infinitive analogue ("told the boy to go") already worked via
;;; 2-OBJ-INF-COMP, and a PRONOUN recipient ("told you that ...") worked too
;;; -- but a full-NP recipient + that-clause failed, because the recipient's
;;; nbar must be completed (NBAR-COMPLETE) before the `that' is diagnosed,
;;; and the rule that resolves the det/comp ambiguity there, Marcus's
;;; THAT-DIAG-3 (gram2:240), had not been ported. THAT-DIAG-3 fires in
;;; NBAR-COMPLETE: a `that' + NP after a verb that needs two objects is the
;;; complementizer, not a determiner of the recipient. Porting it (verbatim)
;;; makes the full-NP-recipient ditransitive that-clause parse.
;;;
;;; Expected case structure (the meaning):
;;;   TELL  AGT=I  DAT=(that) boy  NEUT=[ boys should do it ]
;;;     embedded DO  (modal `should')  AGT=boys  NEUT=it
;;;
;;; This sentence also exercises, in one parse: a demonstrative determiner
;;; (`that boy'), a bare plural subject (`boys'), and a modal + do
;;; (`should do').
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-ditransitive-that-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-ditransitive-that-test (&optional verbose)
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
           (noun-of (np) (and np (getr 'word (daughter 'noun (daughter 'nbar np))))))

      (load-full-grammar)

      ;; The flagship: full-NP recipient + that-clause.
      (let ((ok (parse-sentence "i told that boy that boys should do it ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds: I told that boy that boys should do it ." ok t)
        (truthy "top S is a major declarative" (subsetp '(decl major s) (fe c)))

        (let* ((vp  (daughter 'vp c))
               (nps (and vp (daughters 'np vp))))
          (check "matrix verb is `tell'"
                 (and vp (getr 'word (daughter 'verb vp))) 'told)
          (truthy "the matrix VP has two objects (recipient + clause)"
                  (and nps (= (length nps) 2)))
          ;; daughters come back clause-first, recipient-second.
          (let ((recip  (find-if-not (lambda (n) (member 'comp-np (fe n))) nps))
                (compnp (find-if   (lambda (n) (member 'comp-np (fe n))) nps)))
            (check "the recipient object is the NP `boy' (from `that boy')"
                   (noun-of recip) 'boy)
            (truthy "the other object is a comp-np (the that-clause)"
                    (and compnp (member 'comp-np (fe compnp))))
            (let* ((embs (and compnp (daughter 's compnp)))
                   (evp  (and embs (daughter 'vp embs))))
              (truthy "the comp-np dominates an embedded that-S"
                      (and embs (subsetp '(sec comp-s that-s s) (fe embs))))
              (check "the embedded verb is `do'"
                     (and evp (getr 'word (daughter 'verb evp))) 'do)
              (truthy "the embedded aux is a modal (should)"
                      (let ((a (and embs (daughter 'aux embs))))
                        (and a (member 'modal (fe a)))))
              (check "the embedded subject is the bare plural `boys'"
                     (noun-of (daughter 'np embs)) 'boys))))

        ;; Case frame: TELL with agent / recipient / clausal neutral.
        (let* ((vp (daughter 'vp c))
               (cf (and vp (getr 'caseframe vp))))
          (closeframe openframe)
          (check "matrix predicate is TELL" (and cf (get cf 'pred)) 'tell)
          (let ((filled (cadr (first (get cf 'hypo-slots)))))
            (truthy "a dative (recipient) case is filled" (assoc 'dat filled))
            (truthy "a neutral (the clause) case is filled" (assoc 'neut filled))))))

    (format t "~&gram1-ditransitive-that-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-ditransitive-that-test t)
