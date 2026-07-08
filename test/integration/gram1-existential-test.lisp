;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-existential-test.lisp
;;;
;;; An existential `there' yes-no question, parsed end to end and entirely from
;;; the loaded defs.l dictionary: "is there a meeting ?"
;;;
;;;     [S existential/quest/ynquest/major
;;;        [NP there]  [aux is]  [VP is]  [NP a meeting] ]
;;;
;;; The yes-no opener inverts the auxiliary `is' over the subject `there' (a
;;; pseudopropnoun NP whose noun is *there); then, while the aux is being built,
;;; THERE fires (build-aux, [=*be][=np] with the subject = `there'): it relabels
;;; the clause `existential' and attaches the following NP `a meeting' as the
;;; clause's logical subject (a second np of the S). The copula `be' also serves
;;; as the clause's main verb.
;;;
;;; (The full canonical "Is there a meeting scheduled for friday?" additionally
;;; needs a passive-participle reduced relative -- "a meeting scheduled ..." --
;;; which is a separate construction; this test exercises the existential core.)
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-existential-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-existential-test (&optional verbose)
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

      ;; Fully dictionary-driven: is(be)/there/a/meeting/? all from defs.l.
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule* *pronoun-rule*
                        *qp1-done-rule* *inversion-rules* *yes-no-rules*
                        *there-rules*)

      (let ((ok (parse-sentence "is there a meeting ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"is there a meeting ?\"" ok t)

        ;; An existential yes-no question.
        (truthy "the clause is a yes-no question (quest/ynquest/major)"
                (subsetp '(quest ynquest major s) (fe c)))
        (truthy "the clause is labelled `existential'" (member 'existential (fe c)))
        (truthy "the clause is NOT a wh-question" (not (member 'wh-quest (fe c))))

        ;; Two NP daughters: the existential subject `there' and the logical
        ;; subject `a meeting' (attached by THERE).
        (let* ((nps     (daughters 'np c))
               (there-np  (find-if (lambda (n) (eq (noun-of n) 'there)) nps))
               (meeting-np (find-if (lambda (n) (eq (noun-of n) 'meeting)) nps)))
          (check "the S has two NP daughters" (length nps) 2)
          (truthy "one NP is the existential subject `there'" there-np)
          (truthy "`there' is a pseudopropnoun place NP"
                  (and there-np (subsetp '(place) (fe (daughter 'noun (daughter 'nbar there-np))))))
          (truthy "the other NP is the logical subject `a meeting'" meeting-np)
          (truthy "`a meeting' is an indefinite NP"
                  (and meeting-np (member 'indef (fe meeting-np)))))

        ;; The copula `is' is the auxiliary (present) ...
        (let ((aux (daughter 'aux c)))
          (truthy "an aux attached to S" aux)
          (truthy "the aux is present tense" (and aux (member 'pres (fe aux)))))

        ;; ... and also the clause's main verb (the existential copula).
        (let ((vp (daughter 'vp c)))
          (truthy "a VP attached to S" vp)
          (check "the main verb is the copula `be'"
                 (and vp (getr 'word (daughter 'verb vp))) 'is))

        (truthy "final (question) punctuation attached" (daughter 'finalpunc c))))

    (format t "~&gram1-existential-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-existential-test t)
