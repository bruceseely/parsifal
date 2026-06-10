;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-wh-stranding-test.lisp
;;;
;;; A wh-question with a STRANDED PREPOSITION, parsed end to end -- "who did
;;; the boy talk to ?" -- the iconic harder wh-vp placement case. The fronted
;;; wh-element is the object of a preposition left dangling at the clause end:
;;;
;;;     who_i  did  the boy  talk  to  t_i
;;;
;;; After the subject-aux inversion (as in the object-wh test), MAIN-VERB
;;; leaves the wh-comp unutilized into the VP. At the stranded `to', the buffer
;;; is [to][?] -- a preposition followed by a non-NP -- so WH-PP-BUILD fires
;;; (in cpool, while the wh-comp is pending): it builds the pp, drops a TRACE as
;;; the preposition's object, BINDS that trace to the wh-comp, and marks the
;;; wh-comp utilized; PP-UNDER-VP-1 then attaches the pp under the VP, and
;;; WH-RESOLVED finishes.
;;;
;;; Mostly dictionary-driven (who/did/the/boy/to/? from defs.l); the
;;; intransitive verb `talk' (with a `to'->neut pp slot) is supplied with a
;;; small df, as it is absent from the dictionary.
;;;
;;; NB on case frames: the verb's AGENT is filled (the boy). The stranded
;;; preposition's object is the wh-gap, captured as a trace BOUND to the
;;; wh-element -- that binding is what carries the referent. This runtime does
;;; not fill a thematic case from a pp's object at attach time (the existing
;;; imperative-pp test likewise only checks pp *attachment*), so talk's neut
;;; stays an open slot here; the test asserts the stranding structure + the
;;; binding, not a filled neut.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-wh-stranding-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-wh-stranding-test (&optional verbose)
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

      ;; who/did/the/boy/to/? from the dictionary; `talk' hand-built.
      (reset-rule-table)
      (df talk feats (verb mainverb pres tnsless v-3s) cf (neut agt) neut (all)
               markers (act) preps (to (neut)))
      (expandsim 'talk)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*
                        *pronoun-rule* *wh-question-rules* *object-wh-rules*
                        *pp-rules* *wh-pp-rules*)

      (let ((ok (parse-sentence "who did the boy talk to ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"who did the boy talk to ?\"" ok t)
        (truthy "S is a major wh-question" (subsetp '(major quest wh-quest s) (fe c)))

        (let ((whc (getr :wh-comp c)))
          (truthy "the wh-element is the `who' pron-np"
                  (and whc (subsetp '(pron-np wh) (fe whc))))
          (truthy "the wh-comp is marked utilized"
                  (and whc (member 'utilized (fe whc))))

          ;; Subject is the inverted, overt `the boy'.
          (let ((subj (daughter 'np c)))
            (truthy "the subject is overt (not a trace)"
                    (and subj (not (member 'trace (fe subj)))))
            (check "the subject is \"boy\"" (word-of subj) 'boy))

          (let* ((vp (daughter 'vp c))
                 (pp (and vp (daughter 'pp vp))))
            (truthy "a VP attached to S" vp)
            (check "the verb is \"talk\""
                   (and vp (getr 'word (daughter 'verb vp))) 'talk)

            ;; The stranding: a PP under the VP whose preposition is `to' and
            ;; whose object is a trace bound to the fronted wh-element.
            (truthy "a stranded PP attached under the VP" pp)
            (check "the stranded preposition is \"to\""
                   (and pp (getr 'word (daughter 'prep pp))) 'to)
            (let ((ppobj (and pp (daughter 'np pp))))
              (truthy "the preposition's object is a trace" (and ppobj (member 'trace (fe ppobj))))
              (check "the stranded object trace is bound to the wh-element"
                     (and ppobj (getr 'binding ppobj)) whc))

            (truthy "final (question) punctuation attached" (daughter 'finalpunc c))

            ;; The verb's AGENT is the (overt) subject.
            (let ((cf (and vp (getr 'caseframe vp))))
              (truthy "VP has a case frame" cf)
              (check "predicate is talk" (and cf (get cf 'pred)) 'talk)
              (closeframe openframe)
              (let* ((hyps (get cf 'hypo-slots))
                     (agt  (assoc 'agt (cadr (first hyps)))))
                (truthy "an agent case was filled" agt)
                (check "the agent is the subject NP (the boy)"
                       (cadr agt) (daughter 'np c))))))))

    (format t "~&gram1-wh-stranding-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-wh-stranding-test t)
