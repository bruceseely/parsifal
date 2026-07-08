;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-wh-question-test.lisp
;;;
;;; A subject wh-question, parsed end to end -- "Who broke the jar?" -- one
;;; of Marcus's canonical example sentences (notes/pidgin-grammar-intro.text)
;;; and the simplest interrogative form (no subject-aux inversion).
;;;
;;; Mechanism: `who' is built into a wh pron-np; WH-QUEST fronts it into the
;;; clause's whcomp and (via the S-WHCOMP crule) the S's :wh-comp register and
;;; labels the clause np-quest. The subject position is then empty, so the
;;; wh-pool catch-all CREATE-WH-TRACE drops a trace BOUND to the wh-comp into
;;; the subject slot and marks the wh-comp `utilized'; WH-RESOLVED-1 shuts off
;;; wh-pool. The clause finishes as an ordinary transitive clause, and the
;;; trace's binding (the wh-element `who') is what fills the verb's AGENT case:
;;;
;;;     who_i [ t_i broke the jar ]   ->  agent = who, neutral = the jar
;;;
;;; Mostly dictionary-driven: who/broke(break)/the/? all come from defs.l;
;;; only `jar' (absent from the dictionary) is supplied with a small df.
;;;
;;; This exercises the :wh-comp register, which required a glang-cl fix:
;;; `:'-prefixed register names now compile to the CL keyword the runtime
;;; reads (register-quote in denotations.lisp), so S-WHCOMP's write and
;;; setup-current-s's read agree.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-wh-question-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-wh-question-test (&optional verbose)
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

      ;; Mostly dict-driven; only `jar' is missing from defs.l.
      (reset-rule-table)
      (df jar feats (noun ns n3p) markers (physob inanim))
      (expandsim 'jar)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*
                        *pronoun-rule* *wh-question-rules*)

      ;; Sanity: `who' is a wh pronoun in the dictionary.
      (truthy "dict `who' is a wh pronoun" (subsetp '(pronoun wh) (get 'who 'feats)))

      (let ((ok (parse-sentence "who broke the jar ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"who broke the jar ?\"" ok t)

        ;; The clause is a wh-question (np-quest), with the wh-element held in
        ;; the :wh-comp register and marked utilized.
        (truthy "S is a major wh-question" (subsetp '(major quest wh-quest s) (fe c)))
        (truthy "S is np-quest" (member 'np-quest (fe c)))
        (let ((whc (getr :wh-comp c)))
          (truthy "the S has a :wh-comp register (the fronted wh-element)" whc)
          (truthy "the wh-element is the `who' pron-np"
                  (and whc (subsetp '(pron-np wh) (fe whc))))
          (truthy "the wh-comp is marked utilized"
                  (and whc (member 'utilized (fe whc))))

          ;; The subject is a trace bound to the wh-element (who).
          (let ((subj (daughter 'np c)))
            (truthy "the subject is a trace" (and subj (member 'trace (fe subj))))
            (check "the subject trace is bound to the wh-element"
                   (and subj (getr 'binding subj)) whc))

          (let* ((vp  (daughter 'vp c))
                 (obj (and vp (daughter 'np vp))))
            (truthy "a VP attached to S" vp)
            (check "the verb is \"break\""
                   (and vp (getr 'word (daughter 'verb vp))) 'broke)
            (check "the object is \"jar\"" (word-of obj) 'jar)
            (truthy "final (question) punctuation attached" (daughter 'finalpunc c))

            ;; Case frame: the wh-element (who) is the AGENT, the jar the NEUT.
            (let ((cf (and vp (getr 'caseframe vp))))
              (truthy "VP has a case frame" cf)
              (check "predicate is break" (and cf (get cf 'pred)) 'break)
              (closeframe openframe)
              (let ((filled (cadr (first (get cf 'hypo-slots)))))
                (truthy "the frame has filled cases" filled)
                (let ((agt (assoc 'agt filled)) (neut (assoc 'neut filled)))
                  (truthy "an agent case was filled" agt)
                  (check "the agent is the wh-element (who)" (cadr agt) whc)
                  (check "the agent was filled via the subj function" (caddr agt) 'subj)
                  (truthy "a neutral case was filled" neut)
                  (check "the neutral case is the object NP" (cadr neut) obj))))))))

    (format t "~&gram1-wh-question-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-wh-question-test t)
