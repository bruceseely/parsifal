;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-proper-noun-test.lisp
;;;
;;; A proper-noun subject in an object wh-question, parsed end to end -- the
;;; EXACT canonical example "Who did John see?" (notes/pidgin-grammar-intro.text).
;;;
;;;     who_i  did  John  see  t_i      ->  agent = John, neutral = who
;;;
;;; This is the object wh-question from gram1-object-wh-test, but with a PROPER
;;; NOUN subject. A [=name] word (`John') triggers the PROPNAME attention-shift,
;;; which builds a not-modifiable name NP (NAME attaches the word, END-OF-NAME
;;; finalises and drops it). That required adding `name' to the runtime
;;; *as-types* (defs.lisp): Marcus's list omits it, so a bare name word never
;;; triggered the shift -- the gap noted in the object-wh commit.
;;;
;;; Mostly dictionary-driven (who/did/see/? from defs.l); `John' is supplied
;;; with a small df (a proper name; not in the dictionary).
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-proper-noun-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-proper-noun-test (&optional verbose)
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
               (setf results (and pass results)))))

      ;; who/did/see/? from the dictionary; `John' (a proper name) hand-built.
      (reset-rule-table)
      (df john feats (name ns n3p) markers (hanim))
      (expandsim 'john)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*
                        *pronoun-rule* *wh-question-rules* *object-wh-rules*
                        *proper-noun-rules*)

      (let ((ok (parse-sentence "who did john see ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"who did john see ?\"" ok t)
        (truthy "S is a major wh-question" (subsetp '(major quest wh-quest s) (fe c)))

        ;; The subject is the proper-name NP `John' -- built by PROPNAME, overt
        ;; (not a trace), not-modifiable.
        (let ((subj (daughter 'np c)))
          (truthy "the subject NP is present" subj)
          (truthy "the subject is a name NP"
                  (and subj (subsetp '(name not-modifiable) (fe subj))))
          (truthy "the subject is overt (not a trace)"
                  (and subj (not (member 'trace (fe subj)))))
          (check "the name's noun is \"john\""
                 (and subj (getr 'word (daughter 'noun subj))) 'john))

        (let ((whc (getr :wh-comp c)))
          (truthy "the wh-element is the `who' pron-np"
                  (and whc (subsetp '(pron-np wh) (fe whc))))
          (truthy "the wh-comp is marked utilized"
                  (and whc (member 'utilized (fe whc))))

          (let* ((vp  (daughter 'vp c))
                 (obj (and vp (daughter 'np vp))))
            (truthy "a VP attached to S" vp)
            (check "the verb is \"see\""
                   (and vp (getr 'word (daughter 'verb vp))) 'see)
            (truthy "the aux carries do-support (`did')"
                    (let ((aux (daughter 'aux c)))
                      (and aux (getr 'word (daughter 'do aux)))))

            ;; The object is the wh-gap: a trace bound to the wh-element.
            (truthy "the object is a trace" (and obj (member 'trace (fe obj))))
            (check "the object trace is bound to the wh-element (who)"
                   (and obj (getr 'binding obj)) whc)
            (truthy "final (question) punctuation attached" (daughter 'finalpunc c))

            ;; Case frame: John is AGENT, the wh-gap (object trace) is NEUT.
            (let ((cf (and vp (getr 'caseframe vp))))
              (truthy "VP has a case frame" cf)
              (check "predicate is see" (and cf (get cf 'pred)) 'see)
              (closeframe openframe)
              (let ((filled (cadr (first (get cf 'hypo-slots)))))
                (truthy "the frame has filled cases" filled)
                (let ((agt (assoc 'agt filled)) (neut (assoc 'neut filled)))
                  (truthy "an agent case was filled" agt)
                  (check "the agent is the proper-noun subject (John)"
                         (cadr agt) (daughter 'np c))
                  (truthy "a neutral case was filled" neut)
                  (check "the neutral case is the object trace (the wh-gap)"
                         (cadr neut) obj))))))))

    (format t "~&gram1-proper-noun-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-proper-noun-test t)
