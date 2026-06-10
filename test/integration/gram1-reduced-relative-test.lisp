;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-reduced-relative-test.lisp
;;;
;;; A REDUCED relative clause -- one with no overt relative pronoun -- parsed
;;; end to end: "the boy you met ." (an NP utterance).
;;;
;;;     [NP the boy_i  [S-rel  wh-_i  [ you  met  t_i ] ]]
;;;
;;; The input has no relativizer ("the boy YOU met", not "the boy WHO you
;;; met"). When the NP `the boy' completes and is followed by an NP + verb
;;; (rather than a relative pronoun), REDUCED-RELATIVE (in NP-COMPLETE) inserts
;;; a dummy relativizer `wh-' before the following NP; from there the parse is
;;; the ordinary object relative: `wh-' becomes the relative clause's whcomp
;;; bound to the head, `you' is the overt relative subject, and the object gap
;;; is a trace bound to `wh-' (chain trace -> wh- -> the boy). So "you met the
;;; boy".
;;;
;;; `wh-' is a system dummy word (not in the dictionary, only ever inserted by
;;; REDUCED-RELATIVE), so it is supplied here with a small df modelled on the
;;; relative pronoun `who'. Everything else is dictionary-driven.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-reduced-relative-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-reduced-relative-test (&optional verbose)
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
           (noun-of (np) (and np (getr 'word (daughter 'noun (daughter 'nbar np)))))
           (pron-of (np) (and np (getr 'word (daughter 'pronoun np)))))

      ;; the/boy/you/met from defs.l; `wh-' is the system dummy relativizer.
      (reset-rule-table)
      (df |WH-| feats (pronoun relpron wh ns npl n3p) markers (hanim))
      (expandsim '|WH-|)
      (register-grammar *np-rules* *np-utterance-rule* *pronoun-rule*
                        *clause-rules* *vp-np-rule* *inf-complement-rules*
                        *wh-question-rules* *object-wh-rules*
                        *relative-clause-rules*)

      (let ((ok (parse-sentence "the boy you met ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"the boy you met .\"" ok t)
        (truthy "the utterance is an NP utterance" (subsetp '(np-utterance s) (fe c)))

        (let ((np (daughter 'np c)))
          (truthy "the utterance NP is present" np)
          (check "the head noun is \"boy\"" (noun-of np) 'boy)
          (truthy "the head NP is `modified' (carries a reduced relative)"
                  (and np (member 'modified (fe np))))

          (let* ((rel     (and np (daughter 's np)))
                 (whc     (and rel (getr :wh-comp rel)))
                 (relsubj (and rel (daughter 'np rel)))
                 (relvp   (and rel (daughter 'vp rel)))
                 (relobj  (and relvp (daughter 'np relvp))))
            (truthy "a relative S is attached to the head NP" rel)
            (truthy "the relative S is sec/relative"
                    (and rel (subsetp '(sec relative s) (fe rel))))

            ;; The relativizer is the INSERTED dummy `wh-', not an input word.
            (truthy "the relative clause has a wh-comp" whc)
            (check "the relativizer is the inserted dummy `wh-'"
                   (and whc (pron-of whc)) (intern "WH-" :parsifal))
            (check "the relativizer is bound to the head NP (the boy)"
                   (and whc (getr 'binding whc)) np)

            ;; Overt subject `you'; object gap is a trace bound to the dummy.
            (truthy "the relative subject is overt (not a trace)"
                    (and relsubj (not (member 'trace (fe relsubj)))))
            (check "the relative subject is \"you\"" (pron-of relsubj) 'you)
            (check "the relative verb is \"meet\""
                   (and relvp (getr 'word (daughter 'verb relvp))) 'met)
            (truthy "the relative object is a trace" (and relobj (member 'trace (fe relobj))))
            (check "the relative object trace is bound to the relativizer"
                   (and relobj (getr 'binding relobj)) whc)
            (truthy "the wh-comp is marked utilized" (and whc (member 'utilized (fe whc))))

            ;; Relative case frame: `you' is AGENT, the object trace is NEUT.
            (let ((rcf (and relvp (getr 'caseframe relvp))))
              (truthy "the relative VP has a case frame" rcf)
              (check "the relative predicate is meet" (and rcf (get rcf 'pred)) 'meet)
              (closeframe openframe)
              (let ((filled (cadr (first (get rcf 'hypo-slots)))))
                (truthy "the relative frame has filled cases" filled)
                (let ((agt (assoc 'agt filled)) (neut (assoc 'neut filled)))
                  (truthy "an agent case was filled" agt)
                  (check "the relative agent is `you'" (cadr agt) relsubj)
                  (truthy "a neutral case was filled" neut)
                  (check "the relative neutral is the object trace (the head)"
                         (cadr neut) relobj))))))

        (truthy "final punctuation attached" (daughter 'finalpunc c))))

    (format t "~&gram1-reduced-relative-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-reduced-relative-test t)
