;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-wh-pp-next-test.lisp
;;;
;;; "What did Bob change to friday?" parsed end to end, FULLY DICT-DRIVEN -- the
;;; in-situ PP wh-vp case (WH-WITH-PP-NEXT). After the verb leaves the wh-comp
;;; unutilized, the buffer is a PREP followed by an NP, [to][friday]:
;;;
;;;     what_i did Bob change t_i to friday    agt = Bob, neut = what, time = friday
;;;
;;; WH-WITH-PP-NEXT (priority 5, the most specific wh-vp placement rule) fires
;;; on [=prep][=np]. Its first test asks whether [1st-prep + 2nd-np] is a plain
;;; PP that fits one of the verb's pp slots -- here `to friday' fits `change's
;;; TIME slot (change: `to' marks time/loc) -- so it runs PP to build the simple
;;; PP and leaves the wh-comp to fall to a verb object. The wh-comp (`what')
;;; then spends on `change's NEUT slot (CREATE-WH-TRACE, a theme trace bound to
;;; the wh-element). Result: the theme is the wh-gap, `to friday' is a TIME PP.
;;;
;;; Contrast with the siblings already in *ditransitive-wh-rules*:
;;; WH-WITH-NP-NEXT ([np], "What did Bob give Sue?") and WH-WITH-NP-PP-NEXT
;;; ([np][prep], "What did Bob send Sue on friday?"). WH-WITH-PP-NEXT is the
;;; [prep][np] case. The single new rule is WH-WITH-PP-NEXT; everything else
;;; composes (*pp-rules* for the PP build + VP-PP case fill, *wh-pp-rules* for
;;; the WH-PP-BUILD branch, *object-wh-rules* for aux-inversion / WH-RESOLVED).
;;;
;;; NB this is the IN-SITU PP case. PP *fronting* -- the pied-piped "To whom did
;;; Bob give it?" -- is a different, not-yet-built construction: nothing
;;; assembles a clause-initial wh-PP for WH-QUEST's pp-quest branch to front.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-wh-pp-next-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-wh-pp-next-test (&optional verbose)
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

      ;; Fully dict-driven: what/did/bob/change/to/friday/? all resolve through
      ;; morpho + defs.l. `change' carries a TIME/LOC pp frame with `to' marking
      ;; time/loc (cf (neut (time) (loc) agt), preps (to (time loc))).
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*
                        *pronoun-rule* *qp1-done-rule* *wh-question-rules*
                        *inversion-rules* *object-wh-rules*
                        *ditransitive-wh-rules* *wh-pp-rules*
                        *wh-determiner-rules* *pp-rules*)

      ;; Sanity: `change' really licenses a `to'->time/loc PP in the dictionary.
      (expandsim 'change)
      (truthy "dict `change' takes a `to' PP" (member 'to (get 'change 'preps)))

      (let ((ok (parse-sentence "what did bob change to friday ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"what did bob change to friday ?\"" ok t)
        (truthy "WH-WITH-PP-NEXT fired (the [prep][np] wh-vp rule)"
                (member 'wh-with-pp-next *deriv*))
        (truthy "S is a major wh-question" (subsetp '(major quest wh-quest s) (fe c)))

        (let ((whc (getr :wh-comp c)))
          (truthy "the S has a :wh-comp register (the fronted `what')" whc)
          (truthy "the wh-comp is marked utilized" (and whc (member 'utilized (fe whc))))

          (let ((subj (daughter 'np c)))
            (check "the subject is the inverted overt `bob'" (word-of subj) 'bob)

            (let* ((vp    (daughter 'vp c))
                   (nps   (and vp (daughters 'np vp)))
                   (trace (find-if (lambda (n) (member 'trace (fe n))) nps))
                   (pps   (and vp (daughters 'pp vp)))
                   (pp    (car pps)))
              (truthy "a VP attached to S" vp)
              (check "the verb is `change'" (and vp (getr 'word (daughter 'verb vp))) 'change)

              ;; One object NP -- the wh-gap trace (the theme) -- plus the PP.
              (truthy "the VP's object is a single trace (the wh-gap)"
                      (and nps (= (length nps) 1) trace))
              (check "the wh-gap trace is bound to the wh-element (what)"
                     (and trace (getr 'binding trace)) whc)

              (truthy "the VP has exactly one PP" (and pps (= (length pps) 1)))
              (check "the PP's preposition is `to'"
                     (and pp (getr 'word (daughter 'prep pp))) 'to)
              (check "the PP's object is `friday'" (word-of (and pp (daughter 'np pp))) 'friday)
              (truthy "final (question) punctuation attached" (daughter 'finalpunc c))

              ;; Case frame: agt = Bob, neut = wh-gap (theme), time = friday.
              (let ((cf (and vp (getr 'caseframe vp))))
                (truthy "the VP has a case frame" cf)
                (check "the predicate is change" (and cf (get cf 'pred)) 'change)
                (closeframe openframe)
                (let ((filled (cadr (first (get cf 'hypo-slots)))))
                  (truthy "the frame has filled cases" filled)
                  (let ((agt  (assoc 'agt filled))
                        (neut (assoc 'neut filled))
                        (time (assoc 'time filled)))
                    (truthy "an agent case was filled" agt)
                    (check "the agent is the subject `bob'" (cadr agt) subj)
                    (check "the agent was filled via the subj function" (caddr agt) 'subj)
                    (truthy "a neutral (theme) case was filled" neut)
                    (check "the theme is the wh-gap trace" (cadr neut) trace)
                    (truthy "a time case was filled" time)
                    (check "the time is `friday'" (word-of (cadr time)) 'friday)
                    (check "the time was filled via the preposition `to'"
                           (caddr time) 'to)))))))))

    (format t "~&gram1-wh-pp-next-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-wh-pp-next-test t)
