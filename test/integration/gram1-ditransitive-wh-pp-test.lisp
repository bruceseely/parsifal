;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-ditransitive-wh-pp-test.lisp
;;;
;;; "What did Bob send Sue on friday?" parsed end to end -- the wh-vp PP-placement
;;; case (WH-WITH-NP-PP-NEXT). Extends the ditransitive wh-question ("What did
;;; Bob give Sue?") with an adjunct PP after the recipient:
;;;
;;;     what_i did Bob send Sue t_i on friday
;;;       agt = Bob, dat = Sue, neut = what (the wh-gap/theme), time = friday
;;;
;;; The crux is the WH-VP discrimination. With the wh-comp (`what') unutilized
;;; after MAIN-VERB, the buffer is [Sue][on ...] -- an NP followed by a PREP --
;;; so the more specific WH-WITH-NP-PP-NEXT (priority 7) fires instead of the
;;; bare-NP WH-WITH-NP-NEXT. It first asks whether the wh-comp belongs in that
;;; PP -- would [on + wh-comp] fit a pp slot? (pgof + `fits a pp slot'). `what'
;;; is markerless and does not fit the TIME slot, so the PP is a separate
;;; adjunct: with two object slots open, WH-WITH-NP-PP-NEXT delegates to
;;; WH-WITH-NP-NEXT, which attaches `Sue' to the recipient (DAT) slot and keeps
;;; the wh-comp; WH-WITH-END-NEXT then spends the wh-comp on the remaining
;;; NEUT/theme slot (CREATE-WH-TRACE), and `on friday' attaches as a VP PP
;;; (PP-UNDER-VP + the VP-PP case-fill crule) filling the TIME case via `on'.
;;;
;;; The single new rule vs. the ditransitive wh case is WH-WITH-NP-PP-NEXT
;;; (added to *ditransitive-wh-rules*); everything else composes
;;; (*object-wh-rules*, *wh-question-rules*, *inversion-rules*, *pp-rules*).
;;;
;;; `send' is hand-built (like `talk' in the wh-stranding test) because no
;;; dictionary ditransitive verb carries a temporal PP slot: it is `give's
;;; two-object frame plus a TIME case reachable through `on'. Everything else
;;; -- what / did / bob / sue / on / friday / ? -- is dictionary + morpho.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-ditransitive-wh-pp-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-ditransitive-wh-pp-test (&optional verbose)
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

      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*
                        *pronoun-rule* *qp1-done-rule* *wh-question-rules*
                        *inversion-rules* *object-wh-rules*
                        *ditransitive-wh-rules* *wh-determiner-rules* *pp-rules*)

      ;; `send' = give's two-object frame (DAT recipient + NEUT theme) plus a
      ;; TIME case reachable via the preposition `on'. Hand-built because the
      ;; dictionary's ditransitive verbs (give/tell) carry no temporal PP slot.
      (df send feats (mainverb pres tnsless v-3s)
           cf ((*obj dat) neut (dat) (time) agt)
           neut (|#| hanim |#| anim inanim physob)
           markers (act event time)
           preps (on (time)))
      (expandsim 'send)
      (expandsim 'on)
      (truthy "dict `on' is a preposition" (member 'prep (get 'on 'features)))

      (let ((ok (parse-sentence "what did bob send sue on friday ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"what did bob send sue on friday ?\"" ok t)
        (truthy "WH-WITH-NP-PP-NEXT fired (the [np][prep] wh-vp rule)"
                (member 'wh-with-np-pp-next *deriv*))
        (truthy "S is a major wh-question" (subsetp '(major quest wh-quest s) (fe c)))

        (let ((whc (getr :wh-comp c)))
          (truthy "the S has a :wh-comp register (the fronted `what')" whc)
          (truthy "the wh-comp is marked utilized" (and whc (member 'utilized (fe whc))))

          (let ((subj (daughter 'np c)))
            (check "the subject is the inverted overt `bob'" (word-of subj) 'bob)

            (let* ((vp    (daughter 'vp c))
                   (nps   (and vp (daughters 'np vp)))
                   (trace (find-if (lambda (n) (member 'trace (fe n))) nps))
                   (sue   (find-if (lambda (n) (eq (word-of n) 'sue)) nps))
                   (pps   (and vp (daughters 'pp vp)))
                   (pp    (car pps)))
              (truthy "a VP attached to S" vp)
              (check "the verb is `send'" (and vp (getr 'word (daughter 'verb vp))) 'send)

              ;; Two object NPs (overt Sue + the wh-gap trace) AND one adjunct PP.
              (truthy "the VP has two object NPs" (and nps (= (length nps) 2)))
              (truthy "one VP object is the overt `Sue'" sue)
              (truthy "one VP object is a trace (the wh-gap)" trace)
              (check "the wh-gap trace is bound to the wh-element (what)"
                     (and trace (getr 'binding trace)) whc)

              (truthy "the VP has exactly one PP" (and pps (= (length pps) 1)))
              (check "the PP's preposition is `on'"
                     (and pp (getr 'word (daughter 'prep pp))) 'on)
              (check "the PP's object is `friday'" (word-of (and pp (daughter 'np pp))) 'friday)
              (truthy "final (question) punctuation attached" (daughter 'finalpunc c))

              ;; Case frame: agt = Bob, dat = Sue, neut = wh-gap, time = friday.
              (let ((cf (and vp (getr 'caseframe vp))))
                (truthy "the VP has a case frame" cf)
                (check "the predicate is send" (and cf (get cf 'pred)) 'send)
                (closeframe openframe)
                (let ((filled (cadr (first (get cf 'hypo-slots)))))
                  (truthy "the frame has filled cases" filled)
                  (let ((agt  (assoc 'agt filled))
                        (dat  (assoc 'dat filled))
                        (neut (assoc 'neut filled))
                        (time (assoc 'time filled)))
                    (truthy "an agent case was filled" agt)
                    (check "the agent is the subject `bob'" (cadr agt) subj)
                    (truthy "a dative (recipient) case was filled" dat)
                    (check "the recipient is `sue'" (cadr dat) sue)
                    (truthy "a neutral (theme) case was filled" neut)
                    (check "the theme is the wh-gap trace" (cadr neut) trace)
                    (truthy "a time case was filled" time)
                    (check "the time is `friday'"
                           (word-of (cadr time)) 'friday)
                    (check "the time was filled via the preposition `on'"
                           (caddr time) 'on)))))))))

    (format t "~&gram1-ditransitive-wh-pp-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-ditransitive-wh-pp-test t)
