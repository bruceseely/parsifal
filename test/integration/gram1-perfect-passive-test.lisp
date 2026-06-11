;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-perfect-passive-test.lisp
;;;
;;; The canonical example sentence "A meeting seems to have been scheduled for
;;; Friday." (notes/pidgin-grammar-intro.text), parsed end to end and fully
;;; dict-driven. Subject-RAISING (`seem') over a PERFECT-PASSIVE infinitive
;;; complement -- i.e.
;;;
;;;     a meeting_i seems [ t_i to have been scheduled t_i for friday ]
;;;
;;; `seem' is a NO-SUBJ raising verb; with the overt `to' its complement parses
;;; directly as an embedded infinitive (no INSERT-TO-BE). The embedded aux is a
;;; PERFECT-PASSIVE stack built in BUILD-AUX:
;;;   - TO-INFINITIVE consumes `to' (inf aux);
;;;   - PERFECTIVE  [=*have][=en] consumes `have' (`have' + `been'): perf;
;;;   - PASSIVE-AUX [=*be][=en]   consumes `been' (`been' + `scheduled'): passive;
;;; the embedded verb `scheduled' is then passive, so embedded PASSIVE preposes
;;; the embedded subject (itself the raising trace) and leaves an OBJECT trace --
;;; so `a meeting' is the thing scheduled. `for friday' fills schedule's TIME.
;;;
;;; The ONLY new rule vs. "The jar seems broken." + "Is there a meeting
;;; scheduled for friday?" is PERFECTIVE (the perfect aux). Everything else
;;; composes: *raising-rules* (SEEMS/PASSIVE/PASSIVE-AUX), *inf-complement-rules*
;;; (TO-INFINITIVE/INF-S-START1/...), *pp-rules*, the clause + NP layers.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-perfect-passive-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-perfect-passive-test (&optional verbose)
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

      ;; Fully dict-driven: a/meeting/seems/to/have/been/scheduled/for/friday/.
      ;; all resolve through morpho + defs.l (`been' -> *be/en, `scheduled' ->
      ;; *schedule/en, `seems' -> *seem/no-subj/to-be-less-inf-obj).
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*
                        *qp1-done-rule* *inf-complement-rules*
                        *raising-rules* *perfect-rules* *pp-rules*)

      (let ((ok (parse-sentence "a meeting seems to have been scheduled for friday ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on the canonical sentence" ok t)

        ;; Matrix clause: raising verb `seems', surface subject preposed.
        (truthy "matrix S is a major declarative" (subsetp '(decl major s) (fe c)))
        (truthy "matrix S is np-preposed (raising)" (member 'np-preposed (fe c)))
        (check "the surface subject is \"a meeting\"" (noun-of (daughter 'np c)) 'meeting)

        (let* ((vp     (daughter 'vp c))
               (compnp (and vp (daughter 'np vp))))
          (truthy "a VP attached to S" vp)
          (check "matrix verb is \"seem\""
                 (and vp (getr 'word (daughter 'verb vp))) 'seems)

          ;; The complement is the embedded clause, wrapped as a comp-np.
          (truthy "the matrix object is a comp-np" (and compnp (member 'comp-np (fe compnp))))
          (check "the comp-np is marked inf-comp"
                 (and compnp (getr 'markers compnp)) '(inf-comp))

          (let* ((embs  (and compnp (daughter 's compnp)))
                 (esubj (and embs (daughter 'np embs)))
                 (eaux  (and embs (daughter 'aux embs)))
                 (evp   (and embs (daughter 'vp embs))))
            (truthy "the comp-np dominates an embedded S" embs)
            (truthy "the embedded S is a comp-s / inf-s"
                    (and embs (subsetp '(sec comp-s inf-s s) (fe embs))))
            (truthy "the embedded S is itself np-preposed (passive)"
                    (and embs (member 'np-preposed (fe embs))))

            ;; The raising: the embedded subject is a trace bound to the
            ;; surface subject `a meeting'.
            (truthy "the embedded subject is a trace" (and esubj (member 'trace (fe esubj))))
            (check "the embedded trace is bound to the surface subject NP"
                   (and esubj (getr 'binding esubj)) (daughter 'np c))

            ;; The embedded aux is the perfect-passive infinitive stack.
            (truthy "the embedded aux is an inf aux" (and eaux (member 'inf (fe eaux))))
            (check "the embedded aux carries `to'"
                   (and eaux (getr 'word (daughter 'to eaux))) 'to)
            (truthy "the embedded aux is labelled perf" (and eaux (member 'perf (fe eaux))))
            (check "the embedded aux's perf daughter is `have'"
                   (and eaux (getr 'word (daughter 'perf eaux))) 'have)
            (truthy "the embedded aux has a passive daughter" (and eaux (daughter 'passive eaux)))
            (check "the embedded aux's passive daughter is `been'"
                   (and eaux (getr 'word (daughter 'passive eaux))) 'been)

            ;; The embedded predicate is the passive participle `scheduled',
            ;; with an object trace + the `for friday' PP.
            (let ((everb (and evp (daughter 'verb evp)))
                  (eobj  (and evp (daughter 'np evp)))
                  (epp   (and evp (daughter 'pp evp))))
              (check "the embedded verb is \"schedule\"" (and everb (getr 'word everb)) 'scheduled)
              (truthy "the embedded verb is passive" (and everb (member 'passive (fe everb))))
              (truthy "the embedded object is a trace" (and eobj (member 'trace (fe eobj))))
              (truthy "the embedded object trace is bound" (and eobj (getr 'binding eobj)))
              (truthy "a PP attached under the embedded VP" epp)
              (check "the PP preposition is \"for\""
                     (and epp (getr 'word (daughter 'prep epp))) 'for)
              (check "the PP object is \"friday\"" (and epp (noun-of (daughter 'np epp))) 'friday)

              ;; Embedded case frame (finalised by EMBEDDED-S-DONE): NEUT = the
              ;; object trace (the scheduled thing), TIME = friday via `for',
              ;; no agent (passive).
              (let ((ecf (and evp (getr 'caseframe evp))))
                (truthy "the embedded VP has a case frame" ecf)
                (check "the embedded predicate is schedule" (and ecf (get ecf 'pred)) 'schedule)
                (let ((filled (cadr (first (and ecf (get ecf 'hypo-slots))))))
                  (truthy "the embedded frame has filled cases" filled)
                  (check "no agent case (schedule is passive)" (assoc 'agt filled) nil)
                  (let ((neut (assoc 'neut filled)) (time (assoc 'time filled)))
                    (truthy "a neutral case was filled (the scheduled thing)" neut)
                    (check "the neutral case is the embedded object trace" (cadr neut) eobj)
                    (truthy "a time case was filled" time)
                    (check "the time case is `friday'" (noun-of (cadr time)) 'friday)
                    (check "the time case was filled via the preposition `for'"
                           (caddr time) 'for)))))

            (truthy "final punctuation attached to S" (daughter 'finalpunc c))

            ;; Matrix case frame: seem is no-subj (no agent); the embedded
            ;; clause fills its neutral/object case.
            (let ((cf (and vp (getr 'caseframe vp))))
              (truthy "matrix VP has a case frame" cf)
              (check "matrix predicate is seem" (and cf (get cf 'pred)) 'seem)
              (closeframe openframe)
              (let ((filled (cadr (first (get cf 'hypo-slots)))))
                (truthy "the matrix frame has filled cases" filled)
                (check "no agent case was filled (seem is no-subj)" (assoc 'agt filled) nil)
                (let ((neut (assoc 'neut filled)))
                  (truthy "a neutral case was filled" neut)
                  (check "the matrix neutral case is the comp-np (the embedded clause)"
                         (cadr neut) compnp))))))))

    (format t "~&gram1-perfect-passive-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-perfect-passive-test t)
