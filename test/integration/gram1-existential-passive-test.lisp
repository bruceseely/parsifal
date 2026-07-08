;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-existential-passive-test.lisp
;;;
;;; The EXACT canonical example sentence "Is there a meeting scheduled for
;;; friday?" (notes/pidgin-grammar-intro.text), parsed end to end and fully
;;; dict-driven. An existential yes-no question with a PASSIVE predicate.
;;;
;;;     [S existential/np-preposed/quest/ynquest/major
;;;        [NP there]  is  [NP a meeting]_i  [VP scheduled [NP t_i] [PP for friday]] ]
;;;
;;; Pidgin analyses "a meeting scheduled for friday" NOT as a separate reduced
;;; relative but as the existential clause's passive predicate -- deep
;;; structure "a meeting is scheduled for friday" with there-insertion:
;;;   - the yes-no opener inverts `is' over the subject `there'; THERE attaches
;;;     `a meeting' as the existential clause's logical NP;
;;;   - `is scheduled' is a passive aux+verb (PASSIVE-AUX [=*be][=en]); PASSIVE
;;;     preposes the surface subject and leaves an object trace, so `a meeting'
;;;     is the OBJECT of `schedule' (the thing scheduled), bound by the trace;
;;;   - `for friday' fills schedule's TIME case via the preposition `for'.
;;;
;;; Pure composition -- no new rules. PASSIVE-AUX + PASSIVE come from
;;; *raising-rules* (built for "The jar seems broken."); the existential, pp,
;;; inversion and yes-no layers are all already in place.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-existential-passive-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-existential-passive-test (&optional verbose)
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

      ;; Fully dict-driven: is/there/a/meeting/scheduled/for/friday/? all defs.l.
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule* *pronoun-rule*
                        *qp1-done-rule* *inversion-rules* *yes-no-rules*
                        *there-rules* *pp-rules* *raising-rules*)

      (let ((ok (parse-sentence "is there a meeting scheduled for friday ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on the canonical sentence" ok t)

        ;; An existential, passive (np-preposed), yes-no question.
        (truthy "the clause is a yes-no question (quest/ynquest/major)"
                (subsetp '(quest ynquest major s) (fe c)))
        (truthy "the clause is `existential'" (member 'existential (fe c)))
        (truthy "the clause is `np-preposed' (passive)" (member 'np-preposed (fe c)))

        ;; Two NP daughters: existential `there' + logical `a meeting'.
        (let* ((nps (daughters 'np c))
               (there-np (find-if (lambda (n) (eq (noun-of n) 'there)) nps))
               (meeting-np (find-if (lambda (n) (eq (noun-of n) 'meeting)) nps)))
          (truthy "the existential subject `there' is present" there-np)
          (truthy "the logical NP `a meeting' is present" meeting-np)

          (let* ((vp   (daughter 'vp c))
                 (verb (and vp (daughter 'verb vp)))
                 (obj  (and vp (daughter 'np vp)))
                 (pp   (and vp (daughter 'pp vp))))
            (truthy "a VP attached to S" vp)
            ;; The predicate is the PASSIVE participle `scheduled'.
            (check "the predicate verb is \"schedule\"" (and verb (getr 'word verb)) 'scheduled)
            (truthy "the predicate verb is passive" (and verb (member 'passive (fe verb))))

            ;; `a meeting' is the OBJECT of schedule, via an object trace.
            (truthy "the VP object is a trace" (and obj (member 'trace (fe obj))))
            (check "the object trace is bound to `a meeting'"
                   (and obj (getr 'binding obj)) meeting-np)

            ;; `for friday' is a PP under the VP.
            (truthy "a PP attached under the VP" pp)
            (check "the PP preposition is \"for\""
                   (and pp (getr 'word (daughter 'prep pp))) 'for)
            (check "the PP object is \"friday\"" (and pp (noun-of (daughter 'np pp))) 'friday)

            (truthy "final (question) punctuation attached" (daughter 'finalpunc c))

            ;; Case frame: NEUTRAL = the meeting (scheduled thing, via the trace),
            ;; TIME = friday (via the preposition `for').
            (let ((cf (and vp (getr 'caseframe vp))))
              (truthy "the VP has a case frame" cf)
              (check "the predicate is schedule" (and cf (get cf 'pred)) 'schedule)
              (closeframe openframe)
              (let ((filled (cadr (first (get cf 'hypo-slots)))))
                (truthy "the frame has filled cases" filled)
                (let ((neut (assoc 'neut filled)) (time (assoc 'time filled)))
                  (truthy "a neutral case was filled (the scheduled thing)" neut)
                  (check "the neutral case is the object trace (-> a meeting)"
                         (cadr neut) obj)
                  (truthy "a time case was filled" time)
                  (check "the time case is `friday'" (noun-of (cadr time)) 'friday)
                  (check "the time case was filled via the preposition `for'"
                         (caddr time) 'for))))))))

    (format t "~&gram1-existential-passive-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-existential-passive-test t)
