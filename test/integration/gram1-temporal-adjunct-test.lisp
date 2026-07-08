;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-temporal-adjunct-test.lisp
;;;
;;; "You scheduled the meeting yesterday." parsed end to end -- a bare temporal
;;; adjunct filling the verb's TIME case. A bare time NP cannot fill TIME
;;; positionally (TIME is not an object case), so the gram4 TIME-NP-TO-PP rule
;;; rewrites `yesterday' -> `during yesterday', a PP that attaches to the VP and
;;; fills TIME through `during' (which is cases-marked-by time):
;;;
;;;     you scheduled the meeting yesterday
;;;       agt = you, neut = the meeting, time = yesterday (via inserted `during')
;;;
;;; `schedule' carries a TIME case in its frame (cf (neut (time) (loc) agt)),
;;; which is what makes this clean: the `during'-PP attaches under the VP and the
;;; VP-PP crule fills TIME. (Marcus's literal "give Sue yesterday" examples use
;;; `give', which has no TIME slot -- there the PP attaches at the S level and
;;; TIME does not fill; that case stays deferred. See *temporal-adjunct-rules*.)
;;;
;;; New: the *temporal-adjunct-rules* group (TIME-NP-TO-PP, gram4:277). It is
;;; deliberately NOT in *full-grammar* -- Marcus flagged it "needs to be
;;; controlled, but right zeroeth approx", and it misfires on the object of an
;;; existing time PP (it would BAD-flag "for friday"), so it is only composed
;;; here. `yesterday' is hand-defined as a pseudopropnoun (Marcus's df1 feature
;;; set, which in the shipped dictionary is dead code: a "MM 4/78 hack"
;;; `jlike yesterday wednesday' already defines the word, so the define-if-new
;;; df1 never applies and the bare NP would otherwise be flagged BAD).
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-temporal-adjunct-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-temporal-adjunct-test (&optional verbose)
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
           (np-word (np)
             (and np (or (getr 'word (daughter 'noun (daughter 'nbar np)))
                         (getr 'word (daughter 'pronoun np))))))

      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*
                        *pronoun-rule* *qp1-done-rule* *pp-rules*
                        *temporal-adjunct-rules*)

      ;; `yesterday' as a pseudopropnoun time word (Marcus's df1 feature set;
      ;; the shipped dict's "MM 4/78 hack" jlike masks it -- see file header).
      (setf (symbol-plist 'yesterday) nil)
      (df yesterday feats (noun ns n3p time pseudopropnoun) markers (time))
      (expandsim 'yesterday)
      (truthy "`yesterday' is a time word" (member 'time (get 'yesterday 'features)))
      (truthy "dict `schedule' carries a TIME case"
              (assoc 'time (progn (expandsim 'schedule) (get 'schedule 'case-frame))))

      (let ((ok (parse-sentence "you scheduled the meeting yesterday ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds" ok t)
        (truthy "no BAD constituent (the bare time NP was diverted cleanly)"
                (notany (lambda (n) (member 'bad (fe n))) *nodelist*))
        (truthy "TIME-NP-TO-PP fired" (member 'time-np-to-pp *deriv*))
        (truthy "main S is a major declarative" (subsetp '(decl major s) (fe c)))
        (check "the subject is `you'" (np-word (daughter 'np c)) 'you)

        (let* ((vp  (daughter 'vp c))
               (obj (and vp (daughter 'np vp)))
               (pps (and vp (daughters 'pp vp)))
               (pp  (car pps)))
          (check "the verb is `schedule' (scheduled)"
                 (and vp (getr 'word (daughter 'verb vp))) 'scheduled)
          (check "the object is `the meeting'" (np-word obj) 'meeting)

          ;; The bare time NP was rewritten to a `during'-PP and attached.
          (truthy "the VP has exactly one PP" (and pps (= (length pps) 1)))
          (check "the inserted preposition is `during'"
                 (and pp (getr 'word (daughter 'prep pp))) 'during)
          (check "the PP's object is `yesterday'" (np-word (and pp (daughter 'np pp))) 'yesterday)
          (truthy "final punctuation attached" (daughter 'finalpunc c))

          ;; Case frame: you scheduled the meeting, time = yesterday via `during'.
          (let ((cf (and vp (getr 'caseframe vp))))
            (check "predicate is schedule" (and cf (get cf 'pred)) 'schedule)
            (closeframe openframe)
            (let ((filled (cadr (first (get cf 'hypo-slots)))))
              (let ((agt (assoc 'agt filled)) (neut (assoc 'neut filled)) (time (assoc 'time filled)))
                (truthy "agent filled" agt)
                (check "the agent is `you'" (cadr agt) (daughter 'np c))
                (truthy "neutral (theme) filled" neut)
                (check "the theme is `the meeting'" (np-word (cadr neut)) 'meeting)
                ;; The point of the test: the bare adjunct filled TIME.
                (truthy "a TIME case was filled" time)
                (check "the time is `yesterday'" (np-word (cadr time)) 'yesterday)
                (check "the time was filled via the inserted preposition `during'"
                       (caddr time) 'during)))))))

    (format t "~&gram1-temporal-adjunct-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-temporal-adjunct-test t)
