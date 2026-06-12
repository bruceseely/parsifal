;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram4-day-of-week-test.lisp
;;;
;;; "monday ." parsed as a day-of-week date NP -- the first of the gram4
;;; date/time constructions wired into the clause grammar. A bare day name is
;;; not a plain noun: the MONDAY rule builds a complex-noun TIME NP and records
;;; the date semantics on it.
;;;
;;;   [S np-utterance [NP prop-np/time [NBAR time
;;;       [NOUN complex-np/propnoun  time=dow  dow=monday  markers=(time)
;;;          [NP complex-noun-np [NOUN monday]]]]] .]
;;;
;;; So the outer complex noun carries the interpretation (it is a `dow' date
;;; whose day is `monday') while still dominating the surface word. This is the
;;; same complex-noun shape the month/date and clock-time rules use.
;;;
;;; New: the *day-of-week-rules* group (gram4:202 MONDAY). Tested as an NP
;;; utterance, the cleanest way to exercise the NP construction on its own; the
;;; resulting TIME NP composes wherever a time NP is wanted (PP object, adjunct).
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram4-day-of-week-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram4-day-of-week-test (&optional verbose)
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

      ;; The whole NP/clause machinery plus the day-of-week rule (which is not
      ;; in *full-grammar* -- it would reanalyse `friday' in "for friday").
      (load-full-grammar)
      (register-grammar *day-of-week-rules*)

      ;; Sanity: `monday' is a day-of-week time word in the dictionary.
      (expandsim 'monday)
      (truthy "dict `monday' is day-of-week" (member 'day-of-week (get 'monday 'features)))

      (let ((ok (parse-sentence "monday ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"monday .\"" ok t)
        (truthy "no BAD constituent" (notany (lambda (n) (member 'bad (fe n))) *nodelist*))
        (truthy "S is an NP-utterance" (member 'np-utterance (fe c)))

        (let* ((np   (daughter 'np c))
               (nbar (and np (daughter 'nbar np)))
               (noun (and nbar (daughter 'noun nbar))))
          (truthy "the utterance NP is a TIME prop-np"
                  (and np (subsetp '(prop-np time) (fe np))))
          (truthy "the NBAR is a time NBAR" (and nbar (member 'time (fe nbar))))

          ;; The outer complex noun carries the date interpretation.
          (truthy "the head is a complex-np propnoun"
                  (and noun (subsetp '(time complex-np propnoun) (fe noun))))
          (check "its TIME register is `dow' (a day-of-week date)"
                 (and noun (getr 'time noun)) 'dow)
          (truthy "its DOW register is the day word `monday'"
                  (and noun (let ((d (getr 'dow noun)))
                              (and d (eq (getr 'word d) 'monday)))))
          (check "its markers register is (time)"
                 (and noun (getr 'markers noun)) '(time))

          ;; The surface word is still dominated, inside the complex-noun-np.
          (let ((inner-np (and noun (daughter 'np noun))))
            (truthy "the complex noun dominates a complex-noun-np"
                    (and inner-np (member 'complex-noun-np (fe inner-np))))
            (check "whose noun is the surface word `monday'"
                   (and inner-np (getr 'word (daughter 'noun inner-np))) 'monday))

          (truthy "final punctuation attached" (daughter 'finalpunc c)))))

    (format t "~&gram4-day-of-week-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram4-day-of-week-test t)
