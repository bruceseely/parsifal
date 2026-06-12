;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram4-month-date-test.lisp
;;;
;;; "june 1st ." parsed as a calendar-date NP -- the gram4 month/date
;;; construction. A month name plus an ordinal builds a complex-noun DATE NP:
;;;
;;;   [S np-utterance [NP prop-np/time [NBAR time
;;;       [NOUN date/complex-np  time=date  month=june  day=1
;;;          [NP complex-noun-np [NOUN june] [QP [ord 1st]]]]]] .]
;;;
;;; The outer complex noun carries the interpretation -- a `date' whose month is
;;; `june' and whose day is 1 -- with the surface month word and the day ordinal
;;; dominated underneath. The ordinal `1st' is recognised by morpho (1+st) and
;;; labelled a `complete-num' by the number grammar (*number-rules*); `first'
;;; works identically, as do "june the 1st" and "the first of june".
;;;
;;; New: the *month-date-rules* group (gram4:142-191: MONTH, JUNE-1ST,
;;; JUNE-THE-1ST, THE-FIRST-OF-JUNE, MONTH-COMPLETE). Composed on top of
;;; *full-grammar* (for the NP machinery + *number-rules*); not in it (months in
;;; other roles / `may' ambiguity). Tested as an NP utterance.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram4-month-date-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram4-month-date-test (&optional verbose)
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

      (load-full-grammar)
      (register-grammar *month-date-rules*)

      ;; Sanity: `june' is a month time word in the dictionary.
      (expandsim 'june)
      (truthy "dict `june' is a month" (member 'month (get 'june 'features)))

      (let ((ok (parse-sentence "june 1st ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"june 1st .\"" ok t)
        (truthy "no BAD constituent" (notany (lambda (n) (member 'bad (fe n))) *nodelist*))
        (truthy "S is an NP-utterance" (member 'np-utterance (fe c)))

        (let* ((np   (daughter 'np c))
               (nbar (and np (daughter 'nbar np)))
               (noun (and nbar (daughter 'noun nbar))))
          (truthy "the utterance NP is a TIME prop-np"
                  (and np (subsetp '(prop-np time) (fe np))))

          ;; The outer complex noun carries the date interpretation.
          (truthy "the head is a DATE complex-np"
                  (and noun (subsetp '(date time complex-np) (fe noun))))
          (check "its TIME register is `date'" (and noun (getr 'time noun)) 'date)
          (truthy "its MONTH register is the month word `june'"
                  (and noun (let ((m (getr 'month noun)))
                              (and m (eq (getr 'word m) 'june)))))
          (check "its DAY register is 1" (and noun (getr 'day noun)) 1)
          (check "its markers register is (time)"
                 (and noun (getr 'markers noun)) '(time))

          ;; Inside: the month word and the day ordinal.
          (let* ((inner-np (and noun (daughter 'np noun)))
                 (qp       (and inner-np (daughter 'qp inner-np))))
            (truthy "the complex noun dominates a complex-noun-np"
                    (and inner-np (member 'complex-noun-np (fe inner-np))))
            (check "whose noun is the month word `june'"
                   (and inner-np (getr 'word (daughter 'noun inner-np))) 'june)
            (truthy "and a qp holding the day ordinal" qp)
            (let ((ord (and qp (daughter 'ord qp))))
              (truthy "the qp's ordinal is an ord" (and ord (member 'ord (fe ord))))
              (check "the day ordinal's word is `1st'" (and ord (getr 'word ord)) '1st)))

          (truthy "final punctuation attached" (daughter 'finalpunc c)))))

    (format t "~&gram4-month-date-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram4-month-date-test t)
