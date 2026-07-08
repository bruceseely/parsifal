;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram4-clock-time-test.lisp
;;;
;;; "2 o'clock ." parsed as a clock-time NP -- the gram4 hour-of-day
;;; construction. A number followed by a time postmod builds a complex-noun
;;; TIME NP recording the hour:
;;;
;;;   [S np-utterance [NP prop-np/time [NBAR time
;;;       [NOUN hour/complex-noun  time=hour  hours=2  minutes=0  markers=(time)
;;;          [NP complex-noun-np [hour 2] [oclock o'clock]]]]] .]
;;;
;;; TWO-OCLOCK handles "2 o'clock" / "2 a.m." / "2 p.m." and TWO-THIRTY handles
;;; "2:30" (the tokenizer splits `2:30' into num colon num); both open the
;;; build-hour packet and HOUR-COMPLETE finalises -- time `hour', hours = the
;;; hour num's quant, minutes = the minute num's quant or 0, time-of-day = the
;;; postmod.
;;;
;;; New: the *clock-time-rules* group (gram4:106-140: TWO-OCLOCK, TWO-THIRTY,
;;; HOUR-COMPLETE). Composed on top of *full-grammar*; not in it (the [=num]
;;; openers are npool-greedy). Tested as an NP utterance.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram4-clock-time-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram4-clock-time-test (&optional verbose)
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
      (register-grammar *clock-time-rules*)

      (let ((ok (parse-sentence "2 o'clock ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"2 o'clock .\"" ok t)
        (truthy "no BAD constituent" (notany (lambda (n) (member 'bad (fe n))) *nodelist*))
        (truthy "S is an NP-utterance" (member 'np-utterance (fe c)))

        (let* ((np   (daughter 'np c))
               (nbar (and np (daughter 'nbar np)))
               (noun (and nbar (daughter 'noun nbar))))
          (truthy "the utterance NP is a TIME prop-np"
                  (and np (subsetp '(prop-np time) (fe np))))

          ;; The outer complex noun carries the clock-time interpretation.
          (truthy "the head is an HOUR complex-noun"
                  (and noun (subsetp '(hour time complex-noun) (fe noun))))
          (check "its TIME register is `hour'" (and noun (getr 'time noun)) 'hour)
          (check "its HOURS register is 2" (and noun (getr 'hours noun)) 2)
          (check "its MINUTES register is 0" (and noun (getr 'minutes noun)) 0)
          (check "its markers register is (time)"
                 (and noun (getr 'markers noun)) '(time))
          (truthy "a TIME-OF-DAY register was set" (and noun (getr 'time-of-day noun)))

          ;; Inside: the hour number and the o'clock postmod.
          (let* ((inner-np (and noun (daughter 'np noun)))
                 (hour     (and inner-np (daughter 'hour inner-np)))
                 (oclock   (and inner-np (daughter 'oclock inner-np))))
            (truthy "the complex noun dominates a complex-noun-np"
                    (and inner-np (member 'complex-noun-np (fe inner-np))))
            (check "whose hour daughter is the number word `2'"
                   (and hour (getr 'word hour)) '|2|)
            (truthy "and an o'clock postmod daughter" oclock))

          (truthy "final punctuation attached" (daughter 'finalpunc c)))))

    (format t "~&gram4-clock-time-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram4-clock-time-test t)
