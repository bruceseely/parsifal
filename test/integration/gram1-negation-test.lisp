;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-negation-test.lisp
;;;
;;; Clausal negation -- OUR extension, *negation-rules* (not a Marcus port):
;;;
;;;     {RULE NEGATION PRIORITY: 5 IN BUILD-AUX
;;;      [=auxverb] [=*not] --> Attach 2nd to c as neg. Label c neg.}
;;;
;;; `not' follows the first auxiliary, where it breaks the adjacency every
;;; BUILD-AUX stacking rule needs. NEGATION splices it onto the aux as `neg'
;;; and labels the aux `neg', and the auxiliary then stacks as usual.
;;;
;;; A contraction is the same two words: TOKENIZE splits `doesn't' into `does'
;;; `not'. Before that, MORPHO stripped the `n't' and the negation vanished --
;;; `the girl doesn't eat pie' parsed, silently, as `the girl does eat pie'.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --eval '(ql:quickload :parsifal)' \
;;;        --load test/integration/gram1-negation-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-negation-test (&optional verbose)
  (let ((results t))
    (labels ((check (test-name actual expected)
               (let ((pass (equal actual expected)))
                 (when (or verbose (not pass))
                   (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                   (unless pass
                     (format t "    expected: ~s~%    actual:   ~s~%" expected actual)))
                 (setf results (and pass results))))
             (parse (sentence)
               (handler-bind ((warning #'muffle-warning))
                 (parse-sentence sentence :initial-rule (intern "INITIAL-RULE" :parsifal))))
             (aux-of (s) (daughter 'aux s))
             (neg-p (aux) (and aux (member 'neg (fe aux)) t))
             (verb-of (s) (let ((vp (daughter 'vp s)))
                            (and vp (getr 'word (daughter 'verb vp)))))
             (embedded-s (s) (let* ((vp (daughter 'vp s))
                                    (np (and vp (daughter 'np vp))))
                               (and np (daughter 's np)))))

      ;; The tokenizer: a negative contraction is two words.
      (check "doesn't -> does not" (tokenize "doesn't") '("does" "not"))
      (check "isn't -> is not" (tokenize "isn't") '("is" "not"))
      (check "won't -> will not" (tokenize "won't") '("will" "not"))
      (check "can't -> can not" (tokenize "can't") '("can" "not"))
      (check "other words are untouched" (tokenize "the girl's pie .")
             '("the" "girl's" "pie" "."))
      (check "not is a known word" (unknown-words "the girl does not eat pie .") nil)

      (load-full-grammar)

      ;; Each auxiliary stacks as it would without `not', and the aux is neg.
      (dolist (case '(("the girl does not eat pie ."           do    eat)
                      ("the girl doesn't eat pie ."            do    eat)
                      ("the girl did not eat pie ."            do    eat)
                      ("the girl is not eating pie ."          prog  eating)
                      ("the girl isn't eating pie ."           prog  eating)
                      ("the girl will not eat pie ."           will  eat)
                      ("the girl won't eat pie ."              will  eat)
                      ("the girl has not eaten pie ."          perf  eaten)
                      ("the boy should not have scheduled the meeting ." modal scheduled)
                      ("the pie was not eaten ."               passive eaten)))
        (destructuring-bind (sentence slot verb) case
          (check (format nil "parses: ~a" sentence) (and (parse sentence) t) t)
          (let ((aux (aux-of c)))
            (check (format nil "  its aux is neg") (neg-p aux) t)
            (check (format nil "  `not' is the aux's neg daughter")
                   (and aux (getr 'word (daughter 'neg aux))) 'not)
            (check (format nil "  the aux still has its ~(~a~)" slot)
                   (and aux (daughter slot aux) t) t)
            (check (format nil "  the main verb is ~(~a~)" verb) (verb-of c) verb))))

      ;; Without `not' nothing is neg.
      (check "parses: the girl eats pie ." (and (parse "the girl eats pie .") t) t)
      (check "  its aux is not neg" (neg-p (aux-of c)) nil)
      (check "parses: the girl does eat pie ." (and (parse "the girl does eat pie .") t) t)
      (check "  its aux is not neg" (neg-p (aux-of c)) nil)

      ;; Inside a that-complement the embedded clause is the denied one.
      (check "parses: the girl believes the boy does not eat pie ."
             (and (parse "the girl believes the boy does not eat pie .") t) t)
      (check "  the matrix aux is not neg" (neg-p (aux-of c)) nil)
      (check "  the embedded aux is neg" (neg-p (aux-of (embedded-s c))) t)

      ;; The regression guard: without the rule group, `does not' dies.
      (reset-rule-table)
      (apply #'register-grammar (remove *negation-rules* *full-grammar*))
      (check "without *negation-rules* `does not eat' fails to parse"
             (parse "the girl does not eat pie .") nil))

    (format t "~&gram1-negation-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-negation-test t)
