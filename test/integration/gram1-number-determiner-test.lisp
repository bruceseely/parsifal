;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-number-determiner-test.lisp
;;;
;;; "I gave the boy three books." parsed end to end, FULLY DICT-DRIVEN -- the
;;; gram4 NUMBER grammar serving as an NP determiner. Where gram4-test exercises
;;; the number builders on standalone numbers ("ninety nine", "two hundred"),
;;; this pins down a number used as a QUANTIFIER inside a noun phrase: `three
;;; books' is a count NP whose qp is a numqp.
;;;
;;;     I gave the boy three books    agt = I, dat = the boy, neut = three books
;;;
;;; `three' is a `num' (an as-type), so inside the object NP the NUMBER AS rule
;;; shifts into BUILD-NUMBER; NUMBER-DONE finalises it (complete-num + quant +
;;; npl) and hands back to npool; then the ordinary quantifier-phrase QUANT rule
;;; attaches it as the NP's NUMQP (carrying the quant register 3) and `books'
;;; becomes the head noun -- a plural (npl) NP.
;;;
;;; This is what unblocks the literal nested-relative flagship sentence ("...to
;;; three books") -- see gram1-nested-relative-flagship-test. New: the
;;; *number-rules* group (gram4 BUILD-NUMBER), composed into *full-grammar*.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-number-determiner-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-number-determiner-test (&optional verbose)
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

      (load-full-grammar)

      ;; Sanity: `three' is a num (the as-type the NUMBER rule keys on) with a
      ;; quant register of 3.
      (expandsim 'three)
      (truthy "dict `three' is a num" (member 'num (get 'three 'features)))
      (check "dict `three' has quant register 3" (get 'three 'quant) 3)

      (let ((ok (parse-sentence "i gave the boy three books ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds" ok t)
        (truthy "no BAD constituent" (notany (lambda (n) (member 'bad (fe n))) *nodelist*))
        (truthy "main S is a major declarative" (subsetp '(decl major s) (fe c)))
        (check "the subject is `I'" (np-word (daughter 'np c)) 'i)

        (let* ((vp    (daughter 'vp c))
               (objs  (and vp (daughters 'np vp)))
               ;; `three books' is the object that carries a qp; `the boy' has none.
               (three (find-if (lambda (n) (daughter 'qp n)) objs))
               (boy   (find-if (lambda (n) (and (not (daughter 'qp n))
                                                (eq (np-word n) 'boy))) objs)))
          (check "the main verb is `give' (gave)"
                 (and vp (getr 'word (daughter 'verb vp))) 'gave)
          (check "the VP has two objects" (length objs) 2)
          (check "the recipient object is `the boy'" (np-word boy) 'boy)

          ;; The count NP `three books'.
          (truthy "the number object NP exists" three)
          (check "its head noun is `books'" (np-word three) 'books)
          (truthy "the number object NP is plural (npl)" (and three (member 'npl (fe three))))
          (let* ((qp    (and three (daughter 'qp three)))
                 (quant (and qp (daughter 'quant qp))))
            (truthy "the NP has a qp" qp)
            (truthy "the qp is a numqp" (and qp (member 'numqp (fe qp))))
            (check "the qp's quantifier word is `three'"
                   (and quant (getr 'word quant)) 'three)
            (check "the qp carries the numeric quant register 3"
                   (and quant (getr 'quant quant)) 3))

          (truthy "final punctuation attached" (daughter 'finalpunc c))

          ;; Case frame: I gave the boy three books.
          (let ((cf (and vp (getr 'caseframe vp))))
            (check "predicate is give" (and cf (get cf 'pred)) 'give)
            (closeframe openframe)
            (let ((filled (cadr (first (get cf 'hypo-slots)))))
              (let ((agt (assoc 'agt filled)) (dat (assoc 'dat filled)) (neut (assoc 'neut filled)))
                (truthy "agent filled" agt)
                (check "the agent is `I'" (cadr agt) (daughter 'np c))
                (truthy "dative (recipient) filled" dat)
                (check "the recipient is `the boy'" (cadr dat) boy)
                (truthy "neutral (theme) filled" neut)
                (check "the theme is `three books'" (cadr neut) three)))))))

    (format t "~&gram1-number-determiner-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-number-determiner-test t)
