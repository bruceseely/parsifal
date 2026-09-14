;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-noun-compound-test.lisp
;;;
;;; NOUN-NOUN COMPOUNDS -- our extension (*noun-compound-rules*; EXTENSIONS.md
;;; section 10). gram1-gram5 and the 1977 appendix have no compound rule: NOUN
;;; (gram3:131) takes a single [=noun] into an nbar and ADJ (gram3:114) handles
;;; attributive ADJECTIVES only. So "a cherry pie" parsed as TWO NPs -- `a
;;; cherry' plus a determiner-less `pie' that NP-DONE labels `bad' -- and a
;;; downstream extractor took the FIRST as the object, losing the head noun.
;;;
;;; NOUN-COMPOUND (IN PARSE-ADJ, priority 5) attaches the first noun as an
;;; `nmod' daughter and lets the second continue as the head, so the compound
;;; is head-final: a cherry pie is a PIE.
;;;
;;; The regression guards matter more than the happy path here. Marcus enters
;;; deictics and calendar words as ordinary nouns (`there' is place, `monday'
;;; is time), so an adjunct sits two nouns side by side -- "a book yesterday",
;;; "the dog here". Both cells therefore exclude `time' and `place'; without
;;; that the rule eats the adjunct and the DATIVE/LOC is lost.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-noun-compound-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-noun-compound-test (&optional verbose)
  (let ((results t))
    (flet ((truthy (test-name actual)
             (let ((pass (and actual t)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name))
               (setf results (and pass results))))
           (check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%" expected actual)))
               (setf results (and pass results)))))

      (load-full-grammar)

      ;; -- the compound: "a cherry pie" is ONE NP, head-final --
      (let ((ok (parse-sentence "the girl eats a cherry pie ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (truthy "parse succeeds on \"the girl eats a cherry pie .\"" ok)
        (let* ((obj  (daughter 'np (daughter 'vp c)))
               (nmod (and obj (daughter 'nmod obj))))
          (truthy "object NP present" obj)
          (check  "HEAD noun is `pie', not `cherry'"
                  (and obj (getr 'word (daughter 'noun (daughter 'nbar obj)))) 'pie)
          (truthy "object has an nmod daughter" nmod)
          (check  "the nmod is `cherry'" (and nmod (getr 'word nmod)) 'cherry)))

      ;; -- no false positive: a single noun gets no nmod --
      (let ((ok (parse-sentence "the girl eats a pie ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (truthy "parse still succeeds without a compound" ok)
        (let ((obj (daughter 'np (daughter 'vp c))))
          (truthy "plain object has NO nmod daughter"
                  (not (and obj (daughter 'nmod obj))))))

      ;; -- REGRESSION GUARD: a bare TIME adjunct is not swallowed --
      ;; "a book yesterday" is [book][yesterday], two adjacent nouns. If the
      ;; rule fires the dative goes unfilled (gram1-give-time-test's symptom).
      (let ((ok (parse-sentence "the boy gave the girl a book yesterday ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (truthy "parse succeeds with a bare temporal adjunct" ok)
        ;; A ditransitive VP already has several np daughters, so DAUGHTERS --
        ;; DAUGHTER errors on the multiplicity. None of them may carry an nmod.
        (let ((nps (daughters 'np (daughter 'vp c))))
          (truthy "no object NP absorbed `yesterday' as an nmod"
                  (notany (lambda (np) (daughter 'nmod np)) nps))))

      ;; -- REGRESSION GUARD: a PLACE deictic is not swallowed --
      ;; `here' stays an NP of its own, so the VP has TWO np daughters -- which
      ;; is the point: absorbed as an nmod there would be only one. Hence
      ;; DAUGHTERS, not DAUGHTER, which errors on the multiplicity.
      (let ((ok (parse-sentence "the boy sees the dog here ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (truthy "parse succeeds with a place deictic" ok)
        (let ((nps (daughters 'np (daughter 'vp c))))
          (truthy "`here' stays a separate NP (two np daughters)"
                  (= (length nps) 2))
          (truthy "no object NP absorbed `here' as an nmod"
                  (notany (lambda (np) (daughter 'nmod np)) nps)))))

    (format t "~&gram1-noun-compound-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-noun-compound-test t)
