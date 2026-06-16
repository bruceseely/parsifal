;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-coverage-test.lisp
;;;
;;; BROAD COVERAGE SWEEP -- a wide regression net under the deep
;;; per-construction tests. It loads the whole grammar ONCE
;;; (`load-full-grammar') and runs a large corpus of real sentences --
;;; Bruce's pass through Marcus's 1977-grammar-intro examples -- against
;;; that single image, checking only the coarse outcome (parses / does not
;;; parse). This complements, and does not replace, the dedicated tests
;;; that assert tree structure and case roles; here we just guard that the
;;; whole family set keeps PARSING and keeps COMPOSING (every construction
;;; coexisting in one grammar -- the property that broke when TOO-MANY-NPS
;;; was reachable).
;;;
;;; Two corpora, each with an EXPECTED outcome:
;;;
;;;   *COVERAGE-PASSING*  -- must parse (=> non-NIL). If a future change
;;;     regresses one, this catches it.
;;;
;;;   *COVERAGE-DEFERRED* -- known NOT to parse today (=> NIL), the
;;;     deliberately-deferred constructions (see the deferred backlog:
;;;     bare temporal adjuncts on slot-full verbs, pied-piping, participial
;;;     reduced relatives, the existential/reduced-relative ordering
;;;     conflict). Asserting NIL turns the backlog into executable
;;;     documentation: if a fix makes one of these START parsing, THIS TEST
;;;     FAILS ON PURPOSE -- the nudge to come back and give it a real
;;;     structural test and move it up to the passing list.
;;;
;;; CAVEAT -- "parses" here means "returns non-NIL," NOT "is semantically
;;; correct." In particular several passing sentences contain a bare
;;; temporal adjunct (`yesterday'/`tomorrow') that rides along as a BAD
;;; node filling no TIME role; they complete, but the adjunct is dropped.
;;; They are marked [TIME-DROPPED] below. (Their sibling "What did you give
;;; Sue yesterday?" instead OVERFLOWS a full verb and is in the deferred
;;; list.) Deep correctness lives in the per-construction tests.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-coverage-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defparameter *coverage-passing*
  '("The boy persuaded the girl to go."
    "I will schedule a meeting."
    "John should have scheduled the meeting."
    "Is a meeting scheduled for Wednesday."
    "I saw the man with the red hair."
    "The jar seems broken."
    "I wanted John to do it."
    "I want to do it."
    "I persuaded John to do it."
    "There seems to have been a meeting scheduled for friday."
    "Schedule a meeting for friday."
    "Does there seem to be a meeting scheduled for friday?"
    "A meeting seems to have been scheduled for Friday."
    "I told Sue you would schedule the meeting."
    "The boy who wanted to meet you scheduled the meeting."
    "The boy who met you scheduled the meeting."
    "The boy who you met scheduled the meeting."
    "The boy you met scheduled the meeting."
    "Who did John see?"
    "Who broke the jar?"
    "What did Bob give to Sue?"
    "Who did Bob give the book?"
    "Who did Bob give the book to?"
    "What did Bob give Sue?"
    "I promised John to do it."
    "Who did you say that Bill told?"
    "You promised to give the book to John."
    "Who did you promise to give the book to?"
    "Who did you promise to schedule the meeting?"
    "Who did you say scheduled the meeting?"
    "Who did you persuade to do it?"
    "Who did you give the book yesterday?"             ; [TIME-DROPPED]
    "Who did you ask to schedule the meeting?"
    "Who do you want to give a book to tomorrow?"      ; [TIME-DROPPED]
    "Who did you want to give a book to Sue?"
    "Who did you promise to give the book to Sue tomorrow?" ; [TIME-DROPPED]
    "I saw the man with the telescope."
    "I told that boy that boys should do it."
    "There seems to be a jar broken."
    "I told the boy that I saw Sue."
    "I told the girl that you would schedule the meeting."
    "I gave the boy who you wanted to give the books to three books."
    "Who did you promise to give the book to tomorrow?") ; [TIME-DROPPED]
  "Corpus that must PARSE under the whole grammar (coarse outcome only).")

(defparameter *coverage-deferred*
  '("What did you give Sue yesterday?"        ; bare time adjunct overflows give
    "To whom did you promise to give the book?" ; pied-piping (Marcus strands)
    "the meeting scheduled for friday meets."  ; participial reduced relative (Marcus gap)
    "Is there a meeting scheduled for friday?") ; existential vs reduced-relative ordering
  "Corpus known NOT to parse today (=> NIL). Asserting NIL documents the
   deferred boundary; a sentence that starts parsing flips this test RED on
   purpose -- promote it to *COVERAGE-PASSING* (with a real structural
   test) when that happens. See the deferred backlog.")


(defun gram1-coverage-test (&optional verbose)
  (let ((results t)
        (*warn-unknown-words* nil))   ; corpus is in-vocabulary; keep output clean
    (flet ((expect (sentence want)
             (let* ((got (and (ignore-errors
                               (parse-sentence sentence
                                               :initial-rule (intern "INITIAL-RULE" :parsifal)))
                              t))
                    (pass (eq got want)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  [~:[NIL~;T~]] ~a~%" pass got sentence)
                 (unless pass
                   (format t "        expected ~:[NIL~;T~], got ~:[NIL~;T~]~%" want got)))
               (setf results (and pass results)))))

      (load-full-grammar)

      (when verbose (format t "~&-- must parse (~d) --~%" (length *coverage-passing*)))
      (dolist (s *coverage-passing*) (expect s t))

      (when verbose (format t "~&-- deferred, must NOT parse (~d) --~%"
                            (length *coverage-deferred*)))
      (dolist (s *coverage-deferred*) (expect s nil)))

    (format t "~&gram1-coverage-test (~d sentences): ~:[FAILED <<<~;passed~]~%"
            (+ (length *coverage-passing*) (length *coverage-deferred*))
            results)
    results))


(gram1-coverage-test t)
