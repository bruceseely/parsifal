;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-imperative-pp-test.lisp
;;;
;;; A Marcus example sentence parsed end to end, dictionary-driven:
;;; "Schedule a meeting for friday." (notes/pidgin-grammar-intro.text).
;;; This adds three things over the simple declarative clause, all from
;;; the loaded defs.l dictionary (no hand-built df):
;;;
;;;   IMPERATIVE   ss-start [=tnsless] -> label the clause imper/major and
;;;                insert the implicit subject "you"
;;;   PRONOUN      npool -> "you" becomes a pron-np subject
;;;   PP           cpool [=prep][=np] -> build "for friday"; PP-UNDER-VP-1
;;;                attaches it (it fits schedule's `for'->time slot)
;;;
;;; ("friday" is parsed as a plain noun via the no-determiner NP path,
;;; QUANT-DONE; the day-of-week complex-NP machinery is not needed here.)
;;;
;;; Result tree:
;;;   [S imper/major [NP you] [VP [verb schedule] [NP a meeting]
;;;                               [PP [prep for] [NP friday]]] [finalpunc .]]
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-imperative-pp-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-imperative-pp-test (&optional verbose)
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
           (word-of (np)            ; the head noun's word under an NP
             (and np (getr 'word (daughter 'noun (daughter 'nbar np))))))

      ;; Dictionary-driven: no hand-built lexicon.
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*
                        *pronoun-rule* *imperative-rule* *qp1-done-rule*
                        *pp-rules*)

      (let ((ok (parse-sentence "schedule a meeting for friday ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"schedule a meeting for friday .\"" ok t)
        (truthy "S is an imperative major clause"
                (subsetp '(imper major s) (fe c)))

        ;; Subject: the inserted "you", as a pron-np.
        (let ((subj (daughter 'np c)))
          (truthy "an implicit subject NP was attached" subj)
          (truthy "the subject is a pron-np with 2nd-person features"
                  (and subj (subsetp '(pron-np n2p) (fe subj)))))

        (let ((vp (daughter 'vp c)))
          (truthy "a VP attached to S" vp)
          (check "the verb is the dictionary verb schedule"
                 (and vp (getr 'word (daughter 'verb vp))) 'schedule)
          (check "the object NP is \"a meeting\""
                 (and vp (word-of (daughter 'np vp))) 'meeting)
          ;; the PP "for friday" attached to the VP, fitting schedule's
          ;; for->time slot.
          (let ((pp (and vp (daughter 'pp vp))))
            (truthy "a PP attached to the VP" pp)
            (check "the PP's preposition is \"for\""
                   (and pp (getr 'word (daughter 'prep pp))) 'for)
            (check "the PP's object is \"friday\""
                   (and pp (word-of (daughter 'np pp))) 'friday)))
        (truthy "final punctuation attached to S" (daughter 'finalpunc c))))

    (format t "~&gram1-imperative-pp-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-imperative-pp-test t)
