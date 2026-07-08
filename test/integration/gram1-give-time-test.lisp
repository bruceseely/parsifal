;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-give-time-test.lisp
;;;
;;; Give-class TIME case: a bare temporal adjunct on a give-class transfer
;;; verb now FILLS a TIME case (was dropped as a BAD object). Marcus entered
;;; give/tell/deliver with no TIME slot; AUGMENT-GIVE-CLASS-TIME (dictionary.lisp)
;;; splices an optional, refillable TIME case into their frames after load, and
;;; VP-TIME-NP-TO-PP (*bare-time-rules*) turns the bare temporal into a
;;; `during'-PP that VP-PP fills into that slot.
;;;
;;;     "the boy gave the girl a book yesterday ."
;;;
;;; Expected filled frame: AGT via SUBJ (boy), DAT via OBJ (girl),
;;; NEUT via OBJ (book), TIME via DURING (yesterday).
;;;
;;; Also checks that a jlike descendant (`buy' jlike `give') inherits the
;;; augmented frame -- the TIME case rides through *specregs* at expansion.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-give-time-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-give-time-test (&optional verbose)
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
           (head-word (node)
             (ignore-errors (getr 'word (daughter 'noun (daughter 'nbar node))))))

      ;; --- lexical augmentation: give gained a TIME case, descendants inherit
      (expandsim 'give)
      (truthy "give's case-frame has a TIME case"
              (assoc 'time (get 'give 'case-frame)))
      (expandsim 'buy)                  ; buy is `jlike give'
      (truthy "jlike descendant `buy' inherits the TIME case"
              (assoc 'time (get 'buy 'case-frame)))

      ;; --- and it actually FILLS on a ditransitive give + bare temporal ----
      (load-full-grammar)
      (let ((ok (parse-sentence "the boy gave the girl a book yesterday ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds" (and ok t) t)

        (let* ((vp     (daughter 'vp c))
               (cf     (and vp (getr 'caseframe vp)))
               (filled (and cf (cadr (first (get cf 'hypo-slots))))))
          (check "the predicate is `give'" (and cf (get cf 'pred)) 'give)
          (truthy "an AGENT case was filled" (assoc 'agt filled))
          (truthy "a DATIVE case was filled" (assoc 'dat filled))
          (truthy "a NEUTRAL case was filled" (assoc 'neut filled))

          (let ((time (assoc 'time filled)))
            (truthy "a TIME case was filled (was dropped before)" time)
            (check  "TIME filled via a `during'-PP" (and time (caddr time)) 'during)
            (check  "the TIME filler is `yesterday'"
                    (and time (head-word (cadr time))) 'yesterday)))))

    (format t "~&gram1-give-time-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-give-time-test t)
