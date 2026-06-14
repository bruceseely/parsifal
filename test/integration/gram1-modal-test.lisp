;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-modal-test.lisp
;;;
;;; Declarative MODAL and FUTURE auxiliaries, parsed end to end and fully
;;; dict-driven:
;;;
;;;     "I will schedule a meeting ."            (future:  will  + schedule)
;;;     "John should schedule the meeting ."     (modal:   should + schedule)
;;;     "John should have scheduled the meeting ." (modal + perfect stack)
;;;
;;; These are book examples that previously returned NIL. The leading
;;; verb-group word (`will'/`should') is tensed, so it satisfies [=verb]
;;; (redund: pres/past/future/tnsless -> verb); STARTAUX therefore makes an
;;; aux node and copies the tense onto it, but -- being non-destructive --
;;; leaves the word in the buffer. With no rule to consume it, MAIN-VERB then
;;; wrongly grabbed the modal/future word as the main verb (`will' became the
;;; verb of "I will schedule ...", stranding "schedule a meeting ."), so the
;;; parse failed.
;;;
;;; *modal-rules* (gram1:121-125) fills that gap in BUILD-AUX:
;;;   - MODAL  [=modal][=tnsless] consumes a modal (`should'/`would'/`could'/
;;;     `must'/`can'): attaches it under the aux as `modal', labels the aux modal;
;;;   - FUTURE [=*will][=tnsless] does the same for `will' (aux daughter `will',
;;;     aux labelled future).
;;; Both fire at the default priority 10 -- ahead of AUX-COMPLETE (15), which
;;; then drops the completed aux so MAIN-VERB sees the genuine main verb. They
;;; are disjoint from PERFECTIVE/PASSIVE-AUX/DO-SUPPORT (each keyed on its own
;;; lexical anchor), so they STACK: "should have scheduled" is MODAL then
;;; PERFECTIVE, giving a perf+modal aux over the participle `scheduled'.
;;;
;;; Everything else composes: the clause + NP layers, *vp-np-rule*,
;;; *perfect-rules* (for the perfect stack), *pronoun-rule* (for `I'),
;;; *proper-noun-rules* (for `John'), *qp1-done-rule* (for `a meeting').
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-modal-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-modal-test (&optional verbose)
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

      ;; Fully dict-driven: will/should/have/schedule/scheduled/meeting/john all
      ;; resolve through morpho + defs.l (`will' -> *will/future, `should' ->
      ;; modal via the jlike chain should->would->could->can, `scheduled' ->
      ;; *schedule/en). Curate the aux + clause + NP layers plus *modal-rules*.
      (reset-rule-table)
      (register-grammar *np-rules* *pronoun-rule* *proper-noun-rules*
                        *clause-rules* *vp-np-rule* *qp1-done-rule*
                        *perfect-rules* *modal-rules*)

      ;; --- FUTURE: "I will schedule a meeting ." ---------------------------
      (let ((ok (parse-sentence "i will schedule a meeting ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "future: parse succeeds" ok t)
        (truthy "future: top S is a major declarative" (subsetp '(decl major s) (fe c)))
        (let* ((aux (daughter 'aux c))
               (vp  (daughter 'vp c)))
          (truthy "future: an aux attached to S" aux)
          (truthy "future: the aux is labelled future" (and aux (member 'future (fe aux))))
          (check "future: the aux's `will' daughter is `will'"
                 (and aux (getr 'word (daughter 'will aux))) 'will)
          (truthy "future: a VP attached to S" vp)
          (check "future: the MAIN VERB is `schedule', not `will'"
                 (and vp (getr 'word (daughter 'verb vp))) 'schedule)
          (let ((cf (and vp (getr 'caseframe vp))))
            (check "future: the predicate is SCHEDULE (the modal is not the verb)"
                   (and cf (get cf 'pred)) 'schedule))))

      ;; --- MODAL: "John should schedule the meeting ." --------------------
      (let ((ok (parse-sentence "john should schedule the meeting ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "modal: parse succeeds" ok t)
        (truthy "modal: top S is a major declarative" (subsetp '(decl major s) (fe c)))
        (let* ((aux (daughter 'aux c))
               (vp  (daughter 'vp c)))
          (truthy "modal: an aux attached to S" aux)
          (truthy "modal: the aux is labelled modal" (and aux (member 'modal (fe aux))))
          (check "modal: the aux's `modal' daughter is `should'"
                 (and aux (getr 'word (daughter 'modal aux))) 'should)
          (check "modal: the MAIN VERB is `schedule', not `should'"
                 (and vp (getr 'word (daughter 'verb vp))) 'schedule)
          (let ((cf (and vp (getr 'caseframe vp))))
            (check "modal: the predicate is SCHEDULE" (and cf (get cf 'pred)) 'schedule))))

      ;; --- MODAL + PERFECT STACK: "John should have scheduled the meeting ."
      (let ((ok (parse-sentence "john should have scheduled the meeting ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "modal+perfect: parse succeeds" ok t)
        (truthy "modal+perfect: top S is a major declarative" (subsetp '(decl major s) (fe c)))
        (let* ((aux (daughter 'aux c))
               (vp  (daughter 'vp c)))
          (truthy "modal+perfect: an aux attached to S" aux)
          (truthy "modal+perfect: the aux is labelled modal" (and aux (member 'modal (fe aux))))
          (truthy "modal+perfect: the aux is also labelled perf (the stack)"
                  (and aux (member 'perf (fe aux))))
          (check "modal+perfect: the aux's `modal' daughter is `should'"
                 (and aux (getr 'word (daughter 'modal aux))) 'should)
          (check "modal+perfect: the aux's `perf' daughter is `have'"
                 (and aux (getr 'word (daughter 'perf aux))) 'have)
          (check "modal+perfect: the MAIN VERB is the participle `scheduled'"
                 (and vp (getr 'word (daughter 'verb vp))) 'scheduled)
          (let ((cf (and vp (getr 'caseframe vp))))
            (check "modal+perfect: the predicate is SCHEDULE"
                   (and cf (get cf 'pred)) 'schedule)))))

    (format t "~&gram1-modal-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-modal-test t)
