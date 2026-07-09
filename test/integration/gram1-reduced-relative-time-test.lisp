;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-reduced-relative-time-test.lisp
;;;
;;; Bare temporal adjunct INSIDE a reduced relative clause.
;;;
;;;     "i pay you for a hamburger you give me today ."
;;;
;;; The reduced relative "you give me [gap] today" modifies `hamburger'. Its
;;; object gap is give's NEUT (the thing given = the hamburger), and `today' is
;;; give's TIME. Marcus's reduced-relative machinery binds the gap correctly on
;;; its own -- BUT our *bare-time-rules* originally fired only IN SS-VP, never in
;;; the embedded relative clause's WH-VP/EMBEDDED-S-VP packets. So `today' inside
;;; the relative was never rewritten to a `during'-PP; WH-WITH-NP-NEXT then grabbed
;;; it as a candidate second object of `give', the relative closed WITHOUT binding
;;; its gap, and the stray gap-trace + `today' leaked up into the matrix verb's
;;; open NEUT/TIME slots. The fix: EMB-WH-VP-TIME-NP-TO-PP (IN WH-VP) and
;;; EMB-VP-TIME-NP-TO-PP (IN EMBEDDED-S-VP), the embedded siblings of
;;; VP-TIME-NP-TO-PP, so a bare temporal is rewritten to a `during'-PP inside the
;;; relative and fills the relative verb's TIME there.
;;;
;;; Expected: the relative GIVE fills AGT (you), DAT (me), NEUT (a trace = the
;;; hamburger gap) and TIME (today, via `during'); and the matrix PAY fills only
;;; AGT/DAT/EXCH -- NO leaked NEUT, NO leaked TIME.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-reduced-relative-time-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-reduced-relative-time-test (&optional verbose)
  (let ((results t))
    (flet ((check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%" expected actual)))
               (setf results (and pass results))))
           (truthy (test-name actual)
             (let ((pass (and actual t)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass (format t "    expected non-NIL, got NIL~%")))
               (setf results (and pass results))))
           (falsy (test-name actual)
             (let ((pass (null actual)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass (format t "    expected NIL, got ~s~%" actual)))
               (setf results (and pass results)))))

      (load-full-grammar)
      (let ((ok (parse-sentence "i pay you for a hamburger you give me today ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds" (and ok t) t)

        (let* ((vp     (daughter 'vp c))
               (pp     (and vp (daughter 'pp vp)))
               (np     (and pp (daughter 'np pp)))       ; the hamburger NP
               (rel    (and np (daughter 's np)))        ; its relative clause
               (relvp  (and rel (daughter 'vp rel)))
               (relcf  (and relvp (getr 'caseframe relvp)))
               (relfilled (and relcf (cadr (first (get relcf 'hypo-slots)))))
               (mcf    (and vp (getr 'caseframe vp)))
               (mfilled (and mcf (cadr (first (get mcf 'hypo-slots))))))

          ;; --- the relative clause exists and is `give' -------------------
          (truthy "hamburger NP carries a relative clause (S daughter)" rel)
          (check  "the relative verb is `give'"
                  (and relvp (getr 'word (daughter 'verb relvp))) 'give)

          ;; --- the relative GIVE is COMPLETE: agt, dat, neut-gap, time -----
          (truthy "relative GIVE fills AGT"  (assoc 'agt relfilled))
          (truthy "relative GIVE fills DAT"  (assoc 'dat relfilled))
          (let ((neut (assoc 'neut relfilled)))
            (truthy "relative GIVE fills NEUT (the object gap)" neut)
            (truthy "the NEUT filler is a trace (the reduced-relative gap)"
                    (and neut (member 'trace (fe (cadr neut))))))
          (let ((time (assoc 'time relfilled)))
            (truthy "relative GIVE fills TIME (was leaking to the matrix)" time)
            (check  "relative TIME filled via a `during'-PP"
                    (and time (caddr time)) 'during))

          ;; --- the matrix PAY is CLEAN: no leaked gap or time -------------
          (truthy "matrix PAY fills AGT"  (assoc 'agt mfilled))
          (truthy "matrix PAY fills DAT"  (assoc 'dat mfilled))
          (truthy "matrix PAY fills EXCH" (assoc 'exch mfilled))
          (falsy  "matrix PAY has NO leaked NEUT" (assoc 'neut mfilled))
          (falsy  "matrix PAY has NO leaked TIME" (assoc 'time mfilled)))))

    (format t "~&gram1-reduced-relative-time-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-reduced-relative-time-test t)
