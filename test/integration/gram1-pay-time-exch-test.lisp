;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-pay-time-exch-test.lisp
;;;
;;; The "pay" flagship, parsed under the WHOLE grammar, verified down to its
;;; case frame:
;;;
;;;     "I will gladly pay you Tuesday for a hamburger today."
;;;
;;; This one sentence exercises all three of our recent *grammar-extensions*
;;; at once, and -- crucially -- checks the SEMANTICS, not just that it parses:
;;;
;;;   PREVERBAL-ADVERB (*adverb-rules*) -- `gladly' between the modal and the
;;;     verb attaches as an `adv' daughter of the S.
;;;   VP-TIME-NP-TO-PP (*bare-time-rules*) -- the bare time NPs `Tuesday' and
;;;     `today' each become a `during'-PP under the VP, so VP-PP FILLS the verb's
;;;     TIME case (instead of the time NP riding along as a BAD object).
;;;   the `for a hamburger' PP fills EXCH (`pay' preps for->exch); this only
;;;     survives because `Tuesday' no longer corrupts the frame as a BAD object.
;;;
;;; Expected filled case frame (each slot is (CASE node GFUNC)):
;;;   AGT via SUBJ (i), DAT via OBJ (you), EXCH via FOR (hamburger),
;;;   TIME via DURING (Tuesday) AND TIME via DURING (today).
;;;
;;; `pay', `gladly', `hamburger' come from supplement.dict; everything else
;;; from Marcus's defs.l. Pure whole-grammar composition.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-pay-time-exch-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-pay-time-exch-test (&optional verbose)
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
           ;; A filled case's node is a node-ref (NPk . v); its head is the
           ;; word under nbar -> noun.
           (head-word (node)
             (ignore-errors (getr 'word (daughter 'noun (daughter 'nbar node))))))

      ;; Whole-grammar load path -- the same image parses every construction.
      (load-full-grammar)

      (let ((ok (parse-sentence
                 "I will gladly pay you Tuesday for a hamburger today ."
                 :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on the pay flagship" (and ok t) t)
        (truthy "matrix S is a major declarative" (subsetp '(decl major s) (fe c)))

        ;; --- PREVERBAL-ADVERB: `gladly' is an adv daughter of the S ---------
        (let ((adv (daughter 'adv c)))
          (truthy "S has an adv daughter (pre-verbal `gladly')" adv)
          (check  "the adverb is `gladly'" (and adv (getr 'word adv)) 'gladly))

        ;; --- case frame content ---------------------------------------------
        (let* ((vp     (daughter 'vp c))
               (cf     (and vp (getr 'caseframe vp)))
               (filled (and cf (cadr (first (get cf 'hypo-slots))))))
          (truthy "VP has a case frame" cf)
          (check  "the predicate is `pay'" (and cf (get cf 'pred)) 'pay)
          (truthy "the frame has filled cases" filled)

          ;; AGENT <- subject, DATIVE <- object
          (let ((agt (assoc 'agt filled))
                (dat (assoc 'dat filled)))
            (truthy "an AGENT case was filled" agt)
            (check  "AGENT filled via the subj function" (and agt (caddr agt)) 'subj)
            (truthy "a DATIVE case was filled" dat)
            (check  "DATIVE filled via the obj function" (and dat (caddr dat)) 'obj))

          ;; EXCHANGE <- `for a hamburger' (survives the intervening time NP)
          (let ((exch (assoc 'exch filled)))
            (truthy "an EXCHANGE case was filled (for a hamburger)" exch)
            (check  "EXCH filled via `for'" (and exch (caddr exch)) 'for)
            (check  "EXCH filler is `hamburger'"
                    (and exch (head-word (cadr exch))) 'hamburger))

          ;; TIME <- BOTH bare temporals, each via a `during'-PP
          (let* ((times   (remove-if-not (lambda (s) (eq (car s) 'time)) filled))
                 (gfuncs  (mapcar #'caddr times))
                 (fillers (mapcar (lambda (s) (head-word (cadr s))) times)))
            (check  "TWO TIME cases were filled" (length times) 2)
            (truthy "every TIME case was filled via `during'"
                    (every (lambda (g) (eq g 'during)) gfuncs))
            (truthy "the TIME fillers are Tuesday and today"
                    (and (member 'tuesday fillers) (member 'today fillers)))))))

    (format t "~&gram1-pay-time-exch-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-pay-time-exch-test t)
