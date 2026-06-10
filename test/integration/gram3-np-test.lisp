;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram3-np-test.lisp
;;;
;;; Full NP construction against Marcus's real gram1/gram3 rules: parse
;;; "the dog ." (determiner + noun + final punctuation) end to end. This
;;; exercises the whole wait-and-see apparatus together for the first
;;; time:
;;;
;;;   AS rule          STARTNP        -- attention-shift to start the NP
;;;   creation crule   NP-START       -- associate a case frame with it
;;;   normal rules     DETERMINER, DET-QUANT-DONE, ADJ, NOUN, NBAR
;;;                                   -- with their packet transitions
;;;   NR rules         NBAR-COMPLETE, NP-COMPLETE
;;;                                   -- node reactivation, fired by the
;;;                                      utterance rule looking ahead
;;;   attachment crule NP-NBAR        -- case-frame wiring on attach
;;;   then             NP-UTTERANCE   -- assemble the utterance
;;;
;;; Result tree:  [S [NP [det the] [nbar [noun dog]]] [finalpunc .]],
;;; with number agreement giving the NP the `ns' (singular) feature.
;;;
;;; Rules are Marcus's verbatim (modulo whitespace); LINKed into
;;; :parsifal so data symbols are EQ to the runtime's.
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram3-np-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (unless (find-package :parsifal)
    (asdf:load-system :parsifal))
  (unless (find-package :glang-cl)
    (let* ((here  (or *load-truename* *compile-file-truename*))
           (root  (make-pathname
                   :defaults here
                   :directory (butlast (pathname-directory here) 2)))
           (glang (merge-pathnames "system/reference/glang-cl/" root)))
      (dolist (f '("package" "tokens" "pratt" "fixes"
                   "denotations" "compiler"))
        (load (merge-pathnames (format nil "~a.lisp" f) glang))))))

(in-package :parsifal)


(defparameter *gram3-np-rules*
  '(;; gram1 entry
    "{RULE INITIAL-RULE IN NOWHERE
      [t] -->
      Create a new s node.
      !(setq s {the current s}).
      Activate cpool,ss-start.}"
    ;; gram3 NP construction
    "{AS RULE STARTNP IN CPOOL
      [=ngstart] -->
      Create a new np node.
      If 1st is det then activate parse-det else activate parse-qp-1.
      Activate npool.}"
    "{CREATION CRULE NP-START NP
      if c is none of propn-np, pron-np, name, complex-noun-np, trace, *, comp-np
        and 1st is not pronoun
        then associate a new case frame with c.}"
    "{RULE DETERMINER IN PARSE-DET
      [=det] -->
      Attach 1st to c as det.
      Label c det.
      Transfer the features indef, def, wh from 1st to c.
      Deactivate parse-det. Activate parse-qp-2.}"
    "{RULE DET-QUANT-DONE PRIORITY: 15 IN PARSE-QP-2
      [t] -->
      Deactivate parse-qp-2. Activate parse-adj.}"
    "{RULE ADJ IN PARSE-ADJ
      [t] -->
      If 1st is adj then attach 1st to c as adj
        else if 1st is punc then attach 1st to c as punc
          else deactivate parse-adj; activate parse-noun.}"
    "{RULE NOUN IN PARSE-NOUN
      [=noun] -->
      Attach 1st to a new nbar node as noun.
      Transfer the features massn, time, ns, npl, n1p, n2p, n3p from 1st to c.
      Drop c.}"
    "{RULE NBAR IN PARSE-NOUN
      [=nbar] -->
      Attach 1st to c as nbar.
      Transfer the features time, place, n1p, n2p, n3p from 1st to c.
      Deactivate npool, parse-noun.
      Drop c into the buffer.
      Restore the buffer.}"
    "{ATTACHMENT CRULE NP-NBAR NP OVER NBAR
      Associate the case frame of the upper node with the lower node.
      !'Something-or-other fills the spec slot of the upper node.
      If there is a case frame of upper and there is a noun of the lower node
        then it fills the pred slot of upper.}"
    "{NR RULE NBAR-COMPLETE IN CPOOL
      [=nbar] -->
      Activate cpool, nbar-complete.}"
    "{RULE NBAR-DONE PRIORITY: 15 IN NBAR-COMPLETE
      t -->
      Drop c. Restore the buffer.}"
    "{NR RULE NP-COMPLETE IN CPOOL
      [=np; * is none of modified, not-modifiable] -->
      If there is not a det of c and there is not a qp of c then
        if the nbar of c is npl then label c npl else
        if the noun of the nbar of c is propnoun then label c prop-np else
        if the nbar of c is massn, ns then label c massnp else
        if the nbar of c is time and the ord of the qp of c is general-ord
          then label c next-time-np else
        if the noun of the nbar of c is none of pseudopropnoun then label c bad.
      If there is a det of c or there is a qp of c then
        transfer the meet of (if there is a det of c then the features of it else ns,npl)
          and (if there is a qp of c then the features of it else ns,npl)
          from the nbar of c to c;
        if c is none of ns,npl then label c bad.
      Activate cpool, np-complete.}"
    "{RULE NP-DONE PRIORITY: 15 IN NP-COMPLETE
      t -->
      If there is a case frame of c then finalize the cf of c.
      Drop c. Restore the buffer.}"
    ;; gram1 utterance
    "{RULE NP-UTTERANCE IN SS-START
      [=np] [=finalpunc] -->
      Label c np-utterance.
      Attach 1st to c as np.
      Attach 2nd to c as finalpunc.
      The parse is finished.}"))


(defun gram3-np-test (&optional verbose)
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

      (reset-rule-table)
      (reset-lexicon)

      ;; --- Lexicon (in :parsifal so features are EQ to the linked rules).
      (dolist (w (list 'the 'dog (intern "." :parsifal)))
        (setf (symbol-plist w) nil))
      (df the feats (det ngstart def ns npl))
      (df dog feats (noun ns n3p))
      (%df (list (intern "." :parsifal) 'feats '(finalpunc punc)))
      (dolist (w (list 'the 'dog (intern "." :parsifal))) (expandsim w))

      ;; --- Grammar (compile -> link -> register).
      (dolist (src *gram3-np-rules*)
        (eval (glang-cl::link (glang-cl::compile-rule src))))

      ;; --- Parse "the dog ." end to end.
      (let ((ok (parse-sentence "the dog ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"the dog .\"" ok t)

        ;; The full wait-and-see derivation: AS (STARTNP), the det/qp/adj/
        ;; noun chain, both NR completions, and the utterance rule.
        (check "12-rule derivation, AS + both NR rules firing"
               (reverse (mapcar #'symbol-name *deriv*))
               '("INITIAL-RULE" "STARTNP" "DETERMINER" "DET-QUANT-DONE"
                 "ADJ" "NOUN" "NBAR" "NP-COMPLETE" "NBAR-COMPLETE"
                 "NBAR-DONE" "NP-DONE" "NP-UTTERANCE"))

        ;; The tree: [S [NP [det the] [nbar [noun dog]]] [finalpunc .]].
        (truthy "S labelled np-utterance" (member 'np-utterance (fe c)))
        (let ((np (daughter 'np c)))
          (truthy "an NP was attached to S" np)
          (truthy "the NP carries det / np features"
                  (and np (subsetp '(np det) (fe np))))
          (truthy "number agreement made the NP singular (ns)"
                  (and np (member 'ns (fe np))))
          (truthy "the determiner is under the NP"
                  (and np (daughter 'det np)))
          (let ((nbar (and np (daughter 'nbar np))))
            (truthy "an NBAR is under the NP" nbar)
            (truthy "the noun is under the NBAR"
                    (and nbar (daughter 'noun nbar)))))
        (truthy "final punctuation attached to S" (daughter 'finalpunc c))))

    (format t "~&gram3-np-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram3-np-test t)
