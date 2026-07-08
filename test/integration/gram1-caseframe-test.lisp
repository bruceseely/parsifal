;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-caseframe-test.lisp
;;;
;;; Case-frame *content* verification: parse a transitive clause and
;;; check that the case.l slot-filling assigned the right thematic roles
;;; -- the subject to the verb's AGENT case and the object to its
;;; NEUTRAL case -- with the semantic-marker checks (smqval) that gate
;;; filling. Dictionary-driven ("the boy meets the boy ."): the verb
;;; `meet' (cf ((neut)(com)agt)) and the case markersets (agt: hanim/
;;; anim; neut: all) all come from the loaded defs.l.
;;;
;;; The VP's frame ends a parse as the open frame; closing it flushes its
;;; HYPO-SLOTS to the plist. Each filled case is (CASE node GFUNC).
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-caseframe-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)


(defun gram1-caseframe-test (&optional verbose)
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

      ;; Dictionary-driven: no hand-built lexicon.
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*)

      (let ((ok (parse-sentence "the boy meets the boy ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on \"the boy meets the boy .\"" ok t)

        (let* ((vp   (daughter 'vp c))
               (cf   (and vp (getr 'caseframe vp)))
               (subj (daughter 'np c))     ; subject NP sits under S
               (obj  (and vp (daughter 'np vp))))  ; object NP under the VP
          (truthy "VP has a case frame" cf)
          (truthy "subject and object NPs present" (and subj obj))
          (check "the predicate is the dictionary verb meet" (and cf (get cf 'pred)) 'meet)

          ;; Close the (open) VP frame to flush its filled cases, then read
          ;; the surviving hypothesis's filled-case list.
          (closeframe openframe)
          (let ((filled (cadr (first (get cf 'hypo-slots)))))
            (truthy "the frame has filled cases" filled)
            ;; subject -> AGENT (the boy is hanim, fits meet's agt)
            (let ((agt (assoc 'agt filled)))
              (truthy "an agent case was filled" agt)
              (check "the agent case is the subject NP" (cadr agt) subj)
              (check "the agent was filled via the subj grammatical function"
                     (caddr agt) 'subj))
            ;; object -> NEUTRAL (meet's neut markerset is `all')
            (let ((neut (assoc 'neut filled)))
              (truthy "a neutral case was filled" neut)
              (check "the neutral case is the object NP" (cadr neut) obj)
              (check "the neutral case was filled via the obj grammatical function"
                     (caddr neut) 'obj)))))

      ;; The marker check really gates filling: "the lecture" (socev /
      ;; mentobj) does NOT fit meet's agent case (hanim / anim), so the
      ;; agent slot stays unfilled -- a semantically odd subject is
      ;; rejected, not forced.
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule*)
      (parse-sentence "the lecture meets ."
                      :initial-rule (intern "INITIAL-RULE" :parsifal))
      (let* ((vp (daughter 'vp c)) (cf (and vp (getr 'caseframe vp))))
        (closeframe openframe)
        (check "a non-animate subject does NOT fill the agent case"
               (assoc 'agt (cadr (first (get cf 'hypo-slots))))
               nil))

    (format t "~&gram1-caseframe-test: ~:[FAILED <<<~;passed~]~%" results)
    results)))


(gram1-caseframe-test t)
