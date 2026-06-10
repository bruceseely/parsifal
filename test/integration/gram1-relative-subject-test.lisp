;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram1-relative-subject-test.lisp
;;;
;;; A relative-modified NP serving as a full clause subject -- the EXACT
;;; canonical example sentence "The boy who met you scheduled the meeting."
;;; (notes/pidgin-grammar-intro.text), parsed end to end and entirely from the
;;; loaded defs.l dictionary. This is the flagship relative-clause result: a
;;; relative clause that itself has an object, embedded inside the subject NP
;;; of a transitive main clause.
;;;
;;;   [S [NP the boy_i [S-rel who_i [ t_i met you ]]]  scheduled  [NP the meeting] ]
;;;
;;; - The relative clause "who met you" is a subject relative: `who' is bound
;;;   to the head `the boy', the relative subject is a trace bound to `who'
;;;   (chain trace -> who -> the boy), and `you' is its object.
;;; - The whole modified NP "the boy who met you" is then the subject of the
;;;   main verb `scheduled', whose object is "the meeting".
;;; - Case frame of the main clause: AGENT = the boy, NEUTRAL = the meeting;
;;;   i.e. "the boy" is both the scheduler (main agent) and the meeter
;;;   (relative subject, via the trace chain).
;;;
;;; No new rules over gram1-relative-clause-test -- pure composition of the
;;; existing groups (relative + embedded-clause + clause + wh-comp).
;;;
;;; Invocation:
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-relative-subject-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "clause-grammar.lisp"
                         (or *load-truename* *compile-file-truename*))))

(in-package :parsifal)


(defun gram1-relative-subject-test (&optional verbose)
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
           (noun-of (np)
             (and np (getr 'word (daughter 'noun (daughter 'nbar np)))))
           (pron-of (np)
             (and np (getr 'word (daughter 'pronoun np)))))

      ;; Fully dictionary-driven: the/boy/who/met/you/scheduled/meeting/. .
      (reset-rule-table)
      (register-grammar *np-rules* *clause-rules* *vp-np-rule* *pronoun-rule*
                        *inf-complement-rules* *wh-question-rules*
                        *relative-clause-rules*)

      (let ((ok (parse-sentence "the boy who met you scheduled the meeting ."
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (check "parse succeeds on the canonical sentence" ok t)
        (truthy "the main clause is a major declarative" (subsetp '(decl major s) (fe c)))

        ;; --- main clause ---
        (let* ((subj (daughter 'np c))
               (vp   (daughter 'vp c))
               (obj  (and vp (daughter 'np vp))))
          (truthy "the subject NP is present" subj)
          (check "the subject head is \"boy\"" (noun-of subj) 'boy)
          (truthy "the subject NP is `modified' (carries a relative clause)"
                  (and subj (member 'modified (fe subj))))
          (check "the main verb is \"schedule\""
                 (and vp (getr 'word (daughter 'verb vp))) 'scheduled)
          (check "the main object head is \"meeting\"" (noun-of obj) 'meeting)

          ;; --- relative clause inside the subject ---
          (let* ((rel     (and subj (daughter 's subj)))
                 (whc     (and rel (getr :wh-comp rel)))
                 (relsubj (and rel (daughter 'np rel)))
                 (relvp   (and rel (daughter 'vp rel)))
                 (relobj  (and relvp (daughter 'np relvp))))
            (truthy "a relative S hangs off the subject NP" rel)
            (truthy "the relative S is sec/relative"
                    (and rel (subsetp '(sec relative s) (fe rel))))
            (truthy "the relativizer `who' is the relative wh-comp"
                    (and whc (subsetp '(relpron-np wh) (fe whc))))
            (check "the relativizer is bound to the head NP (the boy)"
                   (and whc (getr 'binding whc)) subj)
            (truthy "the relative subject is a trace" (and relsubj (member 'trace (fe relsubj))))
            (check "the relative subject trace is bound to `who'"
                   (and relsubj (getr 'binding relsubj)) whc)
            (check "the relative verb is \"meet\""
                   (and relvp (getr 'word (daughter 'verb relvp))) 'met)
            (check "the relative clause's object is \"you\"" (pron-of relobj) 'you))

          (truthy "final punctuation attached" (daughter 'finalpunc c))

          ;; --- main-clause case frame ---
          (let ((cf (and vp (getr 'caseframe vp))))
            (truthy "the main VP has a case frame" cf)
            (check "the main predicate is schedule" (and cf (get cf 'pred)) 'schedule)
            (closeframe openframe)
            (let ((filled (cadr (first (get cf 'hypo-slots)))))
              (truthy "the main frame has filled cases" filled)
              (let ((agt (assoc 'agt filled)) (neut (assoc 'neut filled)))
                (truthy "an agent case was filled" agt)
                (check "the agent is the (relative-modified) subject NP -- the boy"
                       (cadr agt) (daughter 'np c))
                (truthy "a neutral case was filled" neut)
                (check "the neutral case is the main object -- the meeting"
                       (cadr neut) (daughter 'np vp))))))))

    (format t "~&gram1-relative-subject-test: ~:[FAILED <<<~;passed~]~%" results)
    results))


(gram1-relative-subject-test t)
