;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Lowercase: Yes -*-
;;;
;;; Existential passive under the WHOLE grammar:
;;;   "Is there a meeting scheduled for friday ?"
;;;
;;; The curated `gram1-existential-passive-test' proves this parses in ISOLATION
;;; -- a grammar without *relative-clause-rules*. This test proves it under
;;; *full-grammar*, where Marcus's REDUCED-RELATIVE would otherwise fire on
;;; [a meeting][scheduled], commit to a participial reduced-relative reading his
;;; grammar cannot finish, and dead-end at TOO-MANY-NPS. Our extension override
;;; (*existential-relative-rules*, EXTENSIONS.md #8) adds one guard so
;;; REDUCED-RELATIVE defers in an existential `there' clause; THERE then wins the
;;; fork and `scheduled for friday' parses as the passive predicate.
;;;
;;; This is the regression guard for that override: it checks the parse is the
;;; existential-passive one (existential + np-preposed, `scheduled' the passive
;;; VP verb with `a meeting' as its object trace), not merely non-NIL.
;;;
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram1-existential-full-test.lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "system/grammar/clause-grammar.lisp"
                         (asdf:system-source-directory :parsifal))))

(in-package :parsifal)

(defun gram1-existential-full-test (&optional verbose)
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
           (noun-of (np) (and np (getr 'word (daughter 'noun (daughter 'nbar np))))))

      ;; The WHOLE grammar -- the override must win the reduced-relative fork here.
      (load-full-grammar)

      (let ((ok (parse-sentence "is there a meeting scheduled for friday ?"
                                :initial-rule (intern "INITIAL-RULE" :parsifal))))
        (truthy "parses under *full-grammar* (override wins the fork)" ok)

        ;; The parse is the existential-passive one, not a mangled relative.
        (truthy "the clause is a yes-no question (quest/ynquest/major)"
                (subsetp '(quest ynquest major s) (fe c)))
        (truthy "the clause is `existential'" (member 'existential (fe c)))
        (truthy "the clause is `np-preposed' (passive)" (member 'np-preposed (fe c)))

        ;; Two NP daughters: existential `there' + logical `a meeting'.
        (let* ((nps (daughters 'np c))
               (there-np (find-if (lambda (n) (eq (noun-of n) 'there)) nps))
               (meeting-np (find-if (lambda (n) (eq (noun-of n) 'meeting)) nps)))
          (truthy "the existential subject `there' is present" there-np)
          (truthy "the logical NP `a meeting' is present" meeting-np)

          (let* ((vp   (daughter 'vp c))
                 (verb (and vp (daughter 'verb vp)))
                 (obj  (and vp (daughter 'np vp)))
                 (pp   (and vp (daughter 'pp vp))))
            (truthy "a VP attached to S" vp)
            (check "the predicate verb is \"schedule\"" (and verb (getr 'word verb)) 'scheduled)
            (truthy "the predicate verb is passive" (and verb (member 'passive (fe verb))))

            ;; `a meeting' is the OBJECT of schedule, via an object trace.
            (truthy "the VP object is a trace" (and obj (member 'trace (fe obj))))
            (check "the object trace is bound to `a meeting'"
                   (and obj (getr 'binding obj)) meeting-np)

            ;; `for friday' is a PP under the VP.
            (truthy "a PP attached under the VP" pp)
            (check "the PP preposition is \"for\""
                   (and pp (getr 'word (daughter 'prep pp))) 'for)
            (check "the PP object is \"friday\"" (and pp (noun-of (daughter 'np pp))) 'friday)))))

    (format t "~&gram1-existential-full-test: ~:[FAILED <<<~;passed~]~%" results)
    results))

(gram1-existential-full-test t)
