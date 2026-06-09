;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-

(in-package :parsifal)

;;; Tests for system/core/runtime/util.lisp.
;;;
;;; (util-test)   -> t if all pass, nil otherwise
;;; (util-test t) -> verbose

(defun util-test (&optional verbose)
  (let ((results t))
    (flet ((check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%"
                           expected actual)))
               (setf results (and pass results))))
           (leaf (word pos)
             ;; A leaf word-node: `word'/`inputpos' registers, origword.
             (let ((n (cons (gensym "W") 0)))
               (setf (symbol-plist (car n)) nil)
               (setr 'word word n)
               (setr 'inputpos pos n)
               (setf (get word 'origword) word)
               n))
           (out (thunk)
             (with-output-to-string (s)
               (let ((*standard-output* s)) (funcall thunk)))))

      ;; --- concat ----------------------------------------------------

      (check "concat joins two symbols' printnames"
             (concat 'foo 'bar) (intern "FOOBAR" :parsifal))
      (check "concat appends a number as its digits, not a char code"
             (concat 'g 5) (intern "G5" :parsifal))
      (check "concat synthesizes a prefixed rule name from a symbol"
             (concat 'act-of- 'run) (intern "ACT-OF-RUN" :parsifal))
      ;; A string contributes its characters verbatim (case-exact),
      ;; while a symbol's printname is upcased by the reader -- so a
      ;; lowercase string part stays lowercase. (concat is normally
      ;; called with symbols/numbers; this just pins the behavior.)
      (check "concat keeps a string part's case verbatim"
             (concat "new" 'york) (intern "newYORK" :parsifal))
      (check "concat interns in :parsifal (EQ to the read-syntax symbol)"
             (concat 'a 'b) 'ab)
      (check "concat of no args is the empty symbol"
             (concat) (intern "" :parsifal))

      ;; --- cat (macro shorthand for concat) --------------------------

      (check "cat expands to a concat call"
             (cat 'foo 'bar) (concat 'foo 'bar))
      (check "cat builds the same prefixed rule name"
             (cat 'act-of- 'run) (intern "ACT-OF-RUN" :parsifal))

      ;; --- phrase display (collectw / phrasify / prphrase / ...) -----
      ;; Tiny tree:  NP -[det]-> "the"(pos 1),  NP -[noun]-> "dog"(pos 2)

      (let ((w-the (leaf 'the 1))
            (w-dog (leaf 'dog 2))
            (np    (cons (gensym "NP") 0)))
        (setf (symbol-plist (car np)) nil)
        (attach np w-the 'det)
        (attach np w-dog 'noun)

        (check "collectw gathers (inputpos . word) for every leaf"
               (sort (collectw np) #'< :key #'car)
               '((1 . the) (2 . dog)))
        (check "phrasify returns the words in input order, origcased"
               (phrasify np) '(the dog))
        (check "nodes-to-words flattens a node list to its words"
               (nodes-to-words (list np)) '(the dog))
        (check "prphrase prints the words concatenated (print2 = princ)"
               (out (lambda () (prphrase '(the dog)))) "THEDOG")
        (check "prphrase returns T"
               (prphrase '(the dog)) t)

        ;; cfprint smoke test: opens NP's frame, prints (output captured
        ;; and discarded) without error, returns T. CERTAIN-FRAME nil =>
        ;; no case lines.
        (let ((openframe nil) (hypo-slots '((nil nil))) (objs-needed 0)
              (pred nil) (certain-frame nil) (ctrace nil)
              (result nil))
          (associate-cf (newcf 'normal) np)
          (out (lambda () (setq result (cfprint np))))
          (check "cfprint runs without error and returns T" result t))

        ;; --- interactive supervisor (super-smqval / fitspg1 / ...) ---
        ;; Drive super-smqval with canned operator input; capture and
        ;; discard its terminal output. super-clears NIL skips cursorpos.
        (flet ((grade (input)
                 (let ((super-clears nil) (certain-frame nil)
                       (openframe nil) (hypo-slots '((nil nil)))
                       (objs-needed 0) (pred nil) (ctrace nil))
                   (associate-cf (newcf 'normal) np)
                   (with-input-from-string (in input)
                     (let ((*standard-input* in)
                           (*standard-output* (make-string-output-stream)))
                       (super-smqval 'agt np np))))))
          (check "super-smqval maps operator grade 2 -> 1"  (grade "2") 1)
          (check "super-smqval maps operator grade 1 -> 0"  (grade "1") 0)
          (check "super-smqval maps operator grade 0 -> -2" (grade "0") -2)
          (check "super-smqval re-prompts on bad input, then accepts"
                 (grade "foo 7 2") 1))

        (let ((super-clears 'no-super))
          (check "super-smqval returns neutral 0 when disabled (no-super)"
                 (super-smqval 'agt np np) 0))

        (check "fitspg1 (undelivered) signals an error"
               (handler-case (progn (fitspg1 nil nil t) :no-error)
                 (error () :errored))
               :errored)

        (check "super-fit-of threads the caseset form's last element as frame"
               (macroexpand-1 '(super-fit-of nd (cases up nil fr)))
               '(super-fit1-of nd (cases up nil fr) fr))))

    results))
