;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; test/integration/gram4-test.lisp
;;;
;;; End-to-end integration tests for the parsifal runtime + the
;;; glang-cl rule-language compiler. Each scenario compiles a chunk
;;; of Marcus's gram4.l verbatim, runs it through PARSE-LOOP on a
;;; hand-built word stream, and checks the resulting *deriv* trace
;;; and node state.
;;;
;;; The tests are deliberately NOT part of the parsifal ASDF system:
;;;
;;;   - The unit-test suite (`test-all') runs against :parsifal alone,
;;;     so reloading the system can't accidentally trigger a full
;;;     parse cycle.
;;;   - The integration tests need glang-cl, which lives under
;;;     system/reference/ and isn't loaded by the system either.
;;;
;;; Invocation:
;;;
;;;   sbcl --noinform --non-interactive \
;;;        --load test/integration/gram4-test.lisp
;;;
;;; or, interactively:
;;;
;;;   (load "test/integration/gram4-test.lisp")     ; one-shot, prints
;;;   (parsifal::gram4-test t)                      ; verbose re-run

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


;;; ===========================================================
;;; Helpers
;;; ===========================================================

(defun reset-parser ()
  "Bring every per-parse global back to a clean shape so each
   scenario runs against a known baseline.

   Configuration values that defs.l populates -- *as-types*,
   *nr-types*, *parts-of-speech*, *sentence-types*, etc. -- are
   *not* reset here. They're load-time state, not per-parse state."
  (setq *bufpntr*       0
        *bufmax*       -1
        *bufpntrstak*   nil
        *activenodestak* nil
        *activepackets*  nil
        *activerule*     nil
        *nextrule*       nil
        *parsecomplete*  nil
        *deriv*          nil
        *current-s*      nil
        *wh-comp*        nil
        *1stfeat*        nil
        *2ndfeat*        nil
        *3rdfeat*        nil
        |1ST|            nil
        |2ND|            nil
        |3RD|            nil
        c                nil
        s                nil
        nth              nil
        *wstring*        nil)
  (dotimes (i (length *buffer*))
    (setf (aref *buffer* i) nil))
  (reset-rule-table))

(defun mkword (feat-names &optional q)
  "Build a fresh word-node whose feature list is the given NAMES
   interned in :glang-cl, with an optional `quant' register value Q."
  (let* ((h (gensym "W-"))
         (w (cons h 0)))
    (setf (symbol-value h)
          (mapcar (lambda (n) (intern n :glang-cl)) feat-names)
          (symbol-plist h) nil)
    (when q (setr (intern "QUANT" :glang-cl) q w))
    w))

(defun mkplaceholder-c ()
  "Create and assign a placeholder c-node so newnode's ACTIVATENODE
   has a parent to push onto *activenodestak*. In a real parse this
   would be an S-node established by an initial rule."
  (let* ((h (gensym "S-"))
         (n (cons h 0)))
    (setf (symbol-value h) (list (intern "S" :glang-cl))
          (symbol-plist h) nil)
    (setq c n)
    n))

(defun bootstrap-loop (initial-packets)
  "Install a no-op INIT as the initial rule and activate the given
   packet names (strings) in :glang-cl."
  (setq *activepackets* (mapcar (lambda (p) (intern p :glang-cl))
                                initial-packets)
        *activerule*    (list 'init (lambda () nil))))

(defun register-rules (srcs)
  "Compile and EVAL each source string so rule-index lands the rule
   in *rule-table* and its action functions become callable."
  (dolist (src srcs)
    (eval (glang-cl::compile-rule-form (glang-cl::parse-rule src)))))

(defun deriv-names ()
  "Symbol-NAMES of every fired rule, in firing order (oldest first),
   excluding the bootstrap INIT. Lets us compare against literal
   strings without worrying about which package each rule lives in."
  (reverse (mapcar #'symbol-name (butlast *deriv*))))


;;; ===========================================================
;;; The integration test
;;; ===========================================================

(defun gram4-test (&optional verbose)
  "End-to-end scenarios from gram4.l. Each compiles a chunk of
   Marcus's rules verbatim and runs PARSE-LOOP on a hand-built
   word stream. Returns T iff every check passes."
  (let ((results t))
    (flet ((check (test-name actual expected)
             (let ((pass (equal actual expected)))
               (when (or verbose (not pass))
                 (format t "~&  ~:[FAIL~;pass~]  ~a~%" pass test-name)
                 (unless pass
                   (format t "    expected: ~s~%    actual:   ~s~%"
                           expected actual)))
               (setf results (and pass results)))))


      ;; --- Scenario 1: NUMBER-DONE on a single num word --------------

      (reset-parser)
      (let ((w (mkword '("NUM" "TENS") 30)))
        (setq *wstring* (list w)))
      ;; Pre-populate the bufpntr stack so NUMBER-DONE's "Restore the
      ;; buffer." has something to pop. (In a real parse the AS rule
      ;; that activated BUILD-NUMBER would have pushed it.)
      (setq *bufpntrstak* '(0))
      (register-rules
       '("{RULE NUMBER-DONE PRIORITY: 12 IN BUILD-NUMBER
          [t] -->
          Label 1st complete-num.
          If 1st is not ord then label 1st quant.
          If 1st is none of ns,npl then label 1st npl.
          Deactivate build-number.
          Activate npool.
          Restore the buffer.}"
         "{RULE FINAL IN NPOOL
          [* is complete-num] -->
          Parse is finished.}"))
      (bootstrap-loop '("BUILD-NUMBER"))

      (check "NUMBER-DONE: parse-loop succeeds"
             (parse-loop)
             t)
      (check "NUMBER-DONE: trace"
             (deriv-names)
             '("NUMBER-DONE" "FINAL"))
      (check "NUMBER-DONE: word labelled complete-num"
             (member (intern "COMPLETE-NUM" :glang-cl)
                     (fe (aref *buffer* 0))
                     :test #'eq)
             (member (intern "COMPLETE-NUM" :glang-cl)
                     (fe (aref *buffer* 0))
                     :test #'eq))                       ; truthy
      (check "NUMBER-DONE: packets flipped to NPOOL"
             (equal *activepackets*
                    (list (intern "NPOOL" :glang-cl)))
             t)


      ;; --- Scenario 2: NINETY-NINE chain on `thirty third' -----------

      (reset-parser)
      (let ((thirty (mkword '("TENS" "NUM")        30))
            (third  (mkword '("ONES" "NUM" "ORD")   3)))
        (setq *wstring* (list thirty third)))
      (mkplaceholder-c)
      (setq *bufpntrstak* '(0))
      (register-rules
       '("{RULE NINETY-NINE IN BUILD-NUMBER
          [=tens] [=ones] -->
          Label a new num node 99s.
          Attach 1st to c as num1.
          Attach 2nd to c as num2.
          Set the quant of c to
               plus(the quant register of 1st, the quant register of 2nd).
          Transfer ord from 2nd to c.
          Drop c.}"
         "{RULE NUMBER-DONE PRIORITY: 12 IN BUILD-NUMBER
          [t] -->
          Label 1st complete-num.
          If 1st is not ord then label 1st quant.
          If 1st is none of ns,npl then label 1st npl.
          Deactivate build-number.
          Activate npool.
          Restore the buffer.}"
         "{RULE FINAL IN NPOOL
          [* is complete-num] -->
          Parse is finished.}"))
      (bootstrap-loop '("BUILD-NUMBER"))

      (check "NINETY-NINE: parse-loop succeeds"
             (parse-loop)
             t)
      (check "NINETY-NINE: trace"
             (deriv-names)
             '("NINETY-NINE" "NUMBER-DONE" "FINAL"))
      (check "NINETY-NINE: 30 + 3 = 33"
             (getr (intern "QUANT" :glang-cl) (aref *buffer* 0))
             33)
      (check "NINETY-NINE: result has ord (transferred from 2nd)"
             (member (intern "ORD" :glang-cl)
                     (fe (aref *buffer* 0))
                     :test #'eq)
             (member (intern "ORD" :glang-cl)
                     (fe (aref *buffer* 0))
                     :test #'eq))                       ; truthy


      ;; --- Scenario 3: NUMBER (AS) + TWO-HUNDRED on `two hundred' ----

      (reset-parser)
      (let ((two     (mkword '("ONES"     "NUM")   2))
            (hundred (mkword '("*HUNDRED" "NUM") 100)))
        (setq *wstring* (list two hundred)))
      (mkplaceholder-c)
      (register-rules
       '("{AS RULE NUMBER IN NPOOL
          [=num ; * is not complete-num] -->
          Deactivate npool.
          Activate build-number.}"
         "{RULE TWO-HUNDRED IN BUILD-NUMBER
          [ * is any of ones, *ten, *10, 99s; * is not ord] [=*hundred] -->
          Label a new num node hundred+.
          Attach 1st to c as num1.
          Attach 2nd to c as num2.
          Set the quant of c to
               times(the quant register of 1st, the quant register of 2nd).
          Transfer ord from 2nd to c.
          Drop c.}"
         "{RULE NUMBER-DONE PRIORITY: 12 IN BUILD-NUMBER
          [t] -->
          Label 1st complete-num.
          If 1st is not ord then label 1st quant.
          If 1st is none of ns,npl then label 1st npl.
          Deactivate build-number.
          Activate npool.
          Restore the buffer.}"
         "{RULE FINAL IN NPOOL
          [* is complete-num] -->
          Parse is finished.}"))
      (bootstrap-loop '("NPOOL"))

      (check "TWO-HUNDRED: parse-loop succeeds"
             (parse-loop)
             t)
      (check "TWO-HUNDRED: trace"
             (deriv-names)
             '("NUMBER" "TWO-HUNDRED" "NUMBER-DONE" "FINAL"))
      (check "TWO-HUNDRED: 2 * 100 = 200"
             (getr (intern "QUANT" :glang-cl) (aref *buffer* 0))
             200)
      (check "TWO-HUNDRED: result is the new hundred+ node"
             (member (intern "HUNDRED+" :glang-cl)
                     (fe (aref *buffer* 0))
                     :test #'eq)
             (member (intern "HUNDRED+" :glang-cl)
                     (fe (aref *buffer* 0))
                     :test #'eq))                       ; truthy


      ;; --- Scenario 4: three-word number via 99S-ATTACH --------------
      ;;
      ;; `three hundred forty' --> 340. Exercises the BIGNUM-builder
      ;; path: TWO-HUNDRED makes 300, HUNDREDS-STARTS-BIGNUMG promotes
      ;; it to a bignumg, 99S-ATTACH attaches forty as num2 and sums.

      (reset-parser)
      (let ((three   (mkword '("ONES"     "NUM")   3))
            (hundred (mkword '("*HUNDRED" "NUM") 100))
            (forty   (mkword '("TENS"     "NUM")  40)))
        (setq *wstring* (list three hundred forty)))
      (mkplaceholder-c)
      (register-rules
       '("{AS RULE NUMBER IN NPOOL
          [=num ; * is not complete-num] -->
          Deactivate npool.
          Activate build-number.}"
         "{RULE TWO-HUNDRED IN BUILD-NUMBER
          [ * is any of ones, *ten, *10, 99s; * is not ord] [=*hundred] -->
          Label a new num node hundred+.
          Attach 1st to c as num1.
          Attach 2nd to c as num2.
          Set the quant of c to
               times(the quant register of 1st, the quant register of 2nd).
          Transfer ord from 2nd to c.
          Drop c.}"
         "{RULE HUNDREDS-STARTS-BIGNUMG IN BUILD-NUMBER
          [ * is any of hundred+, bignum+; * is not ord] -->
          Create a new num node labelled bignumg.
          Attach 1st to c as num1.
          Activate build-number.}"
         "{RULE 99S-ATTACH IN BUILD-NUMBER
          [t] [** c; = bignumg] -->
          If there is a conj of c or 1st is any of 99s, tens, ones then
                  Attach 1st to c as num2;
                  Transfer ord from 1st to c;
                  Set the quant of c to
                          plus (the quant register of num1 of c,
                                  the quant register of 1st)
                  else set the quant register of c to
                                  the quant register of num1 of c.
          Drop c.}"
         "{RULE NUMBER-DONE PRIORITY: 12 IN BUILD-NUMBER
          [t] -->
          Label 1st complete-num.
          If 1st is not ord then label 1st quant.
          If 1st is none of ns,npl then label 1st npl.
          Deactivate build-number.
          Activate npool.
          Restore the buffer.}"
         "{RULE FINAL IN NPOOL
          [* is complete-num] -->
          Parse is finished.}"))
      (bootstrap-loop '("NPOOL"))

      (check "three-hundred-forty: parse-loop succeeds"
             (parse-loop)
             t)
      (check "three-hundred-forty: trace"
             (deriv-names)
             '("NUMBER" "TWO-HUNDRED" "HUNDREDS-STARTS-BIGNUMG"
               "99S-ATTACH" "NUMBER-DONE" "FINAL"))
      (check "three-hundred-forty: 3 * 100 + 40 = 340"
             (getr (intern "QUANT" :glang-cl) (aref *buffer* 0))
             340)
      (check "three-hundred-forty: result is a bignumg node"
             (member (intern "BIGNUMG" :glang-cl)
                     (fe (aref *buffer* 0))
                     :test #'eq)
             (member (intern "BIGNUMG" :glang-cl)
                     (fe (aref *buffer* 0))
                     :test #'eq))                       ; truthy


      ;; --- Scenario 5: BIGNUM rule -- `three thousand' -> 3000 -------
      ;;
      ;; Exercises Marcus's `BIGNUM' [=num] [=bignum] rule, plus the
      ;; SET* edge case of running off the end of input mid-pattern:
      ;; after the rule fires once and drops a new bignum+ node into
      ;; the buffer, BIGNUM's pattern would re-match a (bignum+)
      ;; against itself unless SET* properly clears the feature
      ;; vector for the position that just ran dry.

      (reset-parser)
      (let ((three    (mkword '("ONES"   "NUM")    3))
            (thousand (mkword '("BIGNUM" "NUM") 1000)))
        (setq *wstring* (list three thousand)))
      (mkplaceholder-c)
      (register-rules
       '("{AS RULE NUMBER IN NPOOL
          [=num ; * is not complete-num] -->
          Deactivate npool.
          Activate build-number.}"
         "{RULE BIGNUM IN BUILD-NUMBER
          [=num; * is not bignumg, ord] [=bignum; * is not *hundred] -->
          Create a new num node labelled bignum+.
          Attach 1st to c as num1.
          Attach 2nd to c as num2.
          Set the quant of c to
               times(the quant register of 1st, the quant register of 2nd).
          Transfer ord from 1st to c.
          Drop c.}"
         "{RULE NUMBER-DONE PRIORITY: 12 IN BUILD-NUMBER
          [t] -->
          Label 1st complete-num.
          If 1st is not ord then label 1st quant.
          If 1st is none of ns,npl then label 1st npl.
          Deactivate build-number.
          Activate npool.
          Restore the buffer.}"
         "{RULE FINAL IN NPOOL
          [* is complete-num] -->
          Parse is finished.}"))
      (bootstrap-loop '("NPOOL"))

      (check "three-thousand: parse-loop succeeds"
             (parse-loop)
             t)
      (check "three-thousand: trace"
             (deriv-names)
             '("NUMBER" "BIGNUM" "NUMBER-DONE" "FINAL"))
      (check "three-thousand: 3 * 1000 = 3000"
             (getr (intern "QUANT" :glang-cl) (aref *buffer* 0))
             3000)
      (check "three-thousand: result is a bignum+ node"
             (member (intern "BIGNUM+" :glang-cl)
                     (fe (aref *buffer* 0))
                     :test #'eq)
             (member (intern "BIGNUM+" :glang-cl)
                     (fe (aref *buffer* 0))
                     :test #'eq))                       ; truthy


      ;; --- Scenario 6: four-word number, seven-rule chain ------------
      ;;
      ;; `thirty three hundred forty' -> 3340 = (30 + 3) * 100 + 40.
      ;; Fires every gram4.l number-builder rule we've ported plus
      ;; the AS rule, the final labeler, and the sentinel:
      ;;   NUMBER -> NINETY-NINE -> TWO-HUNDRED ->
      ;;   HUNDREDS-STARTS-BIGNUMG -> 99S-ATTACH ->
      ;;   NUMBER-DONE -> FINAL.

      (reset-parser)
      (let ((thirty  (mkword '("TENS"     "NUM")  30))
            (three   (mkword '("ONES"     "NUM")   3))
            (hundred (mkword '("*HUNDRED" "NUM") 100))
            (forty   (mkword '("TENS"     "NUM")  40)))
        (setq *wstring* (list thirty three hundred forty)))
      (mkplaceholder-c)
      (register-rules
       '("{AS RULE NUMBER IN NPOOL
          [=num ; * is not complete-num] -->
          Deactivate npool.
          Activate build-number.}"
         "{RULE NINETY-NINE IN BUILD-NUMBER
          [=tens] [=ones] -->
          Label a new num node 99s.
          Attach 1st to c as num1.
          Attach 2nd to c as num2.
          Set the quant of c to
               plus(the quant register of 1st, the quant register of 2nd).
          Transfer ord from 2nd to c.
          Drop c.}"
         "{RULE TWO-HUNDRED IN BUILD-NUMBER
          [ * is any of ones, *ten, *10, 99s; * is not ord] [=*hundred] -->
          Label a new num node hundred+.
          Attach 1st to c as num1.
          Attach 2nd to c as num2.
          Set the quant of c to
               times(the quant register of 1st, the quant register of 2nd).
          Transfer ord from 2nd to c.
          Drop c.}"
         "{RULE HUNDREDS-STARTS-BIGNUMG IN BUILD-NUMBER
          [ * is any of hundred+, bignum+; * is not ord] -->
          Create a new num node labelled bignumg.
          Attach 1st to c as num1.
          Activate build-number.}"
         "{RULE 99S-ATTACH IN BUILD-NUMBER
          [t] [** c; = bignumg] -->
          If there is a conj of c or 1st is any of 99s, tens, ones then
                  Attach 1st to c as num2;
                  Transfer ord from 1st to c;
                  Set the quant of c to
                          plus (the quant register of num1 of c,
                                  the quant register of 1st)
                  else set the quant register of c to
                                  the quant register of num1 of c.
          Drop c.}"
         "{RULE NUMBER-DONE PRIORITY: 12 IN BUILD-NUMBER
          [t] -->
          Label 1st complete-num.
          If 1st is not ord then label 1st quant.
          If 1st is none of ns,npl then label 1st npl.
          Deactivate build-number.
          Activate npool.
          Restore the buffer.}"
         "{RULE FINAL IN NPOOL
          [* is complete-num] -->
          Parse is finished.}"))
      (bootstrap-loop '("NPOOL"))

      (check "thirty-three-hundred-forty: parse-loop succeeds"
             (parse-loop)
             t)
      (check "thirty-three-hundred-forty: 7-rule trace"
             (deriv-names)
             '("NUMBER" "NINETY-NINE" "TWO-HUNDRED"
               "HUNDREDS-STARTS-BIGNUMG" "99S-ATTACH"
               "NUMBER-DONE" "FINAL"))
      (check "thirty-three-hundred-forty: (30+3)*100 + 40 = 3340"
             (getr (intern "QUANT" :glang-cl) (aref *buffer* 0))
             3340)
      (check "thirty-three-hundred-forty: result is a bignumg node"
             (member (intern "BIGNUMG" :glang-cl)
                     (fe (aref *buffer* 0))
                     :test #'eq)
             (member (intern "BIGNUMG" :glang-cl)
                     (fe (aref *buffer* 0))
                     :test #'eq))                       ; truthy


      ;; --- Scenario 7: five-word number, NINETY-NINE fires twice -----
      ;;
      ;; `thirty three hundred thirty three' -> 3333.
      ;; (30 + 3) * 100 + (30 + 3) = 3300 + 33 = 3333.
      ;; NINETY-NINE fires once at the start to produce the 33 that
      ;; feeds TWO-HUNDRED, then again inside the bignumg context
      ;; to produce the trailing 33 that 99S-ATTACH sums.

      (reset-parser)
      (let ((thirty1 (mkword '("TENS"     "NUM")  30))
            (three1  (mkword '("ONES"     "NUM")   3))
            (hundred (mkword '("*HUNDRED" "NUM") 100))
            (thirty2 (mkword '("TENS"     "NUM")  30))
            (three2  (mkword '("ONES"     "NUM")   3)))
        (setq *wstring* (list thirty1 three1 hundred thirty2 three2)))
      (mkplaceholder-c)
      (register-rules
       '("{AS RULE NUMBER IN NPOOL
          [=num ; * is not complete-num] -->
          Deactivate npool.
          Activate build-number.}"
         "{RULE NINETY-NINE IN BUILD-NUMBER
          [=tens] [=ones] -->
          Label a new num node 99s.
          Attach 1st to c as num1.
          Attach 2nd to c as num2.
          Set the quant of c to
               plus(the quant register of 1st, the quant register of 2nd).
          Transfer ord from 2nd to c.
          Drop c.}"
         "{RULE TWO-HUNDRED IN BUILD-NUMBER
          [ * is any of ones, *ten, *10, 99s; * is not ord] [=*hundred] -->
          Label a new num node hundred+.
          Attach 1st to c as num1.
          Attach 2nd to c as num2.
          Set the quant of c to
               times(the quant register of 1st, the quant register of 2nd).
          Transfer ord from 2nd to c.
          Drop c.}"
         "{RULE HUNDREDS-STARTS-BIGNUMG IN BUILD-NUMBER
          [ * is any of hundred+, bignum+; * is not ord] -->
          Create a new num node labelled bignumg.
          Attach 1st to c as num1.
          Activate build-number.}"
         "{RULE 99S-ATTACH IN BUILD-NUMBER
          [t] [** c; = bignumg] -->
          If there is a conj of c or 1st is any of 99s, tens, ones then
                  Attach 1st to c as num2;
                  Transfer ord from 1st to c;
                  Set the quant of c to
                          plus (the quant register of num1 of c,
                                  the quant register of 1st)
                  else set the quant register of c to
                                  the quant register of num1 of c.
          Drop c.}"
         "{RULE NUMBER-DONE PRIORITY: 12 IN BUILD-NUMBER
          [t] -->
          Label 1st complete-num.
          If 1st is not ord then label 1st quant.
          If 1st is none of ns,npl then label 1st npl.
          Deactivate build-number.
          Activate npool.
          Restore the buffer.}"
         "{RULE FINAL IN NPOOL
          [* is complete-num] -->
          Parse is finished.}"))
      (bootstrap-loop '("NPOOL"))

      (check "thirty-three-hundred-thirty-three: parse-loop succeeds"
             (parse-loop)
             t)
      (check "thirty-three-hundred-thirty-three: 8-rule trace, NINETY-NINE twice"
             (deriv-names)
             '("NUMBER" "NINETY-NINE" "TWO-HUNDRED"
               "HUNDREDS-STARTS-BIGNUMG" "NINETY-NINE"
               "99S-ATTACH" "NUMBER-DONE" "FINAL"))
      (check "thirty-three-hundred-thirty-three: 33 * 100 + 33 = 3333"
             (getr (intern "QUANT" :glang-cl) (aref *buffer* 0))
             3333)
      (check "thirty-three-hundred-thirty-three: result is a bignumg node"
             (member (intern "BIGNUMG" :glang-cl)
                     (fe (aref *buffer* 0))
                     :test #'eq)
             (member (intern "BIGNUMG" :glang-cl)
                     (fe (aref *buffer* 0))
                     :test #'eq))                       ; truthy


      ;; --- Scenario 8: BIGNUM-flavored 5-word number -----------------
      ;;
      ;; `thirty three thousand thirty three' -> 33033.
      ;; (30 + 3) * 1000 + (30 + 3) = 33000 + 33 = 33033.
      ;; Same length and chain depth as scenario 7 but using BIGNUM
      ;; (num + bignum) in the middle instead of TWO-HUNDRED
      ;; (ones-or-99s + *hundred). NINETY-NINE still fires twice;
      ;; HUNDREDS-STARTS-BIGNUMG promotes the bignum+ result the
      ;; same way it does for hundred+, demonstrating the bignumg
      ;; builder works against either path.

      (reset-parser)
      (let ((thirty1  (mkword '("TENS"   "NUM")     30))
            (three1   (mkword '("ONES"   "NUM")      3))
            (thousand (mkword '("BIGNUM" "NUM")   1000))
            (thirty2  (mkword '("TENS"   "NUM")     30))
            (three2   (mkword '("ONES"   "NUM")      3)))
        (setq *wstring* (list thirty1 three1 thousand thirty2 three2)))
      (mkplaceholder-c)
      (register-rules
       '("{AS RULE NUMBER IN NPOOL
          [=num ; * is not complete-num] -->
          Deactivate npool.
          Activate build-number.}"
         "{RULE NINETY-NINE IN BUILD-NUMBER
          [=tens] [=ones] -->
          Label a new num node 99s.
          Attach 1st to c as num1.
          Attach 2nd to c as num2.
          Set the quant of c to
               plus(the quant register of 1st, the quant register of 2nd).
          Transfer ord from 2nd to c.
          Drop c.}"
         "{RULE BIGNUM IN BUILD-NUMBER
          [=num; * is not bignumg, ord] [=bignum; * is not *hundred] -->
          Create a new num node labelled bignum+.
          Attach 1st to c as num1.
          Attach 2nd to c as num2.
          Set the quant of c to
               times(the quant register of 1st, the quant register of 2nd).
          Transfer ord from 1st to c.
          Drop c.}"
         "{RULE HUNDREDS-STARTS-BIGNUMG IN BUILD-NUMBER
          [ * is any of hundred+, bignum+; * is not ord] -->
          Create a new num node labelled bignumg.
          Attach 1st to c as num1.
          Activate build-number.}"
         "{RULE 99S-ATTACH IN BUILD-NUMBER
          [t] [** c; = bignumg] -->
          If there is a conj of c or 1st is any of 99s, tens, ones then
                  Attach 1st to c as num2;
                  Transfer ord from 1st to c;
                  Set the quant of c to
                          plus (the quant register of num1 of c,
                                  the quant register of 1st)
                  else set the quant register of c to
                                  the quant register of num1 of c.
          Drop c.}"
         "{RULE NUMBER-DONE PRIORITY: 12 IN BUILD-NUMBER
          [t] -->
          Label 1st complete-num.
          If 1st is not ord then label 1st quant.
          If 1st is none of ns,npl then label 1st npl.
          Deactivate build-number.
          Activate npool.
          Restore the buffer.}"
         "{RULE FINAL IN NPOOL
          [* is complete-num] -->
          Parse is finished.}"))
      (bootstrap-loop '("NPOOL"))

      (check "thirty-three-thousand-thirty-three: parse-loop succeeds"
             (parse-loop)
             t)
      (check "thirty-three-thousand-thirty-three: 8-rule trace (BIGNUM)"
             (deriv-names)
             '("NUMBER" "NINETY-NINE" "BIGNUM"
               "HUNDREDS-STARTS-BIGNUMG" "NINETY-NINE"
               "99S-ATTACH" "NUMBER-DONE" "FINAL"))
      (check "thirty-three-thousand-thirty-three: 33 * 1000 + 33 = 33033"
             (getr (intern "QUANT" :glang-cl) (aref *buffer* 0))
             33033)
      (check "thirty-three-thousand-thirty-three: result is a bignumg node"
             (member (intern "BIGNUMG" :glang-cl)
                     (fe (aref *buffer* 0))
                     :test #'eq)
             (member (intern "BIGNUMG" :glang-cl)
                     (fe (aref *buffer* 0))
                     :test #'eq))                       ; truthy

      results)))


;;; ===========================================================
;;; Script entry point
;;; ===========================================================
;;;
;;; Always run the test on load so the file works both as a CI-style
;;; script (sbcl --load) and as an interactive sanity check.

(let ((pass (gram4-test t)))
  (format t "~&~%gram4-test: ~:[failed <<<~;passed~]~%" pass)
  pass)
