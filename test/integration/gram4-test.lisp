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
  "Bring every parser global back to a clean post-load shape so each
   scenario runs against a known baseline."
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
        *wstring*        nil
        *as-types*       nil)
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
  "Three end-to-end scenarios from gram4.l:

   (1) NUMBER-DONE on a single num word ---> labels + packet shift.
   (2) NINETY-NINE chain on `thirty third'  ---> quant = 33, ord set.
   (3) AS-rule chain (NUMBER + TWO-HUNDRED)
       on `two hundred'                     ---> quant = 200.

   Returns T iff every check passes."
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
      (setq *as-types* (list (intern "NUM" :glang-cl)))
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
