;;; -*- mode: lisp; base: 10; syntax: common-lisp; -*-
;;; system/reference/glang-cl/fixes.lisp
;;;
;;; Fixity macros: NILFIX, PREFIX, SUFFIX, INFIX, INFIXR, INFIXD, DELIM.
;;; These mirror Marcus's macros at glang.l lines 126-139. We bypass his
;;; DEFFIX/ISN/ISP/ISS/ISI/ISM indirection because in CL we can put a
;;; closure directly on the symbol's property list -- the indirection
;;; through a separately-named function was a MacLISP debugging
;;; convenience that we don't need.
;;;
;;; Each macro takes a NAME token and a BODY form. The body is evaluated
;;; at PARSE time, not at definition time. Inside the body, *DRBP* is
;;; bound to the operator's right binding power, and `(RIGHT)' parses
;;; the right operand. For led-style operators *LEFT* is bound to the
;;; current left value.

(in-package #:glang-cl)


(defmacro nilfix (name code)
  "Token NAME has a nud (null denotation) that ignores everything and
   returns CODE. Used for atom-like tokens (`current', `last', etc.)."
  `(setf (get ',name :nud) (lambda () ,code)))


(defmacro prefix (name rbp code)
  "Token NAME is a prefix operator with right binding power RBP.
   At parse time, with *DRBP* bound to RBP, CODE is evaluated and its
   value is returned."
  (let ((rbp-var (gensym "RBP")))
    `(let ((,rbp-var ,rbp))
       (setf (get ',name :nud)
             (lambda ()
               (let ((*drbp* ,rbp-var))
                 ,code))))))


(defmacro suffix (name lbp code)
  "Postfix (suffix) operator with left binding power LBP. *LEFT* is
   bound to the operand."
  `(progn
     (setf (get ',name :lbp) ,lbp)
     (setf (get ',name :led) (lambda () ,code))))


(defmacro infix (name bp code)
  "Left-associative infix with binding power BP for both sides. Inside
   CODE, *LEFT* is the left operand, `(RIGHT)' parses the right operand
   with rbp = BP."
  (let ((bp-var (gensym "BP")))
    `(let ((,bp-var ,bp))
       (setf (get ',name :lbp) ,bp-var)
       (setf (get ',name :led)
             (lambda ()
               (let ((*drbp* ,bp-var))
                 ,code))))))


(defmacro infixr (name bp code)
  "Right-associative infix: lbp = BP, rbp = BP - 1. The lower right
   binding power means a same-precedence operator on the right gets
   absorbed into the right operand."
  (let ((bp-var (gensym "BP"))
        (rbp-var (gensym "RBP")))
    `(let* ((,bp-var ,bp)
            (,rbp-var (1- ,bp-var)))
       (setf (get ',name :lbp) ,bp-var)
       (setf (get ',name :led)
             (lambda ()
               (let ((*drbp* ,rbp-var))
                 ,code))))))


(defmacro infixd (name lbp rbp code)
  "Infix operator with distinct lbp and rbp. Used when neither pure
   left- nor right-associativity is wanted (Marcus uses this for the
   function-call `(' at glang.l line 382)."
  (let ((lbp-var (gensym "LBP"))
        (rbp-var (gensym "RBP")))
    `(let ((,lbp-var ,lbp)
           (,rbp-var ,rbp))
       (setf (get ',name :lbp) ,lbp-var)
       (setf (get ',name :led)
             (lambda ()
               (let ((*drbp* ,rbp-var))
                 ,code))))))


(defmacro delim (name)
  "Mark NAME as a delimiter -- lbp = 0, no nud or led. The parser will
   stop on it (rbp 0 matches lbp 0) but never invoke it."
  `(setf (get ',name :lbp) 0))
