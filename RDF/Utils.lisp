;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; Utilities for SWCLOS and Rbase from AIMA and others
;;; Utilities in this file are taken from AIMA and redefined in package gx.
;;; ==================================================================================

(cl:provide :utils)

(cl:defpackage :gx
  (:use :common-lisp)
  (:export mappend))

(in-package :gx)

;;.............................................................................................
;;
;; Some Utilities
;;

#+(or allegro lispworks)
(defmacro debug-print (&rest args)
  `(when gx-system::*debug-print*
     (format *error-output* ,@args)))

(declaim (inline mkatom mklist mappend set-equal set-eq))
(defun mkatom (x)
  "If <x> is an atom, return it; otherwise if one length list, return the element, else returns <x>"
  (if (atom x) x
    (if (null (cdr x)) (car x) x)))

;; from aima
(defun mklist (x)
  "If <x> is a list, return it; otherwise return a singleton list, (<x>)."
  (if (listp x) x (list x)))

(defun mappend (fn &rest lists)
  "Apply <fn> to respective elements of list(s), and append results."
  (reduce #'append (apply #'mapcar fn lists) :from-end t))

(defun set-equalp (x y)
  "returns true if <x> and <y> is equal as set, the test function is equalp."
  (and (subsetp x y :test #'equalp)
       (subsetp y x :test #'equalp)))

(defun set-eq (x y)
  "returns true if <x> and <y> is equal as set, the test function is eq."
  (and (subsetp x y :test #'eq)
       (subsetp y x :test #'eq)))

(defun length>1 (list)
  "Is this a list of 2 or more elements?"
  (and (consp list) (cdr list)))

(defun length=1 (list)
  "Is this a list of exactly one element?"
  (and (consp list) (null (cdr list))))

(defun length=2 (list)
  "Is this a list of exactly two elements?"
  (and (consp list) (null (cddr list))))

(defun starts-with (list element)
  "Is this a list that starts with the given element?"
  (and (consp list) (eq (car list) element)))

(defun last1 (list)
  "Return the last element of a list."
  (car (last list)))

(defun last2 (lst)
  "Return the last two element of a list."
  (let ((inv (reverse lst)))
    (nreverse (list (car inv) (cadr inv)))))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
    (cons x y)))

;;;
;;;; Delay Evaluation from OnLisp
;;;
;;; Delay mechanism is copied from ``On Lisp'' by Paul Graham.

(defconstant unforced (gensym))

(defstruct delay forced closure)

(defmacro delay (expr)
  (declare (inline))
  (let ((self (gensym)))
    `(let ((,self (make-delay :forced unforced)))
       (setf (delay-closure ,self)
         #'(lambda ()
             (setf (delay-forced ,self) ,expr)))
       ,self)))

(defun force (x)
  (if (delay-p x)
      (if (eq (delay-forced x) unforced)
          (funcall (delay-closure x))
        (delay-forced x))
    x))

;; for SWCLOS connection
(defun delay-role-p (role)
  (declare (inline))
  (get role 'delay))

(defun set-delay-role (role)
  (setf (get role 'delay) t))


#|

(defun sequence-size (x)
  "SEQUENCE-SIZE <sequence>
   returns the size of the sequence."
  (and (sequencep x) (length (mop-slots x))))

(defun sequence->list (sequence)
  "SEQUENCE->LIST <sequence>
   returns a list of the members of the sequence, or the filler of first
   role, of second role, and so on."
  (and sequence
       (progn (assert (sequencep sequence) () "SEQUENCE->LIST: illegal MOP.") t)
       (loop for index from 1 to (sequence-size sequence) with filler
             when (setq filler (role-filler sequence index))
             collect filler)))

(defun sequence-member (mop sequence)
  "SEQUENCE-MEMBER <mop> <sequence>
   returns true if <mop> is a member of <sequence>."
  (and (sequencep sequence)
       (loop for slot in (mop-slots sequence)
            thereis (eql (slot-filler slot) mop))))

(defun sequence-splice (new old sequence)
  "SEQUENCE-SPLICE <mop-list> <mop> <sequence>
   returns a new sequence mop with all the elements of <sequence>, except
   that <mop> is replaced with the elements of <mop-list>.  Note that a NIL
   <mop-list> returns a sequence with <mop> removed."
  (list->sequence
   (loop for mop in (sequence->list sequence)
        append (cond ((eql mop old) new)
                      (t (list mop))))))

(defun sequence-insert (mop sequence)
  "SEQUENCE-INSERT <mop> <sequence>
   returns a new sequence mop with all the elements of <sequence> plus <mop>,
   added at the end."
  (cond ((null mop) sequence)
        ((sequence-member mop sequence) sequence)
        (t (list->sequence (append (sequence->list sequence) (list mop))))))

(defun sequence-add (time data sequence)
  (push `(,time ,data) (mop-slots sequence)))
|#
;;;; And Others, from Winston's Lisp.

(defun squash (x)
  "flattens a nested list <x> and returns a list that includes only atoms."
  (cond ((consp x) (mappend #'squash x))
        (t (list x))))

;;;
;;;; String Pattern
;;;

(defun match (source target &optional (start 0))
  "compares <source> string to <target> string starting <start> in <target>.
   and all characters in <source> are matched to <target> in order, returns true."
  (let ((result (mismatch source target :start2 start :test #'char=)))
    (or (null result)                ; just same string
        (= (length source) result))  ; source is included target and matched
    ))

(defun substitute-pattern (new old sequence &key (start 0))
  (let ((pos nil))
    (cond ((setq pos (position (char old 0) sequence :start start :test #'char=))
           (let ((result (mismatch old sequence :start2 pos :test #'char=)))
             (cond ((null result)                ; just same string
                    (concatenate 'string
                      (subseq sequence 0 pos)
                      new))
                   ((= (length old) result)      ; old is included in sequence and matched
                    (concatenate 'string
                      (subseq sequence 0 pos)
                      new
                      (subseq sequence (+ pos result))))
                   (t (substitute-pattern new old sequence :start (+ pos result))))))
          (t sequence))))

(defun duplicate-p (list &key (test #'eql) key)
  (cond ((null list) nil)
        (key (cond ((member (funcall key (car list)) (cdr list) :test test :key key) t)
                   (t (duplicate-p (cdr list) :test test :key key))))
        (t (cond ((member (car list) (cdr list) :test test) t)
                 (t (duplicate-p (cdr list) :test test))))))

(declaim (inline null-string-p))
(defun null-string-p (str)
  (string= str ""))

;; End of module
;; --------------------------------------------------------------------
;;;
;;; Seiji Koide Nov-15-2010
;;;
