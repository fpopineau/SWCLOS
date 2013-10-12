;;;-*- Mode: Lisp; Syntax: Common-Lisp;  -*-
;;; ==================================================================================
;;;; Unify
;;;
;;; This module is copied from http://www.norvig.com/ and modified by Seiji Koide 
;;; for logic programs in AIMA.
;;; Copyright (c) 2009 Seiji Koide
;;; ==================================================================================
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

(cl:provide :unify)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :utils)
) ; end of eval-when

(defpackage :unify 
  (:use :common-lisp :utils)
  (:export +no-bindings+ +fail+ unify lookup
           variable? new-variable
           ))
(in-package :unify)

;;;
;;; Followings are copied from Allegro Prolog origianal version.
;;;

(defconstant +no-bindings+ '((t . t))
  "Indicates pat-match success, with no variables.")

(defconstant +fail+ nil "Indicates pat-match +fail+ure")

(defparameter *occurs-check* t "Should we do the occurs check?")

(defvar *new-variable-counter* 0)

(defun subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (cond ((eq bindings +fail+) +fail+)
        ((eq bindings +no-bindings+) x)
        ((and (variable? x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((atom x) x)
        (t (reuse-cons (subst-bindings bindings (car x))
                       (subst-bindings bindings (cdr x))
                       x))))

(defun unify (x y &optional (bindings +no-bindings+))
  "See if x and y match with given bindings."
  (cond ((eq bindings +fail+) +fail+)
        ((eql x y) bindings)
        ((variable? x) (unify-variable x y bindings))
        ((variable? y) (unify-variable y x bindings))
        ((and (consp x) (consp y))
         (unify (rest x) (rest y)
                (unify (first x) (first y) bindings)))
        (t +fail+)))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun predicate (relation) (and (consp relation) (first relation)))
(defun arg-rest (relation) (and (consp relation) (cddr relation)))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(eval-when (:execute :load-toplevel :compile-toplevel)
(defmacro make-binding (var val) `(cons ,var ,val))

(defmacro extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (let ((b (gensym)))
    `(cons (make-binding ,var ,val)
	   ;; Once we add a "real" binding,
	   ;; we can get rid of the dummy +no-bindings+
	   (let ((,b ,bindings))
	     (if (and (eq ,b +no-bindings+))
		 nil
        ,b)))))
)

(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable? x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings))
        ((and *occurs-check* (occurs-check var x bindings))
         +fail+)
        (t (extend-bindings var x bindings))))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun variable? (x)
  "Is x a variable (a symbol starting with `?')?"
  (and (symbolp x) (eql (char (symbol-name x) 0) #\?)))

(defun occurs-check (var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq var x) t)
        ((and (variable? x) (get-binding x bindings))
         (occurs-check var (lookup x bindings) bindings))
        ((consp x) (or (occurs-check var (first x) bindings)
                       (occurs-check var (rest x) bindings)))
        (t nil)))

(defun new-variable (var)
  "Create a new variable.  Assumes user never types variables of form ?X.9"
  (concat-symbol (if (variable? var) "" "?")
                 var "." (incf *new-variable-counter*)))

(defun concat-symbol (&rest args)
  "Concatenate the args into one string, and turn that into a symbol."
  (intern (format nil "~{~a~}" args)))

;;;
;;;; Following is copied from prolog.lisp by Norvig
;;;

;; clauses are represented as (head . body) cons cells
(defun clause-head (clause) (first clause))
(defun clause-body (clause) (rest clause))

;; clauses are stored on the predicate's plist
(defun get-clauses (pred) (get pred 'clauses))

(defun rename-variables (x)
  "replace all variables in x with new ones."
  (sublis (mapcar #'(lambda (var) (cons var (gensym (string var))))
                  (variables-in x))
          x))

(defun continue-p ()
  "Ask user if we should continue looking for solutions."
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise 
      ;(format t " Type ; to see more or . to stop")
     (continue-p))))

(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'non-anon-variable? exp))

(defun non-anon-variable? (x)
  (and (variable? x) (not (eq x '?))))

(defun unique-find-anywhere-if (predicate tree
                                &optional found-so-far)
  "return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-anywhere-if
        predicate
        (first tree)
        (unique-find-anywhere-if predicate (rest tree)
                                 found-so-far))))

