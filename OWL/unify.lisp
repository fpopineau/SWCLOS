;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; Unify Module
;;;
;;; This code is written by Seiji Koide.
;;;
;;; Copyright (c) 2009 Seiji Koide
;;;
;; History
;; -------
;; 2009.11.09    File created.

;;;
;;;
;;;

(defvar *new-variable-counter* 0)

(defconstant +fail+ nil "Indicates unification failure")

(defconstant +no-bindings+ '((nil))
  "Indicates unification success, with no variables.")

(defun binary-p (model)
  (and (consp model)
       (= 3 (length model))))

(defun unary-p (model)
  (and (consp model)
       (= 2 (length model))))

(defun binary-unify (binary models bindings)
  "returns a list of possible bindings between <bindary> and some of <models>."
  (loop for model in (remove-if-not #'binary-p models) with new-bindings
      unless (setq new-bindings (unify binary model bindings))
      collect new-bindings))

(defun unary-tunify (unary models bindings)
  "returns a list of possible bindings between <uniary> and some of <models>.
   Note that this unification takes care of type unification,"
  (loop for model in (remove-if-not #'unary-p models) with new-bindings
      unless (setq new-bindings (tunify unary model bindings))
      collect new-bindings))

;;;
;;;
;;;

(defun unify (x y &optional (bindings +no-bindings+))
  "See if x and y match with given bindings.  If they do,
  return a binding list that would make them equal [p 303]."
  (cond ((eq bindings +fail+) +fail+)
        ((eql x y) bindings)
        ((variable? x) (unify-var x y bindings))
        ((variable? y) (unify-var y x bindings))
        ((and (consp x) (consp y))
         (cond ((and (symbolp (second x)) (symbolp (second y)))
                (unify (rest x) (rest y) 
                       (unify (first x) (first y) bindings)))
               ((set-equalp (rest x) (rest y))
                (unify (first x) (first y) bindings))
               (t +fail+)))
        (t +fail+)))

(defun unify-var (var x bindings)
  "Unify var with x, using (and maybe extending) bindings [p 303]."
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable? x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings))
        ((occurs-in? var x bindings)
         +fail+)
        (t (extend-bindings var x bindings))))

(defun make-binding (var val) (cons var val))

(defun new-variable (var)
  "Create a new variable.  Assumes user never types variables of form ?X.9"
  (concat-symbol (if (variable? var) "" "?")
                 var "." (incf *new-variable-counter*)))

(defun variable? (x)
  "Is x a variable (a symbol starting with `?')?"
  (and (symbolp x) (eql (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

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

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (make-binding var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy +no-bindings+
        (if (eq bindings +no-bindings+)
            nil
          bindings)))

(defun occurs-in? (var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq var x) t)
        ((and (variable? x) (get-binding x bindings))
         (occurs-in? var (lookup x bindings) bindings))
        ((consp x) (or (occurs-in? var (first x) bindings)
                       (occurs-in? var (rest x) bindings)))
        (t nil)))

(defun concat-symbol (&rest args)
  "Concatenate the args into one string, and turn that into a symbol."
  (intern (format nil "~{~a~}" args)))

(defun owl-same-p+ (x y bindings)
  (gx:owl-same-p (lookup x y) t))
  )
