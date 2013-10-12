;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;; This file is manually copied by Seiji from TR1993-017, Technical Report of 
;;; Mitsubishi Electric Research Laboratories, Author Richard C. Waters 
;;; See also, http://www.merl.com/papers/TR93-17/
;;; The copyright of this program belongs Mitsubishi Electric Research Laboratories. 
;;; See also the copyright warranty at TR1993-017.

(defpackage :mexp)

(in-package :mexp)

(export '(macroexpand-all))

(defun macroexpand-all (f &optional env)
  (mexp (copy-tree f) env))

(defun mexp (f env &aux (flag t) m)
  (loop
    (cond ((atom f)
           (return f))
          ((not (symbolp (car f)))
           (return (all-mexp f env)))
          ((setq m (get (car f) 'mexp))
           (return (funcall m f env)))
          ((not flag)
           (return (funcall-mexp f env))))
    (multiple-value-setq (f flag)
      (macroexpand-1 f env))))

(defun all-mexp (list env)
  (do ((f list (cdr f))
       (r () (cons (mexp (car f) env) r)))
      ((atom f) (nreconc r f))))

(defun funcall-mexp (f env)
  `(,(car f) ,@(all-mexp (cdr f) env)))

(defun quote-mexp (f env)
  (declare (ignore env))
  f)

(defun block-mexp (f env)
  `(,(car f)
      ,(cadr f)
      ,@(all-mexp (cddr f) env)))

(defun let-mexp (f env)
  `(,(car f)
      ,(mapcar #'(lambda (p)
                   (bind-mexp p env))
         (cadr f))
      ,@(all-mexp (cddr f) env)))

(defun bind-mexp (p env)
  (if (and (consp p) (consp (cdr p)))
      (list (car p) (mexp (cadr p) env))
    p))

(defun lambda-mexp (f env)
  `(,(car f)
      ,(mapcar #'(lambda (p)
                   (arg-mexp p env))
         (cadr f))
      ,@(all-mexp (cddr f) env)))

(defun arg-mexp (arg env)
  (if (and (consp arg) (consp (cdr arg)))
      `(,(car arg)
          ,(mexp (cadr arg) env)
          ,@(cddr arg))
    arg))

(defun get-var (b)
  (if (consp b) (car b) b))

(defun get-val (b)
  (eval (if (consp b) (cadr b) nil)))

(defun compiler-let-mexp (f env)
  (progv (mapcar #'get-var (cadr f))
    (mapcar #'get-val (cadr f))
    (mexp
     (if (null (cdddr f))
         (caddr f)
       `(let nil ,@(cddr f)))
     env)))

(defun macrolet-mexp (f env)
  (with-env env `(macrolet ,(cadr f))
    #'mexp
    (if (null (cdddr f))
        (caddr f)
      `(let nil ,@(cddr f)))))

(defun flet-mexp (f env)
  `(flet
       ,(all-lambda-mexp (cadr f) env)
     ,@(with-env env `(flet ,(cadr f))
         #'all-mexp
         (cddr f))))

(defun labels-mexp (f env)
  (with-env env `(labels ,(cadr f))
    #'labels-mexp-2 f))

(defun labels-mexp-2 (f env)
  `(labels
       ,(all-lambda-mexp (cadr f) env)
     ,@(all-mexp (cddr f) env)))

(defun all-lambda-mexp (list env)
  (mapcar #'(lambda (f)
              (lambda-mexp f env))
    list))

(mapc #'(lambda (x)
          (setf (get (car x) 'mexp)
            (eval (cadr x))))
  '((block #'block-mexp)
    (catch #'funcall-mexp)
    (compiler-let #'compiler-let-mexp)
    (declare #'quote-mexp)
    (eval-when #'block-mexp)
    (flet #'flet-mexp)
    (function #'funcall-mexp)
    (go #'quote-mexp)
    (if #'funcall-mexp)
    (labels #'labels-mexp)
    (lambda #'lambda-mexp)
    (let #'let-mexp)
    (let* #'let-mexp)
    (macrolet #'macrolet-mexp)
    (multiple-value-call #'funcall-mexp)
    (multiple-value-prog1 #'funcall-mexp)
    (progn #'funcall-mexp)
    (progv #'funcall-mexp)
    (quote #'quote-mexp)
    (return-from #'block-mexp)
    (setq #'funcall-mexp)
    (tagbody #'funcall-mexp)
    (the #'block-mexp)
    (throw #'funcall-mexp)
    (unwind-protect #'funcall-mexp)))

(defmacro grab-env (fn x
                       &environment env)
  `',(funcall fn x env))

(defun aug-env (env form fn x)
  (evalhook `(,@ form (grab-env ,fn ,x))
            nil nil env))

#+(or :SYMBOLICS :AKCL :CORAL :FRANZ-INC)
(defun with-env (env form fn x)
  (aug-env (convert-env env) form fn x))

#+(or :SYMBOLICS :AKCL)
(defun convert-env (env)
  env)

#+:CORAL
(defun convert-env (env)
  (list nil env nil nil nil nil))

#+:FRANZ-INC
(defun convert-env (env)
  (list nil env nil nil))

#+:LUCID
(defun with-env (env form fn x)
  (aug-env nil
           form
           #'with-appended-env
           (list env fn x)))

#+:LUCID
(defun with-appended-env (z delta)
  (let ((env (car z))
        (fn (cadr z))
        (x (caddr z)))
    (funcall fn x (append delta env))))

(defun with-env (env form bind fn body)
  (funcall fn body
           (if (eq form 'macrolet)
               (augment-env env :macro
                            (mapcar #'parse bind))
             (augment-env env :function
                          (mapcar #'car bind)))))
(defun parse (b)
  (list (car b)
        (parse-macro (car b)
                     (cadr b)
                     (cddr b)
                     env)))