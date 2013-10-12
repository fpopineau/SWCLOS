;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; OWL OneOf Module
;;;
;;; IT Program Project in Japan:
;;;    Building Operation-Support System for Large-scale System using IT.
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan.
;;;
;;; Copyright (c) 2007,2008 Seiji Koide
;;;
;; History
;; -------
;; 2008.01.18    OneOf module is extracted from leanOWL
;;;
;;; Based on the set theoretic semantics of RDF, an instance of vin:WineBody, vin:Light, is also an
;;; instance of OneOf{Light} class and OneOf{Light Medium} class. Furthermore, OneOf{Light} class
;;; is a subclass of OneOf{Light Medium} class, and OneOf{Light Medium} class is a subclass of
;;; OneOf{Light Medium Full} class or vin:WineBody. Such relationship is depicted as follows.
;;; ----------------------------------------------------------------------------------
;;;                            |owl|:|Class-----------owl|:OneOf
;;;                           /                   /
;;;                |rdfs|:|Class----------------OneOf|
;;;               /                            :............................
;;;              /                             :               :           :
;;;             /                              :    +-----{Medium Full}--{Full}
;;;            /                               :   /                    X
;;;  |owl|:|Thing| -------vin:WineTaste----vin:WineBody--------{Light Full}  {Medium}
;;;                                               \                     X
;;;                                                 +----{Light Medium}--{Light}
;;;                                         ................................:
;;;                                         :
;;;                                     vin:Light
;;; ----------------------------------------------------------------------------------
;;; In this implementation, we set up |owl|:|OneOf| metaclass as subclass of |owl|:|Class|, and
;;; every OneOf class is generated under |owl|:|OneOf| metaclass.

(in-package :gx)

;;;
;;;; Delay Evaluation Role for |owl|:|oneOf|
;;;

(eval-when (:execute :load-toplevel :compile-toplevel)
  (set-delay-role '|owl|:|oneOf|))

#|
  (when (delay-role-p role)
    ;(format t "~%DELAY: addForm ~S ~S" form role)
    (return-from addForm (delay (addForm form role))))
|#
;;;
;;;; Top Level addObject:around for OneOf
;;;
;;; Adding an OneOf form like below causes |owl|:|OneOf| metaclass in addObject:around methods.
;;; ----------------------------------------------------------------------------------
;;; (addForm '(|owl|:|Class| WineBody (|owl|:|oneOf| Full Medium Light)))
;;; ----------------------------------------------------------------------------------
;;; Note that there is no around method in RDFS module and OWL module.
#|
(defmethod addObject :around ((type |rdfs|:|Class|) slot-forms)
  "If slot-forms include an |owl|:|oneOf| slot, <type> is set to |owl|:|OneOf|."
  (assert (not (eq (car (mop:class-direct-superclasses |owl|:|Thing|)) |owl|:|Thing|)))
  (cond ((assoc '|owl|:|oneOf| slot-forms)
         ;(format t "~%ADDOBJECT:AROUND for |owl|:|oneOf| ~S ~S" type slot-forms)
         (let* ((name (second (assoc ':name slot-forms)))
                (ones (cdr (assoc '|owl|:|oneOf| slot-forms)))
                (oneofclass OneOf))
           (cond ((and name (boundp name) (owl-oneof-p (symbol-value name)))
                  (let ((old-ones (slot-value (symbol-value name) '|owl|:|oneOf|))
                        (new-ones ones))
                    (cond ((and (subsetp old-ones new-ones :test #'%owl-same-p)
                                (subsetp new-ones old-ones :test #'%owl-same-p))
                           (symbol-value name))
                          (t (error 'oneof-condition-unsatiafiable
                               :format-control "~S vs. ~S~%  members of two groups must be equivalent."
                               :format-arguments `(,(symbol-value name) ,new-ones))))))
                 (t (call-next-method oneofclass slot-forms domains)))))
        (t (call-next-method))))
|#
;;;
;;;; Reinitialize-Instance :around for OneOf
;;;
;;; This is called when a predefined class with entailment is regularly defined.

(defmethod reinitialize-instance :around ((class OneOf) &rest initargs)
  (let ((name (and (slot-boundp class 'excl::name) (slot-value class 'excl::name)))
        ;; note that method name for OneOf creates consed name.
        (ones (getf initargs '|owl|:|oneOf|))
        (supers (getf initargs :direct-superclasses)))
    ;(format t "~%Supers:~S" supers)
    (loop for one in ones
        do
          (cond ((cl:typep (class-of one) 'OneOf)
                 (let* ((oneclass (class-of one))
                        (onename (and (slot-boundp oneclass 'excl::name) (slot-value oneclass 'excl::name))))
                   (when (and (null name) onename) (setq name onename))
                   (unless (eql class oneclass)
                     (cond ((and (subsetp (slot-value oneclass '|owl|:|oneOf|) ones :test #'%owl-same-p)
                                 (subsetp ones (slot-value oneclass '|owl|:|oneOf|) :test  #'%owl-same-p))
                            ;(format t "~%~S = ~S" class oneclass)
			    )
                           ((subsetp (slot-value oneclass '|owl|:|oneOf|) ones :test #'%owl-same-p)
                            ;(format t "~%~S > ~S" class oneclass)
                            (reinitialize-instance oneclass :direct-superclasses `(,class)))
                           ((subsetp ones (slot-value oneclass '|owl|:|oneOf|) :test  #'%owl-same-p)
                            ;(format t "~%~S < ~S" class oneclass)
                            (pushnew oneclass supers))
                           ((error "Cant happen!"))))))
                (t (change-class one class))))
    (setq initargs (copy-list initargs))
    (remf initargs :direct-superclasses)
    (remf initargs :name)
    (cond (name (apply #'call-next-method class :name name :direct-superclasses supers initargs))
          (t (apply #'call-next-method class :direct-superclasses supers initargs)))))

;;;
;;;; Shared-Initialize:After for OneOf
;;;
;;; OneOf class is directed in addObject:around method
;;;

(defmethod shared-initialize :after ((class OneOf) slot-names &rest initargs)
  (cond ((and (null slot-names) (null initargs))
         )
        ((and (consp slot-names) (null initargs))
         )
        ((and (null slot-names) (null (getf initargs '|owl|:|oneOf|)))
         ;(format t "~%SHARED-INITIALIZE:AFTER ??? in OneOf ~S ~S ~S" class slot-names initargs)
         (assert (null (intersection (mop:class-direct-superclasses class)
                                     (mop:class-direct-subclasses class))))
         )
        ((and (eq slot-names t) (getf initargs '|owl|:|oneOf|)) ; first definition
         ;(format t "~%SHARED-INITIALIZE:AFTER initialDefinition in OneOf~%  ~S~%  ~S~%  ~S" class slot-names initargs)
         ;(format t "~%MMMCLASSES2:~S" (mclasses class))
         (loop for one in (getf initargs '|owl|:|oneOf|)
             do (cond ((cl:typep one class))
                      ((and (cl:typep (class-of one) 'OneOf) (%oneof-equal (slot-value (class-of one) '|owl|:|oneOf|) class)))
                      ((named-p class)
                       ;(format t "~%Change class ~S to ~S" one class)
                       (change-class one class)
                       ))
               (when (and (not (shadow-class-p one)) (name one))
                 (push `(,(name one) line ,*line-number*) *defined-resources*)))
         ;(format t "~%MMMCLASSES2:~S" (mclasses class))
         )
        ((and (null slot-names) (getf initargs '|owl|:|oneOf|))
         ;(format t "~%SHARED-INITIALIZE:AFTER reinitialize in OneOf ~S ~S ~S" class slot-names initargs)
         (loop for one in (getf initargs '|owl|:|oneOf|)
             do (cond ((cl:typep one class)
                       (when (shadow-class-p (class-of one))
                         (let ((shadow (class-of one))
                               (mclasses (mclasses one)))
                           (cond ((cl:member class mclasses)
                                  (let ((newclasses (set-difference mclasses mclasses
                                                                    :test #'(lambda (subsumer subsumee)
                                                                              (and (not (eql subsumer subsumee))
                                                                                   (subsumed-p subsumee subsumer))))))
                                    ;; newclasses are most specific subsumers in sense of oneof.
                                    ;(format t "~%newclasses:~S" newclasses)
                                    (unless (set-equalp newclasses (mclasses one))
                                      (cond ((null (cdr newclasses))
                                             ;(format t "~%Unshadowing for ~S to ~S" one (car newclasses))
                                             (error "Check it2!")
                                             (change-class one (car newclasses)))
                                            ((error "Not Yet!"))))))
                                 ((and (null (cdr mclasses)) (cl:typep (car mclasses) 'OneOf))
                                  (with-slots (|owl|:|oneOf|) (car mclasses)
                                    (cond ((null |owl|:|oneOf|) (error "Cant happen!"))
                                          ((null (cdr |owl|:|oneOf|))  ; one element
                                           ;(format t "~%Unshadowing for ~S to ~S" shadow (class-of (car mclasses)))
                                           (reinitialize-instance shadow :name cl:nil '|owl|:|oneOf| (cl:list one))
                                           (change-class shadow (class-of (car mclasses)))
                                           )
                                          (t  ;; nothing done
                                             ))))
                                 (t ;; nothing done
                                  )))))
                      (t ;(format t "~%Change class ~S to ~S" one class)
                         (change-class one class)))
               (when (and (not (shadow-class-p one)) (name one))
                 (push `(,(name one) line ,*line-number*) *defined-resources*)))
         )
        (t ;; redefinition
         ;(format t "~%SHARED-INITIALIZE:AFTER redefinition in OneOf ~S ~S ~S" class slot-names initargs)
         (let ((ones (mklist (slot-value class '|owl|:|oneOf|))))
           ;(format t "~%REDEFINED to ~S" ones)
           (slot-makunbound class '|owl|:|oneOf|)
           (setf (slot-value class '|owl|:|oneOf|)
             (loop for one in ones
                 do ;(format t "~%Forced ~S -> " one)
                   (setq one (force one))
                   ;(prin1 one)
                   (when (and (not (shadow-class-p one)) (name one))
                     (push `(,(name one) line ,*line-number*) *defined-resources*))
                 collect one))
           ;(format t "~%Class ~S" class)
           ;; in making mclasses, delete the rdf-equally duplicates of classes but
           ;; instances are returned from primary shared-initialize and handled normaly.
           ;; you cannot retlieve all occurence of classes of oneOf from mclasses but
           ;; collect-all-extensions-of is effective in collection of instances
           (loop for one in (mklist (slot-value class '|owl|:|oneOf|))
               when (not (find class (mclasses one) :test #'rdf-equalp))
               do ;(format t "~%Changing class ~S -> ~S" one class)
                 (change-class one class))
           ))))

;;;
;;;; Change Class for OneOf
;;;
;;; In the process, if it is found that an rdf source object has an oneOf slot, the object must be
;;; an instance of |owl|:|OneOf| metaclass. The following method must be called. See also <owl-oneof-p>.

(defmethod change-class ((instance |rdfs|:|Resource|) (new-class OneOf) &rest initargs)
  (let ((old-class (class-of instance)))
    ;(format t "~%OldClass:~S NewClass:~S" old-class new-class)
    ;(format t "~%old-supers:~S" (mop:class-direct-superclasses old-class))
    (cond ((and (shadow-class-p old-class)
                (null (cdr (mop:class-direct-superclasses old-class)))
                (owl-equivalent-p new-class (car (mop:class-direct-superclasses old-class))))
           (call-next-method))
          ((cl:subtypep old-class new-class) (call-next-method))
          ((cl:subtypep new-class old-class) (call-next-method))
          ((and (subsumed-p new-class old-class)
                (subsumed-p old-class new-class))
           (call-next-method))
          ((subsumed-p old-class new-class)
           (reinitialize-instance old-class :direct-superclasses `(,new-class))
           (apply #'reinitialize-instance old-class initargs))
          ((subsumed-p new-class old-class)
           (reinitialize-instance new-class :direct-superclasses `(,old-class))
           (call-next-method))
          ((owl-oneof-p old-class)
           (let ((old-ones (slot-value old-class '|owl|:|oneOf|))
                 (new-ones (slot-value new-class '|owl|:|oneOf|)))
             (let ((meta (car (most-specific-concepts
                               (mapcar #'(lambda (one) (class-of (class-of one)))
                                 (append old-ones new-ones)))))
                   (intersects (intersection old-ones new-ones))
                   (old-diff (set-difference old-ones new-ones))
                   (new-diff (set-difference new-ones old-ones)))
               ;(format t "~%OldOnes:~S~%NewOnes:~S" old-ones new-ones)
               ;(format t "~%meta:~S~%intersects:~S~%old-diff:~S~%new-diff:~S" meta intersects old-diff new-diff)
               (assert old-diff)
               (assert new-diff)
               (let ((intersects-class
                      (mop:ensure-class-using-class cl:nil cl:nil
                                                    :metaclass meta
                                                    :direct-superclasses `(,new-class ,old-class)
                                                    '|owl|:|oneOf| intersects))
                     (old-diff-class
                      (mop:ensure-class-using-class cl:nil cl:nil
                                                    :metaclass meta
                                                    :direct-superclasses `(,old-class)
                                                    '|owl|:|oneOf| old-diff))
                     (new-diff-class
                      (mop:ensure-class-using-class cl:nil cl:nil
                                                    :metaclass meta
                                                    :direct-superclasses `(,new-class)
                                                    '|owl|:|oneOf| new-diff)))
                 ;(format t "~%intersect-class:~S~%old-diff-class:~S~%new-diff-class:~S"
                 ;  intersects-class old-diff-class new-diff-class)
                 instance)))
          (t (call-next-method)))))

(defmethod change-class :after ((class |rdfs|:|Class|) (new-class (eql OneOf)) &rest initargs)
  (declare (ignore initargs))
  (loop for one in (and (slot-boundp class '|owl|:|oneOf|) (slot-value class '|owl|:|oneOf|))
      do (change-class one class)))

(defmethod change-class :after ((class |rdfs|:|Class|) (new-class (eql |owl|:|OneOf|)) &rest initargs)
  (declare (ignore initargs))
  (loop for one in (and (slot-boundp class '|owl|:|oneOf|) (slot-value class '|owl|:|oneOf|))
      do (change-class one class)))
