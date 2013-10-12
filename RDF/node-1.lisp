;;;-*- Mode: common-lisp; syntax: common-lisp -*-

(cl:provide :rdfnode)

(defpackage :gx
  (:export name))
(in-package :gx)

;;;; gnode & rdf-node
;;; rdf-node will be a superclass of rdfs:Class.
;;; gnode will be a superclass of rdfs:Resource.
;;; gnode class is needed for registration of class-instance relation.

(defclass rdf-node (standard-class)
  ((direct-instances :initarg :direct-instances
                     :initform nil
                     :accessor class-direct-instances))
  (:documentation "This metaclass is node class. This metaclass provides method class-direct-instances"))

#+(or lispworks sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod mop:validate-superclass
      ((class rdf-node)
       (superclass standard-class))
    t))

(defmethod excl::name (class)
  (and (slot-exists-p class 'excl::name)
       (slot-boundp class 'excl::name)
       (slot-value class 'excl::name)))

(defclass gnode ()
  ((excl::name :initarg :name :initform nil)
   (iri :initarg :iri :initform nil :accessor iri)
   (mclasses :initarg :mclasses :initform nil :accessor mclasses)
   (type-tag :initarg :type-tag :initform nil :accessor type-tag)
   ;(inv-plist :initform nil)
   )
  (:metaclass rdf-node)
  (:documentation "This class is needed to maintain mclasses."))


;;; FP addition
(defun make-initargs-from-slotds (slotds)
  (mapcar #'make-initarg-from-slotd slotds))
(defun make-initarg-from-slotd (slotd)
  (loop for facetd in (mop:class-slots (class-of slotd)) with name
      when (and (slot-boundp slotd (setq name (mop:slot-definition-name facetd)))
                (cond ((eq name 'excl::initform) ; (mop:slot-definition-initform slotd)
                       t) ; :initform is needed with initfunction, even if nil
                      ((eq name 'excl::initfunction) ; (mop:slot-definition-initfunction slotd)
                       t) ; :initform is needed with initfunction, even if nil
                      ((eq name 'excl::readers) (slot-value slotd name))
                      ((eq name 'excl::writers) (slot-value slotd name))
                      ((eq name 'common-lisp:type) (not (eq t (slot-value slotd name))))
                      ((eq name #+allegro 'documentation #+lispworks 'mop::documentation-slot) (slot-value slotd name))
                      #-lispworks
                      ((eq name 'excl::fixed-index) nil)
                      (t t)))
      append (cond ((eq name 'excl::name) `(:name ,(mop:slot-definition-name slotd)))
                   ((eq name 'excl::initargs) `(:initargs ,(mop:slot-definition-initargs slotd)))
                   ((eq name 'excl::initform) `(:initform ,(mop:slot-definition-initform slotd)))
                   ((eq name 'excl::initfunction) `(:initfunction ,(mop:slot-definition-initfunction slotd)))
                   ((eq name 'excl::readers) `(:readers ,(slot-value slotd name)))
                   ((eq name 'excl::writers) `(:writers ,(slot-value slotd name)))
                   ((eq name 'common-lisp:type) `(:type ,(mop:slot-definition-type slotd)))
                   ((eq name 'excl::allocation) `(:allocation ,(#+allegro excl::slotd-allocation #+lispworks mop::slot-definition-allocation slotd)))
                   ((eq name #+allegro 'documentation #+lispworks 'mop::documentation-slot) `(:documentation ,(slot-value slotd name)))
                   (t `(,(car (slot-value facetd 'excl::initargs)) ,(slot-value slotd name))))))

(defvar *class-architecture* (list (find-class 'gnode) (find-class 'rdf-node)))

(defvar *class-table* (make-hash-table))

(defvar *class-options*
  '((:direct-superclasses mop:class-direct-superclasses)
    (:direct-subclasses mop:class-direct-subclasses)
    (:precedence-list mop:class-precedence-list)
    (:metaclass class-of)
    (:direct-slots compute-direct-slots-list)
    (:direct-instances class-direct-instances)
    (:default-initargs mop::class-default-initargs)
    (:direct-default-initargs mop::class-direct-default-initargs)))

(defun block-finalization (class)
  (let (result)
    (debug-print "Blocking finalization for ~S " class)
    (setf (gethash class *class-table*) t)
    (loop for c in (mop:class-direct-subclasses class)
       do (debug-print "~S " c)
       do (setf (gethash c *class-table*) t)
       do (push c result))
    (debug-print "~%")
    (cons class result)))

(defun unblock-finalization (classes)
  (debug-print "Unblocking finalization for ")
  (loop for c in classes
       do (debug-print "~S " c)
       do (remhash c *class-table*))
  (debug-print "~%"))

(defvar *finalization-list* ())

(defmethod mop:ensure-class-using-class ((class rdf-node) name &rest args)
  (debug-print "Ensure class using class ~S name ~S with args ~S~%" class name args)
  (call-next-method))

;; What kind of value should that function return ?

(defmethod mop:finalize-inheritance :around ((class rdf-node))
  (unless (member class *class-architecture*)
    (push class *class-architecture*))
  (when (gethash class *class-table*) (debug-print "Finalization of ~S delayed~%" class))
  (unless nil #+nil (gethash class *class-table*)
    (if (member class *finalization-list*)
        (debug-print "Skipped finalization of ~S because of loop~%" class)
        (progn (debug-print "Finalizing ~S~%" class)
               (push class *finalization-list*)
               (funcall #'call-next-method class)
               (pop *finalization-list*))))
  nil)

(defmethod mop:compute-class-precedence-list :around ((class rdf-node))
  (debug-print "Compute class precedence list for ~S = " class)
  (let ((list (funcall #'call-next-method class)))
;;    (setf list (remove class list))
    (debug-print "~S~%" list)
    list))

#+nil
(progn
#-allegro
  (trace mop::reinitialize-instance)
  (trace (mop::finalize-inheritance
          #-allegro :break #-allegro nil))
  (trace mop::shared-initialize)
  (trace check-superclasses-order)
  (trace mop::make-instances-obsolete)
  (trace mop::compute-class-precedence-list)
  (trace (mop::update-instance-for-redefined-class :step nil))
;;  (trace slot-definition-initfunction)
  (trace mop::update-dependents)
  (trace mop::update-instance)
  (trace mop::refinalize-subclasses)
  (trace mop::update-class-and-subclasses)
  (trace mop::direct-subclasses)
  (trace (%clos-subtype-p ;; :backtrace :verbose
                          ;; :step t
          ;; :break t
                          ))
)
#+nil
(defmethod mop:finalize-inheritance :around ((class rdf-node))
  (when (gethash class *class-table*) (debug-print "Finalization of ~S delayed~%" class))
  (unless (gethash class *class-table*)
    (debug-print "Finalizing ~S~%" class)
    (funcall #'call-next-method class))
  nil)

(defun compute-direct-slots-list (class)
  (mapcar #'(lambda (s) (remf s :location) s)
          (gx::make-initargs-from-slotds (mop:class-slots class))))

(defvar *reinitialization-list* ())

(defmethod mop:reinitialize-instance :around ((class rdf-node) &rest options)
  (let (classes-to-reinitialize classes-to-finalize)
    (unless (member class *class-architecture*)
      (push class *class-architecture*))
    (setf options (apply #'foo class options))
    (debug-print "****************~%Reinitialize instance (rdf-node) of ~S with options ~{~%~S~}~%" class options)
    (setf classes-to-finalize (block-finalization class))
    (push class *reinitialization-list*)
    (apply #'call-next-method class options)
    ;; fix sub/super classes
    (unless (equal (mop:class-direct-subclasses class)
                   (getf options :direct-subclasses))
      (debug-print "Replacing new subclasses ~S with old ones ~S~%"
              (mop:class-direct-subclasses class)
              (getf options :direct-subclasses))
      (setf (mop:class-direct-subclasses class)
            (getf options :direct-subclasses)))
    (unless (equal (mop:class-direct-superclasses class)
                   (getf options :direct-superclasses))
      (debug-print "Replacing new superclasses ~S with old ones ~S~%"
              (mop:class-direct-superclasses class)
              (getf options :direct-superclasses))
      (setf (mop:class-direct-superclasses class)
            (getf options :direct-superclasses)))
    ;; ensure that superclasses have this class as subclass
    ;; and reinit them
    (loop for c in (mop:class-direct-superclasses class)
         when (not (member class (mop:class-direct-subclasses c)))
         do (progn
              (push c classes-to-reinitialize)
              (debug-print "Push ~S to subclasses of ~S~%" class c)
              (push class (mop:class-direct-subclasses c))))
    ;; ensure that subclasses have this class as superclass
    ;; and reinit them
    (loop for c in (mop:class-direct-subclasses class)
       when (not (member class (mop:class-direct-superclasses c)))
       do (progn
            (debug-print "Push ~S to superclasses of ~S~%" class c)
            (push class (mop:class-direct-superclasses c))))
    #+nil (debug-print "Classes to reinitialize : ~S~%" classes-to-reinitialize)
    #+nil (loop for c in classes-to-reinitialize
       do (unless (member c *reinitialization-list*)
            (debug-print "Calling for reinitialization of instance ~S~%" c)
            (mop:reinitialize-instance c)))

    #+nil (apply #'call-next-method class options)

    (loop for c in (mop:class-direct-subclasses class)
       do (unless (member c *reinitialization-list*)
            (debug-print "Calling for reinitialization of instance ~S~%" c)
            (mop:reinitialize-instance c)))
    (remhash class *class-table*)
    (debug-print "Calling for finalization of ~S~%" class)
    (mop:finalize-inheritance class)
    (unblock-finalization classes-to-finalize)
    (loop for c in (mop:class-direct-subclasses class)
       do (debug-print "Calling for finalization of ~S~%" c)
       do (mop:finalize-inheritance c))
    (pop *reinitialization-list*)
    (debug-print "~% blocked finalization ")
    (maphash #'(lambda (k v) (debug-print "~S " k)) *class-table*)
    (debug-print "~%")
    (debug-print "End of reinitialize instance of ~S~%****************~%" class)
    ))

(defun print-relations (&optional stage)
  (when stage (debug-print "Stage ~S~%" stage))
  (loop for c in (reverse *class-architecture*)
       do (mop:finalize-inheritance c)
     do (debug-print "Class : ~S~%subclasses = ~S~%superclasses = ~S~%"
                c (mop:class-direct-subclasses c) (mop:class-direct-superclasses c))))

#+nil
(defmethod mop:reinitialize-instance :around ((class gnode) &rest options)
  (setf options (apply #'foo class options))
  (debug-print "Reinitialize instance (gnode) of ~S with options ~S~%" class options)
  (apply #'call-next-method class options))

(defun foo (class &rest options)
  (let (new-options)
    (setf new-options
          (loop for (option accessor) in *class-options*
             do (debug-print "option ~S accessor ~S~%" option accessor)
             collect option
             collect (cond
                       ;; we add slots
                       ((eq option :direct-slots) (remove-duplicates
                                                   (append (funcall accessor class) (getf options :direct-slots) )
                                                   :key #'(lambda (s) (getf s :name))))
                       ;; we replace other options
                       ((getf options option))
                       ;;               ((getf (cdr (assoc class *class-architecture*)) option))
                       (t (funcall accessor class)))))
    (loop
       while options
       do (progn
            (unless (member (car options) *class-options*)
              (push (second options) new-options)
              (push (first options) new-options))
            (pop options) (pop options)))
    new-options))

;;; An element of direct-instances slot are initially stored by <make-instance(rdf-node)> method
;;; and maintained by <update-instance-for-different-class:after(gnode)> which is invoked by
;;; change-class.

(defmethod make-instance ((class rdf-node) &rest initargs)
  (declare (ignore initargs))
  (let ((instance (call-next-method)))
    (push instance (class-direct-instances class))
    instance))

(defun shadowed-class-p (x)
  "returns true if <x> is an instance of shadowed class.
   shadowed-class is defined at RdfsObjects file."
  (eq (class-name (class-of x)) 'shadowed-class))

(defmethod update-instance-for-different-class :after ((previous gnode) current &rest initargs)
  (declare (ignore initargs))
  (cond ((cl:typep current 'destroyed-class)
         (let ((old-class (class-of previous)))
           (setf (class-direct-instances old-class)
             (remove current (class-direct-instances old-class) :test #'eq))
           ))
        (t (let ((old-class (class-of previous))
                 (new-class (class-of current)))
             ;; domain constraint should be satisfied, if old-class was satisfied.
             ;; class direct instances handling
             (setf (class-direct-instances old-class)
               (remove current (class-direct-instances old-class) :test #'eq))
             (push current (class-direct-instances new-class))
             ;; mclasses handling
             (cond ((shadowed-class-p current)
                    (labels ((get-bright-supers (super)
                                                (cond ((not (shadowed-class-p super)) (list super))
                                                      (t (mapcan #'get-bright-supers (mop:class-direct-superclasses super))))))
                      (setf (mclasses current)
                        (remove-duplicates (mapcan #'get-bright-supers (mop:class-direct-superclasses new-class))))))
                   (t (setf (mclasses current) (list new-class))))))))

(defun node-p (x)
  (cl:typep x 'gnode))

(defun bnode-p (node)
  (or (not (slot-value node 'excl::name))
      (not (symbol-package (slot-value node 'excl::name)))))

(defmethod ground? ((node gnode))
  (and (slot-value node 'excl::name)
       (symbol-package (slot-value node 'excl::name))))

(defmethod name ((node symbol))
  node)

(defmethod name ((node gnode))
  "returns a QName or a nodeID of <node>, if it exists. Otherwise nil."
  (let ((name (slot-value node 'excl::name)))
    (when (and name (symbol-package name)) name))) ; name might have uninterned symbol.

(defmethod (setf name) (symbol (node gnode))
  "exports <symbol> for QName."
  (setf (slot-value node 'excl::name) symbol)
  (export-as-QName symbol)
  (setf (symbol-value symbol) node))
