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

(shadowing-import 'excl::name)

(defclass gnode ()
  ((excl::name :initarg :name :initform nil)
   (iri :initarg :iri :initform nil :accessor iri)
   (mclasses :initarg :mclasses :initform nil :accessor mclasses)
   (type-tag :initarg :type-tag :initform nil :accessor type-tag)
   ;(inv-plist :initform nil)
   )
  (:metaclass rdf-node)
  (:documentation "This class is needed to maintain mclasses."))

#+lispworks
(defmethod (setf class-direct-superclasses) :around (new-value
                                                   (instance rdf-node))
  (debug-print "(setf (class-direct-superclasses ~S) ~S)~%" instance new-value)
  (if (equal new-value
               (list (find-class 'standard-object)))
      (progn
        (debug-print "Trapping setf class-direct-superclasses~%")
        (class-direct-superclasses instance))
      (call-next-method new-value instance)))

#+lispworks
(defmethod (setf class-direct-superclasses) :around (new-value
                                                   (instance gnode))
  (debug-print "(setf (class-direct-superclasses ~S) ~S)~%" instance new-value)
  (if (equal new-value
               (list (find-class 'standard-object)))
      (progn
        (debug-print "Trapping setf class-direct-superclasses~%")
        (class-direct-superclasses instance))
      (call-next-method new-value instance)))

#+nil
(defmethod shared-initialize :around ((class rdf-node) slot-names &rest initargs
                                     &key (direct-superclasses nil dsp))
  (remf initargs :direct-superclasses)
  (if (or (eq slot-names t) dsp)
      (call-next-method)
      (apply #'call-next-method
             class slot-names
             :direct-superclasses
             #+nil (remove-duplicates (append (mop::class-direct-superclasses class)
                                              direct-superclasses))
             (mop::class-direct-superclasses class)
             initargs)))
#+nil
(defmethod shared-initialize :around ((class gnode) slot-names &rest initargs
                                     &key (direct-superclasses nil dsp))
  (remf initargs :direct-superclasses)
  (if (or (eq slot-names t) dsp)
      (call-next-method)
      (apply #'call-next-method
             class slot-names
             :direct-superclasses (mop::class-direct-superclasses class)
             initargs)))

;;; FP addition
#+lispworks
(defun make-initargs-from-slotds (slotds)
  (mapcar #'make-initarg-from-slotd slotds))
#+lispworks
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

(defvar *finalization-list* ())

;; What kind of value should that function return ?
#+lispworks
(defmethod mop:finalize-inheritance :around ((class rdf-node))
  (unless (member class *class-architecture*)
    (setf *class-architecture*
          (closette:std-sort-class-list (push class *class-architecture*))))
  (if (gethash class *class-table*)
      ;(format t "Finalization of ~S delayed~%" class)
      (progn
        (debug-print "~%Finalizing ~S~%Finalization list = ~S~%" class *finalization-list*)
        (if (member class *finalization-list*)
            ;(format t "Skipped finalization of ~S because of loop~%" class)
            (progn (debug-print "Finalizing ~S~%" class)
                   (push class *finalization-list*)
                   (funcall #'call-next-method class)
                   (pop *finalization-list*)))))
  nil)

(defun compute-direct-slots-list (class)
  (mapcar #'(lambda (s) (remf s :location) s)
          (gx::make-initargs-from-slotds (mop:class-direct-slots class))))

(defvar *reinitialization-list* ())

#+nil
(defmethod mop:ensure-class-using-class :around ((class rdf-node) name &rest options)
  (debug-print "~%Creating class ~S of name ~S~%" class name)
  (call-next-method))

#+nil
(defmethod mop:ensure-class-using-class :around ((class gnode) name &rest options)
  (debug-print "~%Creating class ~S of name ~S~%" class name)
  (call-next-method))

#+lispworks
(defmethod mop:reinitialize-instance :around ((class rdf-node) &rest options)
  (let (classes-to-reinitialize classes-to-finalize)
    (unless (member class *class-architecture*)
      (setf *class-architecture* (closette:std-sort-class-list (push class *class-architecture*))))
    (setf options (apply #'foo class options))
    ;; do we need to do it ?
    (if
     (every
      #'(lambda (slot-name)
          (let ((accessor (member slot-name *class-options* :key #'car)))
            (equal (or (and accessor (funcall (cadar accessor) class))
                       (and (slot-exists-p class slot-name)
                            (slot-boundp class slot-name)
                            (slot-value class slot-name)))
                   (getf options slot-name))))
      (loop for s in options by #'cddr collect s))
     ;(format t "Avoiding to reinitialize instance ~S with options ~S~%" class options)
     (progn
       ;(format t "****************~%Reinitialize instance (rdf-node) of ~S with options ~{~%~S~}~%" class options)
       (remf options :precedence-list)
       (remf options :direct-subclasses)
       (remf options :default-initargs)
       (remf options :metaclass)
       (apply #'call-next-method class 
	      options)
       ;; fix sub/super classes
       (unless (equal (mop:class-direct-subclasses class)
                      (getf options :direct-subclasses))
         ;(format t "Replacing new subclasses ~S with old ones ~S~%"
                      (mop:class-direct-subclasses class)
                      (getf options :direct-subclasses))
         (setf (mop:class-direct-subclasses class)
               (getf options :direct-subclasses)))
       (unless (equal (mop:class-direct-superclasses class)
                      (getf options :direct-superclasses))
         ;(format t "Replacing new superclasses ~S with old ones ~S~%"
                      (mop:class-direct-superclasses class)
                      (getf options :direct-superclasses))
         (setf (mop:class-direct-superclasses class)
               (getf options :direct-superclasses)))

       (remhash class *class-table*)
       (debug-print "Calling for finalization of ~S~%" class)
       (mop:finalize-inheritance class)
       (debug-print "About to finalize~S~%" (mop:class-direct-subclasses class))
       (loop for c in (mop:class-direct-subclasses class)
          do (mop:finalize-inheritance c))
       ;(format t "End of reinitialize instance of ~S~%****************~%" class)

       (enforce-sub-super-relations *class-architecture*)

       ))
       class
       ))
#+lispworks
(defun enforce-sub-super-relations (classes)
  (let (classes-to-reinitialize)
    (loop for cl in (reverse classes)
       do (progn
            ;; ensure that superclasses have this class as subclass
            ;; and reinit them
            (loop for c in (mop:class-direct-superclasses cl)
               when (not (member cl (mop:class-direct-subclasses c)))
               do (progn
                    (push c classes-to-reinitialize)
                    (debug-print "Push ~S to subclasses of ~S~%" cl c)
                    (push cl (mop:class-direct-subclasses c))))
            ;; ensure that subclasses have this class as superclass
            ;; and reinit them
            (loop for c in (mop:class-direct-subclasses cl)
               when (not (member cl (mop:class-direct-superclasses c)))
               do (progn
                    (push c classes-to-reinitialize)
                    (debug-print "Push ~S to superclasses of ~S~%" cl c)
                    (push cl (mop:class-direct-superclasses c))))))
    (debug-print "Classes to reinitialize : ~S~%" classes-to-reinitialize)
    (loop for c in (reverse (remove-duplicates classes-to-reinitialize))
       do (unless nil #+nil (member c *reinitialization-list*)
            (debug-print "Calling for reinitialization of instance ~S~%" c)
            (mop:reinitialize-instance c)))))

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

#+nil
(defun test-uniq (class &rest options)
  (let ((accessor 'gx::compute-direct-slots-list))
    (append
     (loop for slot-option in (getf options :direct-slots)
        unless (slot-exists-p class (getf slot-option :name))
        collect slot-option)
     (funcall accessor class))))

;;; Apparently, we have to remove those :direct-slots that are already
;;; inherited slots
(defun foo (class &rest options)
  (debug-print "~%FOO Class ~S with options ~S~%" class options)
  (let (new-options
        #+nil (slot-names (mapcar #'(lambda (s) (slot-value s 'excl::name)) (mop::class-slots class))))
    (setf new-options
          (loop for (option accessor) in *class-options*
             do (debug-print "option ~S accessor ~S =>~%result ~S~%" option accessor (funcall accessor class))
             collect option
             collect (cond
                       ;; we add slots
                       #+nil ((eq option :direct-slots)
                        ;; don't duplicate slots
                        (append
                         (loop for slot-option in (getf options :direct-slots)
                            unless (member (getf slot-option :name) slot-names :test #'string=)
                            collect slot-option)
                         (funcall accessor class)))
                       ;; we replace other options
                       ((getf options option))
                       ;;               ((getf (cdr (assoc class *class-architecture*)) option))
                       (t (funcall accessor class)))))
    (loop
       while options
       do (progn
            (unless (member (car options) *class-options* :key #'car)
              (push (second options) new-options)
              (push (first options) new-options))
            (pop options) (pop options)))
    (debug-print "yield new options ~S~%" new-options)
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
  (let ((name (and (slot-exists-p node 'excl::name) (slot-boundp node 'excl::name) (slot-value node 'excl::name))))
    (when (and name (symbol-package name)) name))) ; name might have uninterned symbol.

(defmethod (setf name) (symbol (node gnode))
  "exports <symbol> for QName."
  (setf (slot-value node 'excl::name) symbol)
  (export-as-QName symbol)
  (setf (symbol-value symbol) node))
