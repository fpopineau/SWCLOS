
(defclass foo (standard-class)
  ((x :initarg :x)))

(defmethod aclmop::validate-superclass
      ((class foo)
       (superclass standard-class))
  t)
(defmethod aclmop::validate-superclass
      ((class standard-class)
       (superclass foo))
  t)

(defclass bar () ()
  (:metaclass foo))

(format t "~S~%" (aclmop::class-direct-superclasses (find-class 'bar)))

(reinitialize-instance (find-class 'bar) :direct-superclasses (list (find-class 'foo)))

(format t "~S~%" (aclmop::class-direct-superclasses (find-class 'bar)))

(defparameter *bar-1* (make-instance 'bar))

(defclass baz (bar) ((y :initarg :y)))

(reinitialize-instance (find-class 'bar)
                       :direct-slots `((:name z :initargs (:z))))

(format t "~S~%" (aclmop::class-direct-superclasses (find-class 'bar)))

(describe (make-instance 'baz :y 3 :z 4))

#+lispworks
(defmethod (setf class-direct-superclasses) :around (new-value
                                                     (instance foo))
  (if (equal new-value
             (list (find-class 'standard-object)))
      (format t "trapping setf direct superclasses~%")
      (class-direct-superclasses instance))
  (call-next-method new-value instance))

(defmethod shared-initialize :after ((instance baz) slot-names &rest initargs)
  (reinitialize-instance (find-class 'foo)
                         :direct-slots `((:name x :initargs (:x))
                                         (:name new :initargs (:new)))))

(describe (make-instance 'baz :z 3 ))

(format t "~S~%" (aclmop::class-direct-superclasses (find-class 'bar)))

