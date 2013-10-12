#+lispworks
(setf *handle-existing-defpackage* '(:warn :add))

#+lispworks
(let (#+lispworks (lw:*handle-warn-on-redefinition* :warn)
      #+lispworks (*packages-for-warn-on-redefinition* nil))
  (defmacro system::without-package-locks (&body body)
    `(#+sbcl sb-ext:without-package-locks
             #+allegro excl::without-package-locks
             #+cmu ext:without-package-locks
             #+lispworks let
             #+lispworks
             ((lw:*handle-warn-on-redefinition* :warn)
              (*packages-for-warn-on-redefinition* nil))
             #+clisp ext:without-package-lock #+clisp ()
             #-(or allegro lispworks sbcl clisp cmu)
             progn
             ,@body)))

#+sbcl
(shadowing-import 'sb-ext::without-package-locks :cl-user)

#+lispworks
(shadowing-import 'system::without-package-locks :cl-user)

(defun package-add-nicknames (package-name &rest new-nicknames)
  (let ((old-nicknames (package-nicknames package-name)))
    (without-package-locks
      (rename-package package-name package-name
                      (remove-duplicates (append old-nicknames new-nicknames)
                                         :test #'equal)))))

(defun package-remove-nicknames (package-name &rest new-nicknames)
  (let ((old-nicknames (package-nicknames package-name)))
    (without-package-locks
      (rename-package package-name package-name
                      (set-difference old-nicknames
                                      (mapcar #'(lambda (x)
                                                  (or (stringp x)
                                                      (and (symbolp x) (symbol-name x)))) new-nicknames)
                                      :test #'equal)))))

#+sbcl
(cl-user::package-add-nicknames :sb-pcl :mop)

#+sbcl
(cl-user::package-add-nicknames :sb-impl :excl)

#+sbcl
(cl-user::package-add-nicknames :sb-impl :system)

#+nil
(without-package-locks
  (defun excl::false (&rest args) (declare (ignore args)) nil))

#+lispworks
(package-add-nicknames :clos :mop)

#+lispworks
(package-add-nicknames :system :excl)

(in-package :excl)

#+lispworks  (export 'excl::without-redefinition-warnings 'excl)
#+lispworks  (export 'excl::without-package-locks 'excl)

#+lispworks
(without-package-locks
  (defmacro without-redefinition-warnings (&rest form)
    `(let ((cl::*redefinition-action* :quiet))
       ,@form))
)

(in-package :cl-user)

;;; First test: reinitialize-instance on class during shared-initialize

(defclass foo (standard-class)
  ((x :initarg :x)))

(defmethod mop::validate-superclass
      ((class foo)
       (superclass standard-class))
  t)
(defmethod mop::validate-superclass
      ((class standard-class)
       (superclass foo))
  t)

;;; Needed by LWW 6.0.1
#+(and lispworks (not lispworks6.1))
(defmethod shared-initialize :around ((class foo) slot-names &rest initargs
                                     &key (direct-superclasses nil dsp))
  (format t "initargs = ~S~%direct-superclasses = ~S => ~S~%" initargs direct-superclasses
          dsp)
  (remf initargs :direct-superclasses)
  (format t "initargs = ~S~%direct-superclasses = ~S => ~S~%" initargs direct-superclasses
          dsp)
 (if (or (eq slot-names t) dsp)
     (call-next-method)
   (apply #'call-next-method
          class slot-names
          :direct-superclasses (remove-duplicates (append (mop::class-direct-superclasses class)
                                                          direct-superclasses))
          initargs)))

#+nil
(defmethod (setf class-direct-superclasses) :around (new-value
                                                     (instance foo))
  (if (equal new-value
             (list (find-class 'standard-object)))
      (format t "trapping setf direct superclasses~%")
      (class-direct-superclasses instance))
  (call-next-method new-value instance))

(defclass bar () ()
  (:metaclass foo))

(format t "~S~%" (mop::class-direct-superclasses (find-class 'bar)))

;;; bar has foo for metaclass, and now we make it a descendant of foo too
(reinitialize-instance (find-class 'bar) :direct-superclasses (list (find-class 'foo)))

(format t "~S~%" (mop::class-direct-superclasses (find-class 'bar)))

(defparameter *bar-1* (make-instance 'bar))

(defclass baz (bar) ((y :initarg :y)))

(reinitialize-instance (find-class 'bar)
                       :direct-slots `((:name z :initargs (:z))))

(format t "adding z slot ~S~%" (mop::class-direct-superclasses (find-class 'bar)))
(describe (find-class 'bar))

(describe (find-class 'baz))

(describe (make-instance 'baz :y 3 :z 4))

;;; Now during shared-initialize on baz instances, we add a new slot to foo.

#+lispworks6.1
(defun  clos::lock-class-redefinition-lock-atomic (class)
  (format t "~%locking class = ~S~%" class)
  ; (describe class)
  )

(defmethod shared-initialize :before ((class foo) slot-names &rest initargs)
  (format t "~%class precedence list = ~S~%" (mop::compute-class-precedence-list class))
)

(defmethod shared-initialize :after ((instance baz) slot-names &rest initargs)
  #+(and lispworks (not lispworks6.1))
  (progn
    #+nil (format t "~%~S~%" (slot-value (class-of instance) 'mop::lock))
    (describe (slot-value (class-of instance) 'mop::lock))
    (if (and (slot-value (class-of instance) 'mop::lock)
	     (mp::lock-locked-p (slot-value (class-of instance) 'mop::lock)))
	(mp::process-sharing-unlock (slot-value (class-of instance) 'mop::lock)))
    (describe (slot-value (class-of instance) 'mop::lock)))
  (reinitialize-instance (find-class 'foo)
                         :direct-slots `((:name x :initargs (:x))
                                         (:name new :initargs (:new)))))

  #+nil
(reinitialize-instance (find-class 'foo)
                       :direct-slots `((:name x :initargs (:x))
                                       (:name new :initargs (:new))))

(describe (make-instance 'bar :z 3 ))

(describe (make-instance 'baz :z 3 :y 4 ))

(format t "~%~S~%" (mop::class-direct-superclasses (find-class 'bar)))

