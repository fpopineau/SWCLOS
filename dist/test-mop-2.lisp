
#+allegro
(progn
(shadowing-import 'aclmop::ensure-class-using-class :cl-user)
)

#+lispworks
(progn
(shadowing-import 'clos::ensure-class-using-class :cl-user)
(shadowing-import 'clos::validate-superclass :cl-user)
)

#+sbcl
(progn
(shadowing-import 'sb-pcl::ensure-class-using-class :cl-user)
(shadowing-import 'sb-pcl::validate-superclass :cl-user)
)

(defclass foo (standard-class) ((name :initargs (:name))))

(defmethod validate-superclass
      ((class foo)
       (superclass standard-class))
  t)

(defclass subfoo (standard-class) () (:metaclass foo) )

(defmethod validate-superclass
      ((class subfoo)
       (superclass standard-class))
  t)

(defclass bar () ((y :initargs (:y))))

#+nil
(trace (make-instance :inside clos::ensure-class-using-class)
       (reinitialize-instance :inside clos::ensure-class-using-class))


(defmethod make-instance ((class foo) &rest initargs)
  (format t "MAKE-INSTANCE foo primary~%")
  (call-next-method))

(defmethod make-instance ((class subfoo) &rest initargs)
  (format t "MAKE-INSTANCE subfoo primary~%")
  (call-next-method))

(defmethod reinitialize-instance ((class subfoo) &rest initargs)
  (format t "REINITIALIZE-INSTANCE subfoo primary~%")
  (call-next-method))

#+sbcl
(shadowing-import 'sb-ext::without-package-locks :cl-user)

#+allegro
(shadowing-import 'excl::without-package-locks :cl-user)

#+lispworks
(let ((lw:*handle-warn-on-redefinition* :warn)
      (*packages-for-warn-on-redefinition* nil))
  (defmacro without-package-locks (&body body)
    `(let
         ((lw:*handle-warn-on-redefinition* :warn)
          (*packages-for-warn-on-redefinition* nil))
             ,@body)))

(without-package-locks
  (let ((name (gensym)))
    (ensure-class-using-class nil name :direct-superclasses (list (find-class 'bar))
                              :metaclass (find-class 'subfoo))
    (format t "First class~%")
    (ensure-class-using-class nil name :direct-superclasses (list (find-class 'bar))
                              :metaclass (find-class 'subfoo))
    (format t "Second class~%")
    ))
