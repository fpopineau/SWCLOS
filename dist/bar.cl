(defclass bar (standard-class) () )

(defmethod validate-super-class ((class bar) (super standard-class)) t)
(defmethod validate-super-class ((class standard-class) (super bar)) t)

(defclass foo ()
	  ((x :initarg :x))
;	  (:metaclass bar)
)

(defclass baz (foo) () )

(defparameter foo (find-class 'foo))

(cl:subtypep (symbol-value 'foo) (find-class 'baz))

(cl:subtypep (find-class 'baz) (symbol-value 'foo))

;(reinitialize-instance (find-class 'foo)
;		       :direct-slots '((:name 'y :initargs '(:y))))

