;;;;
;;;; AspectL
;;;;
;;;; Copyright (c) 2005 Pascal Costanza
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;;; sell copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.
;;;;

(in-package #:cl-user)

(defpackage #:aspectl.clos-mop
  (:documentation
   "Provides a wrapper for the definitions of the CLOS MOP that are
    used in AspectL, and exists to ease porting AspectL to several
    Common Lisp implementations. It also defines a few MOP-related
    utility functions.")
  (:nicknames #:al.clos-mop)
  (:use #:common-lisp)

  (:import-from

   #+allegro #:mop
   #+clisp #:clos
   #+cmu #:clos-mop
   #+lispworks #:clos
   #+mcl-common-mop-subset #:ccl
   #+sbcl #:sb-mop

   #-lispworks #:find-method-combination
   .
   #1=(; metaobject classes
       #:slot-definition #:direct-slot-definition #:effective-slot-definition
       #:standard-class #:funcallable-standard-class
       #-clisp #:forward-referenced-class
       #:standard-slot-definition
       #:standard-direct-slot-definition #:standard-effective-slot-definition
       #:standard-generic-function #:standard-method

       ; metaobject accessors
       #:class-direct-default-initargs #:class-direct-superclasses #:class-direct-subclasses
       #:class-direct-slots #:class-name #:class-prototype #:class-slots
       #:generic-function-name #:generic-function-methods #:generic-function-method-class
       #:method-generic-function #:method-specializers
       #:remove-direct-subclass #:remove-method
       #:slot-definition-allocation #:slot-definition-initargs
       #:slot-definition-initform #:slot-definition-initfunction #:slot-definition-name
       #:slot-definition-readers #:slot-definition-type #:slot-definition-writers

       ; class initialization and finalization
       #:compute-effective-slot-definition
       #:direct-slot-definition-class #:effective-slot-definition-class
       #:ensure-class #:finalize-inheritance
       #:validate-superclass

       ; instance structure protocol
       #:slot-value-using-class
       #:slot-boundp-using-class
       #:slot-makunbound-using-class

       ; generic function invocation protocol
       #-(or allegro clisp mcl-common-mop-subset) #:make-method-lambda))
   (:export
    #:the-class #:the-direct-slot-definition #:the-effective-slot-definition
    #:find-method-combination #:create-method
    #:initialize-class-metaobject #:reinitialize-class-metaobject
    #:defmethod* #:*generic-function*
    . #1#))

(defpackage #:aspectl.mixins
  (:documentation
   "Provides destructive mixins, i.e. functions that allow for incremental
    modification of existing classes. The functions class-add, class-remove and class-set
    enable setting class options. Likewise, slot-add, slot-remove and slot-set enable
    setting slot options. The class/slot for which these changes should take effect are
    either bound to the special variables *the-class* / *the-slot*, or they are passed
    as parameters to those functions. The special variables are bound via with-class
    and with-slot macros that collect all the requested changes and perform them at
    once as soon as the control flow exits those macros.")
  (:nicknames #:al.mixins)
  (:use #:common-lisp #:al.clos-mop )
  (:import-from #:lispworks #:rebinding #:removef #:when-let #:with-unique-names)
  (:export
   #:*the-class* #:*the-slot*
   #:class-options #:slot-options
   #:get-class-option #:finalize-class-option
   #:get-slot-option #:default-slot-option #:finalize-slot-option
   #:class-add #:class-remove #:class-set
   #:slot-add #:slot-remove #:slot-set
   #:with-class #:with-slot))


(in-package #:al.mixins)

(defun the-class (class)
  "If class is a class, return it. If it is a symbol, find the class."
  (ctypecase class
    (class class)
    (symbol (find-class class))))

(defun the-direct-slot-definition (class slot)
  "If slot is a direct-slot-definition, return it.
   If it is a symbol, find it in (the-class class)."
  (ctypecase slot
    (direct-slot-definition slot)
    (symbol (find slot (class-direct-slots (the-class class))
                  :key #'slot-definition-name))))

(defun the-effective-slot-definition (class slot)
  "If slot is an effective-slot-definition, return it.
   If it is a symbol, find it in (the-class class)."
  (ctypecase slot
    (slot-definition slot)
    (symbol (find slot (class-slots (the-class class))
                  :key #'slot-definition-name))))

#+lispworks
(defgeneric find-method-combination (generic-function name options)
  (:method ((generic-function standard-generic-function) name options)
	   "LispWorks doesn't provide find-method-combination.
            Therefore, we use an internal function instead."
	   (declare (ignore options))
	   (find-a-method-combination-type name)))

(defgeneric class-options (class)
  (:method ((class standard-class))
   '(:direct-slots :direct-superclasses
     #+lispworks :default-initargs
     #-lispworks :direct-default-initargs
     :documentation :metaclass)))

(defgeneric slot-options (class)
  (:method ((class standard-class))
   '(:name :initform :initfunction :initargs
     :readers :writers
     :documentation :allocation :type)))

(defgeneric get-slot-option (class slot option)
  (:documentation "Determine the value for some slot option.")
  (:method ((class standard-class)
            (slot standard-slot-definition)
            (option symbol))
   (ccase option
     (:name          (slot-definition-name slot))
     (:initform      (slot-definition-initform slot))
     (:initfunction  (slot-definition-initfunction slot))
     (:initargs      (slot-definition-initargs slot))
     (:readers       (slot-definition-readers slot))
     (:writers       (slot-definition-writers slot))
     (:documentation (documentation slot t) nil)
     (:allocation    (slot-definition-allocation slot))
     (:type          (slot-definition-type slot)))))

(defgeneric default-slot-option (class option &optional name)
  (:documentation "Determine a default value for some slot option.")
  (:method ((class standard-class) (option symbol) &optional name)
   (ccase option
     (:name name)
     ((:initform :initfunction :initargs
       :readers :writers :documentation) nil)
     (:allocation :instance)
     (:type t))))

(defgeneric finalize-slot-option (class option value)
  (:documentation "Process a slot option before it is passed to the MOP.")
  (:method ((class t) (option symbol) value) value))

;; helper functions

(defun make-slot-spec-alist (class)
  (loop for slot in (class-direct-slots class)
        collect (cons (slot-definition-name slot)
                      (loop for option in (slot-options class)
                            collect (cons option (get-slot-option class slot option))))))

(defun finalize-slot-spec-alist-plist (class alist)
  (loop for (nil . slot-options) in (remove-duplicates alist :key #'car :test #'eq :from-end t)
        collect (loop for (option . value) in slot-options
                      collect option
                      collect (finalize-slot-option class option value))))

(defun make-default-slot-spec (class name)
  (loop for option in (slot-options class)
        collect (cons option (default-slot-option class option name))))

(defgeneric get-class-option (class option)
  (:documentation "Determine the value for some class option.")
  (:method ((class standard-class) (option symbol))
   (ccase option
     (:name                    (class-name class))
     (:direct-slots            (make-slot-spec-alist class))
     (:direct-superclasses     (mapcar #'class-name (class-direct-superclasses class)))
     (#+lispworks
      :default-initargs
      #-lispworks
      :direct-default-initargs (class-direct-default-initargs class))
     (:documentation           (documentation class t))
     (:metaclass               (class-name (class-of class))))))

(defgeneric finalize-class-option (class option value)
  (:documentation "Process a class option before it is passed to the MOP.")
  (:method ((class t) (option symbol) value) value))

(defmethod finalize-class-option
           ((class standard-class)
            (option (eql :direct-slots))
            value)
  (finalize-slot-spec-alist-plist class value))

;; helper functions

(defun make-class-spec-alist (class)
  (loop for option in (class-options class)
        collect (cons option (get-class-option class option))))

(defun finalize-class-spec-alist-plist (class alist)
  (loop for (option . value) in alist
        collect option
        collect (finalize-class-option class option value)))

(defvar *the-class* nil
  "Holds the current class metaobject for inspection purposes.")
(defvar *class-spec*) ; the current class specification alist

(defvar *the-slot* nil
  "Holds the current slot definition metaobject for inspection purposes.")
(defvar *slot-name* nil) ; the current slot's name
(defvar *slot-spec*)     ; the current slot specification alist

(defmacro with-class (class &body body)
  "Creates an environment for modifying a class.
   Accepts symbols and class metaobjects."
  (with-unique-names (new-class)
    (rebinding (class)
      `(let* ((*the-class*  (the-class ,class))
	      (*class-spec* (make-class-spec-alist *the-class*))
	      (*the-slot* nil))
	 ,@body
         (handler-bind
             ((warning #'muffle-warning))
           (let ((,new-class (apply #'ensure-class
                                    (class-name *the-class*)
                                    (finalize-class-spec-alist-plist *the-class* *class-spec*))))
             ;; (finalize-inheritance ,new-class)
             ,new-class))))))

(defmacro with-slot (slot &body body)
  "Creates an environment for modifying a slot.
   Must be embedded inside a with-class environment,
   and accepts symbols and slot definition metaobjects."
  (rebinding (slot)
    `(progn
       (assert (not (null *the-class*)))
       (assert (not (null *class-spec*)))
       (let* ((*the-slot* (the-direct-slot-definition *the-class* ,slot))
              (*slot-name* (if (symbolp ,slot) ,slot (slot-definition-name *the-slot*)))
              (*slot-spec* (or (cdr (assoc *slot-name* (cdr (assoc :direct-slots *class-spec*))))
                               (make-default-slot-spec *the-class* *slot-name*))))
         ,@body
         (push (cons *slot-name* *slot-spec*)
               (cdr (assoc :direct-slots *class-spec*)))))))

(defgeneric class-add (option value &rest args)
  (:method (option value &rest args)
	   (declare (dynamic-extent args))
	   (apply #'class-add-using-class *the-class* option value args)))

(defgeneric class-add-using-class (class option value &key &allow-other-keys)
  (:documentation "Pushes information to the value of a class option.")
  (:method ((class standard-class)
            (option symbol) value
            &key (test #'eql) (key #'identity) &allow-other-keys)
   (push (cons option (adjoin value (cdr (assoc option *class-spec*)) :test test :key key))
         *class-spec*)))

(defmethod class-add-using-class
           ((class standard-class)
            (option (eql :direct-slots))
            (value symbol)
            &key &allow-other-keys)
  "Ensures that a slot of this name exists.
   If it doesn't exist, it is created with default slot option values."
  (with-slot value))

(defmethod class-add-using-class
           ((class standard-class)
            (option (eql :direct-slots))
            (value list)
            &key &allow-other-keys)
  "Modifies or creates a slot with additional/changed slot options.
   Slot options are defined as, for example, in DEFCLASS.
   Caution: :initform and :initfunction are not synced!"
  (with-slot (pop value)
    (loop for (key val) on value by #'cddr
          do (case key
               (:initarg  (slot-add :initargs val))
               (:reader   (slot-add :readers val))
               (:writer   (slot-add :writers val))
               (:accessor (slot-add :accessors val))
               (otherwise (slot-set key val))))))

(defgeneric class-remove (option value &rest args)
  (:method (option value &rest args)
	   (declare (dynamic-extent args))
	   (apply #'class-remove-using-class *the-class* option value args)))

(defgeneric class-remove-using-class (class option value &key &allow-other-keys)
  (:documentation "Removes information from the value of a class option.")
  (:method ((class standard-class)
            (option symbol)
            value
            &key (test #'eql) (key #'identity) &allow-other-keys)
   (when-let (cons (assoc option *class-spec*))
     (removef (cdr cons) value :test test :key key))))

(defmethod class-remove-using-class
           ((class standard-class)
            (option (eql :direct-slots))
            (value symbol)
            &key &allow-other-keys)
  "Removes a slot from a class."
  (when-let (cons (assoc :direct-slots *class-spec*))
    (removef (cdr cons) value :key #'car)))

(defmethod class-remove-using-class
           ((class standard-class)
            (option (eql :direct-slots))
            (value list)
            &key &allow-other-keys)
  "Modifies or creates (!) a slot with slot options to be removed.
   Slot options are defined as, for example, in DEFCLASS.
   Caution: :initform and :initfunction are not synced!"
  (with-slot (pop value)
    (loop for (key val) on value by #'cddr
          do (case key
               (:initarg   (slot-remove :initargs val))
               (:reader    (slot-remove :readers val))
               (:writer    (slot-remove :writers val))
               (:accessors (slot-remove :accessors val))
               (otherwise  (slot-set key (default-slot-option class key val)))))))

(defgeneric class-set (option value &rest args)
  (:method (option value &rest args)
	   (declare (dynamic-extent args))
	   (apply #'class-set-using-class *the-class* option value args)))

(defgeneric class-set-using-class (class option value &key &allow-other-keys)
  (:documentation "Sets the value of a class option.")
  (:method ((class standard-class)
            (option symbol)
            value
            &key &allow-other-keys)
   (push (cons option value) *class-spec*)))

(defgeneric slot-add (option value &rest args)
  (:method (option value &rest args)
	   (declare (dynamic-extent args))
	   (apply #'slot-add-using-class *the-class* option value args)))

(defgeneric slot-add-using-class (class option value &key &allow-other-keys)
  (:documentation "Pushes information to the value of a slot option.")
  (:method ((class standard-class)
            (option symbol)
            value
            &key (test #'eql) (key #'identity) &allow-other-keys)
   (push (cons option (adjoin value (cdr (assoc option *slot-spec*)) :test test :key key))
         *slot-spec*)))

(defmethod slot-add-using-class
           ((class standard-class)
            (option (eql :accessors))
            value
            &key &allow-other-keys)
  "Adds an accessor to a slot."
  (slot-add :readers value)
  (slot-add :writers `(setf ,value) :test #'equal))

(defgeneric slot-remove (option value &rest args)
  (:method (option value &rest args)
	   (declare (dynamic-extent args))
	   (apply #'slot-remove-using-class *the-class* option value args)))

(defgeneric slot-remove-using-class (class option value &key &allow-other-keys)
  (:documentation "Removes information from the value of a slot option.")
  (:method ((class standard-class)
            (option symbol)
            value
            &key (test #'eql) (key #'identity) &allow-other-keys)
   (when-let (cons (assoc option *slot-spec*))
     (removef (cdr cons) value :test test :key key))))

(defmethod slot-remove-using-class
           ((class standard-class)
            (option (eql :accessors))
            value
            &key &allow-other-keys)
  "Removes an accessor from a slot."
  (slot-remove :readers value)
  (slot-remove :writers `(setf ,value) :test #'equal))

(defgeneric slot-set (option value &rest args)
  (:method (option value &rest args)
	   (declare (dynamic-extent args))
	   (apply #'slot-set-using-class *the-class* option value args)))

(defgeneric slot-set-using-class (class option value &key &allow-other-keys)
  (:documentation "Sets the value of a slot option.")
  (:method ((class standard-class)
            (option symbol)
            value
            &key &allow-other-keys)
   (push (cons option value) *slot-spec*)))

(defmethod slot-set-using-class
           ((class standard-class)
            (option (eql :accessors))
            value
            &key &allow-other-keys)
  "Sets the accessors of a slot."
  (slot-set :readers value)
  (slot-set :writers (mapcar (lambda (val) `(setf ,val)) value)))

