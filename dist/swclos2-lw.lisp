#+lispworks
(progn
  (setf (environment-variable "HOMEDRIVE") "C:")
  (setf (environment-variable "HOMEPATH") "/Source/Lisp"))

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init "c:\\source\\lisp\\lib\\quicklisp\\setup.lisp"))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

; (asdf :puri)
; (asdf :closer-mop)

(ql:quickload :puri)
;(ql:quickload :closer-mop)
; (ql:quickload :named-readtables)

#+nil
(cd "c:/source/lisp/src/SWCLOS2-2011-test")

#+lispworks
(setf *handle-existing-defpackage* '(:warn :add))

#+sbcl
(setf *default-pathname-defaults*  (make-pathname :device "c" :directory "source\\Lisp\\src\\SWCLOS2-2011-test" ))

; (defmethod source-file-type ((c cl-source-file) (s module)) "cl")

#+lispworks
(let ((lw:*handle-warn-on-redefinition* :warn)
      (*packages-for-warn-on-redefinition* nil))
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


(defun package-add-nicknames (package-name &rest new-nicknames)
  (let ((old-nicknames (package-nicknames package-name)))
    (system::without-package-locks
      (rename-package package-name package-name
                      (remove-duplicates (append old-nicknames new-nicknames)
                                         :test #'equal)))))

(defun package-remove-nicknames (package-name &rest new-nicknames)
  (let ((old-nicknames (package-nicknames package-name)))
    (system::without-package-locks
      (rename-package package-name package-name
                      (set-difference old-nicknames
                                      (mapcar #'(lambda (x)
                                                  (or (stringp x)
                                                      (and (symbolp x) (symbol-name x)))) new-nicknames)
                                      :test #'equal)))))


;; (setf (readtable-case *readtable*) :invert)

#+sbcl
(cl-user::package-add-nicknames :sb-pcl :mop)

#+sbcl
(cl-user::package-add-nicknames :sb-impl :excl)

#+sbcl
(cl-user::package-add-nicknames :sb-impl :system)

#+sbcl
(sb-ext::without-package-locks
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

#+lispworks
(defun read-token (stream firstchar)
  (let* ((oldcase (readtable-case *readtable*))
         symbol)
    (setf (readtable-case *readtable*) :preserve)
    (setf symbol (read stream))
    (setf (readtable-case *readtable*) oldcase)
    (intern (coerce (cons firstchar (coerce (symbol-name symbol) 'list)) 'string))))

#+lispworks
(progn
  (shadowing-import 'mop::validate-metaclass-change :system)
  (shadowing-import 'mop::name :system)
  (shadowing-import 'mop::name :capi)
  (shadowing-import 'mop::allocation :system)
  (shadowing-import 'mop::initargs :system)
  (shadowing-import 'mop::initform :system)
  (shadowing-import 'mop::initfunction :system)
  (shadowing-import 'mop::readers :system)
  (shadowing-import 'mop::writers :system)
  (shadowing-import 'mop::*the-class-t* :system)
  (shadowing-import 'mop::standard-instance-p :system)
  (shadowing-import 'excl::name :cl-user)
  (shadowing-import 'mop::direct-subclasses :system)
  (shadowing-import 'mop::direct-superclasses :system)
  (shadowing-import 'mop::direct-slots :system)
  (export 'excl::name)
)

#+sbcl
(without-package-locks
;  (shadowing-import 'mop::name :system)
;  (shadowing-import 'mop::name :capi)
  (shadowing-import 'mop::allocation :system)
  (shadowing-import 'mop::initargs :system)
  (shadowing-import 'mop::initform :system)
  (shadowing-import 'mop::initfunction :system)
  (shadowing-import 'mop::readers :system)
  (shadowing-import 'mop::writers :system)
  (shadowing-import 'mop::*the-class-t* :system)
  (shadowing-import 'mop::standard-instance-p :system)
;  (shadowing-import 'excl::name :cl-user)
  (shadowing-import 'mop::direct-subclasses :system)
  (shadowing-import 'mop::direct-superclasses :system)
  (shadowing-import 'mop::direct-slots :system)
  (export 'excl::name)
)

#+lispworks
(package-add-nicknames :capi :cg)

;; (load "metaclass-init.lisp")

(load "closette.lisp")

(defpackage gx-system (:use :common-lisp :asdf))

(in-package :gx-system)

#+lispworks
(defparameter gx-system::*debug-print* nil)

;;(setf *error-output* (open "swclos2-debug.log" :direction :output :if-exists :supersede))

#+lispworks
(defparameter gx-system::*optimize-slot-access* nil)

#+lispworks
(setf clos::*CHECK-MAKE-INSTANCE-INITARGS* nil)

#+lispworks
(setf dbg::*debug-print-length* 1024
      dbg::*debug-print-level* 16
      *print-length* 1024
      *print-level* 16)

(in-package :cl-user)

#+(or lispworks sbcl)
(cl:defpackage :gx
  ; (:use :common-lisp)
  (:shadowing-import-from #+lispworks clos #+sbcl sb-pcl 
			  #:name
                          #:default-initargs
                          #:direct-default-initargs
                          #:direct-slots
                          #:direct-subclasses
                          #:direct-superclasses
                          #:precedence-list
                          #:prototype
                          #:direct-methods
                          #:wrapper
                          #:lock
                          #:documentation-slot
                          #:plist
                          #:potential-initargs
                          #:make-instance-flags
                          #:other-lock
                          #:dependents
                          ))

#+(or allegro lispworks)
(setf gx-system::*debug-print* nil)

;; (with-package-iterator (next-symbol (list-all-packages)
;;                                     :internal :external :inherited)
;;   (loop
;;      (multiple-value-bind (more? symbol accessibility package) (next-symbol)
;;        (if more?
;;            (if (string-not-equal (symbol-name symbol) (string-downcase (symbol-name symbol)))
;;            (format t "~A~%" symbol))
;;            (return)))))
;; (with-package-iterator (next-symbol (list-all-packages)
;;                                     :external)
;;   (loop
;;      (multiple-value-bind (more? symbol accessibility package) (next-symbol)
;;        (if more?
;;            (if (string/= (symbol-name symbol) (string-downcase (symbol-name symbol)))
;;                (let ((new-symbol (intern (string-downcase (symbol-name symbol)) package)))
;;                  (ignore-errors
;;                    (when (boundp symbol)
;;                      (setf (symbol-value new-symbol)
;;                            (symbol-value symbol)))
;;                    (when (fboundp symbol)
;;                      (setf (symbol-function new-symbol)
;;                            (symbol-function symbol)))
;;                    (setf   (symbol-plist new-symbol)
;;                            (symbol-plist symbol))
;;                    (export new-symbol package)
;;                    (format t "~A~%" new-symbol))))
;;            (return)))))

(setf (readtable-case *readtable*) :invert)
;; (setf (readtable-case *readtable*) :preserve)

; (in-readtable :modern)

(load "RDF/Utils.cl")
(load "RDF/RdfIO.cl")
(load "RDF/IRI.cl")
(load "RDF/packages.cl")
(load "RDF/Xml.cl")
(load "RDF/rdferror.cl")
(load "RDF/NameSpace.cl")
(load "RDF/Literal.cl")
(load "RDF/RDFShare.cl")
(load "RDF/Rdf.cl")
(load "RDF/RdfReader.cl")
(load "RDF/node.cl")

(load "RDFS/SlotDef.cl")
(load "RDFS/RDFboot.cl")
(load "RDFS/GxType.cl")
(load "RDFS/DomainRange.cl")
(load "RDFS/RdfsObjects.cl")
(load "RDFS/RdfsKernel.cl")
(load "RDFS/GxForwardRef.cl")
(load "RDFS/RdfsCore.cl")
(load "RDFS/gxutils.cl")
(load "RDFS/rdfwriter.cl")

(load "OWL/owlerror.cl")
(load "OWL/owlkernel.cl")
(load "OWL/owlsamedifferent.cl")
(load "OWL/owlequivalentdisjoint.cl")
(load "OWL/NNF.cl")
(load "OWL/tunify.cl")
(load "OWL/subsume.cl")
(load "OWL/OWL.cl")

#+nil (load "NTriple/NTriple.cl")
#+nil (load "NTriple/NTparser.cl")
#+nil (load "NTriple/ntwriter.cl")

;; (setf (readtable-case *readtable*) :upcase)

#+(or allegro lispworks)
(setf gx-system::*debug-print* nil)

#+nil (trace addRdfXml (addInstance :step nil) (addClass :step nil) reinitialize-instance addForm addObject %addForm
             read-Attribute-in-RDF read-property shared-initialize-before-in-RDF collect-ranges collect-domains
             read-QName #+nil mop::change-class #+nil mop::shared-initialize
             satisfy-filler range-satisfy type-option-check-with-cardinality
             #+nil (make-instance :inside mop::ensure-class-using-class)
             #+nil (mop::ensure-class :inside addClass)
             #+nil(mop::ensure-class-using-class :inside addClass))

#+allegro (trace addRdfXml addInstance addClass reinitialize-instance addForm addObject %addForm  read-Attribute-in-RDF read-property shared-initialize-before-in-RDF collect-ranges collect-domains read-QName satisfy-filler type-option-check-with-cardinality #+nil range-satisfy make-this-supers #+nil mop::change-class #+nil mop::shared-initialize)

#+nil
(in-package :gx-user)

#+nil
(let ((*error-output* cl:nil))
   (setq *autoepistemic-local-closed-world* cl:nil)
   (read-rdf-file #'addRdfXml "../SWCLOS2-test1/LUBM/univ-bench.owl")
;   (read-rdf-file #'addRdfXml "../SWCLOS2-test1/WineFood/Wine.rdf")
;   (read-rdf-file #'addRdfXml "../SWCLOS2-test1/WineFood/Food.rdf")
   (setq *autoepistemic-local-closed-world* t)
   )

(cd "LUBM")

(in-package :gx-user)

;; (trace gx::%compute-effective-slot-definition-initargs)
;; (trace (method mop::compute-effective-slot-definition-initargs (|rdfs|:|Class| t t)))
;; (trace (method mop::effective-slot-definition-class (|rdfs|:|Class|)))
;; (trace (method mop::direct-slot-definition-class (|rdfs|:|Class|)))
;; (trace gx::addForm gx::addInstance)

;; (trace (method mop::shared-initialize :after Property-effective-slot-definition)

(defun lubm ()
  (lw:set-default-character-element-type 'lw:simple-char)
  (load "load-lubm.cl")
  (load "queries.cl")
  (time
   (let ((*error-output* cl:nil)
	 (*autoepistemic-local-closed-world* cl:nil))
     (load-lubm10ver17)
   ))
  (query))
  
