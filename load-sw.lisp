#+nil
(cd "c:/source/lisp/src/SWCLOS2-2011-test2")

(ql:quickload :puri)
#+lispworks
(setf *handle-existing-defpackage* '(:warn :add))

#+sbcl
(setf *default-pathname-defaults*  (make-pathname :device "c" :directory "source\\Lisp\\src\\SWCLOS2-2011-test" ))

; (defmethod source-file-type ((c cl-source-file) (s module)) "cl")

#+nil
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

#+nil
(defun package-add-nicknames (package-name &rest new-nicknames)
  (let ((old-nicknames (package-nicknames package-name)))
    (system::without-package-locks
      (rename-package package-name package-name
                      (remove-duplicates (append old-nicknames new-nicknames)
                                         :test #'equal)))))
#+nil
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
(progn
  (cl-user::package-add-nicknames :sb-pcl :mop)
  (cl-user::package-add-nicknames :sb-impl :excl)
  (cl-user::package-add-nicknames :sb-impl :system))
#+sbcl
(sb-ext::without-package-locks
    (defun excl::false (&rest args) (declare (ignore args)) nil))

#+nil
(progn
  (package-add-nicknames :clos :mop)
  (package-add-nicknames :system :excl)
  (package-add-nicknames :capi :cg)
  )

#+nil
(progn
  (package-add-nicknames :common-lisp-user :cl-user)
  (package-add-nicknames :common-lisp-user :CL-USER))

#+nil
(in-package :excl)

#+nil (export 'excl::without-redefinition-warnings 'excl)
#+nil (export 'excl::without-package-locks 'excl)

#+nil
(without-package-locks
  (defmacro without-redefinition-warnings (&rest form)
    `(let ((cl::*redefinition-action* :quiet))
       ,@form))
)

#+nil
(in-package :cl-user)

#+nil
(defun read-token (stream firstchar)
  (let* ((oldcase (readtable-case *readtable*))
         symbol)
    (setf (readtable-case *readtable*) :preserve)
    (setf symbol (read stream))
    (setf (readtable-case *readtable*) oldcase)
    (intern (coerce (cons firstchar (coerce (symbol-name symbol) 'list)) 'string))))

#+nil
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

;; (load "metaclass-init.lisp")

; (asdf :puri)
; (asdf :closer-mop)

;(ql:quickload :puri)
;(ql:quickload :closer-mop)
;(ql:quickload :named-readtables)

;;;
;;; Logging
;;;
;; (ql:quickload :cl-log)

;; (in-package cl-log)

;; (setf (log-manager)
;;       (make-instance 'log-manager :message-class 'formatted-message))

;; (defcategory :stage1)
;; (defcategory :stage2)
;; (defcategory :stage3)
;; (defcategory :stage4)
;; (defcategory :stage5)
;; (defcategory :stage6)
;; (defcategory :stage7)
;; (defcategory :stage8)
;; (defcategory :stage9)
;; (defcategory :stage10)
;; (defcategory :stage11)
;; (defcategory :stage12)
;; (defcategory :stage13)
;; (defcategory :stage14)
;; (defcategory :stage15)
;; (defcategory :stage16)
;; (defcategory :stage17)
;; (defcategory :stage18)
;; (defcategory :stage19)

;; (defcategory :other)

;; (start-messenger 'text-stream-messenger
;; 		 :stream *standard-output*)

;; (in-package :cl-user)

;; (load "closette.lisp")

(defpackage gx-system (:use :common-lisp :asdf))

(defparameter gx-system::*debug-print* t)

;; (in-package :gx-system)

;; ;;(setf *error-output* (open "swclos2-debug.log" :direction :output :if-exists :supersede))

;; #+lispworks
;; (defparameter gx-system::*optimize-slot-access* nil)

;; #+lispworks
;; (progn
;;   (require "describe")
;;   (setf dbg::*debug-print-length* 1024
;; 	dbg::*debug-print-level* 16
;; 	*describe-length* 64
;; 	*describe-print-length* 1024
;; ;	*describe-level* 2
;; 	*print-length* 1024
;; 	*print-level* 16))

;; (in-package :cl-user)

#+nil
(cl:defpackage :gx
  ; (:use :common-lisp)
  (:use "COM.RAVENBROOK.COMMON-LISP-LOG")
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

(progn
  (load "packages.lisp")
  (load "rdf/compat.lisp")
  (load "rdf/utils.lisp")
  (load "rdf/rdf-io.lisp")
  (load "rdf/iri.lisp")
  (load "rdf/xml.lisp")
  (load "rdf/rdf-error.lisp")
  (load "rdf/name-space.lisp")
  (load "rdf/literal.lisp")
  (load "rdf/rdf-share.lisp")
  (load "rdf/rdf-parser.lisp")
  (load "rdf/rdf-reader.lisp")
  (load "rdf/node.lisp"))

(progn
  (load "rdfs/slot-def.lisp")
  (load "rdfs/rdf-boot.lisp")
  (load "rdfs/domain-range.lisp")
  (load "rdfs/rdfs-kernel.lisp")
  (load "rdfs/gx-type.lisp")
  (load "rdfs/rdfs-objects.lisp")
  (load "rdfs/gx-forward-ref.lisp")
  (load "rdfs/rdf-core.lisp")
  (load "rdfs/gx-utils.lisp")
  (load "rdfs/rdf-writer.lisp")
  )

#+lispworks
(progn
  (load "owl/owl-error.lisp")
  (load "owl/owl-kernel.lisp")
  (load "owl/owl-same-different.lisp")
  (load "owl/owl-equivalent-disjoint.lisp")
  (load "owl/nnf.lisp")
  (load "owl/tunify.lisp")
  (load "owl/subsume.lisp")
  (load "owl/owl.lisp"))


; #+(or allegro lispworks)
#+nil
(trace gx::%typep
       gx::collect-ranges
       gx::type-option-check-with-cardinality
       gx::satisfy-filler
       gx::shared-initialize-before-in-RDF
       gx::collect-domains
       gx::%addForm
       gx::addObject
       gx::addForm
       gx::addClass
       gx::addInstance
       gx::addRdfXml)

#+nil
(progn
  (load "NTriple/NTriple.lisp")
  (load "NTriple/NTparser.lisp")
  (load "NTriple/ntwriter.lisp"))

; (in-package :gx-user)

#+nil
(let ((*error-output* cl:nil))
   (setq *autoepistemic-local-closed-world* cl:nil)
   (read-rdf-file #'addRdfXml "WineFood/Wine.rdf")
   (read-rdf-file #'addRdfXml "WineFood/Food.rdf")
   (setq *autoepistemic-local-closed-world* t)
   )
