;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: asdf; Base: 10; Lowercase: Yes -*-
;;;
;;;; SWCLOS: A Semantic Web Processor on CLOS
;;;
;;; IT Program Project in Japan: 
;;:    Building Operation-Support System for Large-scale System using IT.
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan.
;;;
;;; Copyright © 2003, 2004, 2006 by Galaxy Express Corporation
;;; 
;;; Copyright (c) 2007, 2008, 2009 Seiji Koide

;;; ASDF system definition.
;;;
;;; This file must be located at RDF directory that includes many RDF related files.
;;; This file must be used without compiling.

(defpackage gx-system (:use :common-lisp :asdf))  
 
(in-package :gx-system)  

(eval-when (:load-toplevel :execute :compile)
  (defparameter *debug-print* nil))

(eval-when (:load-toplevel :execute)
  (defparameter *rdf-directory*
    (make-pathname :host (pathname-host *load-truename*)
                   :device (pathname-device *load-truename*)
                   :directory (pathname-directory *load-truename*)))
  (setf (logical-pathname-translations "RDF")
    `(("**;*.*"
       ,(make-pathname
         :host (pathname-host *rdf-directory*)
         :device (pathname-device *rdf-directory*)
         :directory (append (pathname-directory *rdf-directory*)
                            (list :wild-inferiors))
         :name :wild
         :type :wild
         ))))
) ; end of eval-when

(defsystem :rdf
    :name "SWCLOS RDF subsystem"
  :author "Seiji Koide <SeijiKoide@aol.com>"
  :maintainer "Seiji Koide <SeijiKoide@aol.com>"
  :version "0.9.0"
  :licence "SWCLOS"
  :description "RDF subsystem of SWCLOS (an OWL Full processor on top of CLOS)."
  :long-description "This code is written at Galaxy Express Corporation, Japan, for the realization of the MEXT IT Program in Japan."
  :depends-on ()
  :pathname #p"RDF:" ; (translate-logical-pathname "RDF:")
  :components
  ((:file "compat")
   (:file "Utils"        :depends-on ("compat"))
   (:file "RdfIO"        :depends-on ("compat"))
   (:file "IRI"          :depends-on ("compat"))
   (:file "packages"     :depends-on ("compat"))
   (:file "Xml"          :depends-on ("packages"))
   (:file "rdferror"     :depends-on ("Utils" "packages"))
   (:file "NameSpace"    :depends-on ("packages" "IRI"))
   (:file "Literal"      :depends-on ("packages" "NameSpace"))
   (:file "RDFShare"     :depends-on ("packages" "RdfIO" "NameSpace"))
   (:file "Rdf"          :depends-on ("packages" "NameSpace" "RDFShare"))
   (:file "RdfReader"    :depends-on ("packages" "Rdf"))
   (:file "node"         :depends-on ("compat")))
)

(in-package #:cl-user)

(format t "~%;;To compile, execute these forms:~%~s~%"
  '(asdf:operate 'asdf:compile-op :rdf))

(format t "~%;;To load, execute these forms:~%~s~%"
  '(asdf:operate 'asdf:load-op :rdf))
