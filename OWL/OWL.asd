;;;-*- Mode: common-lisp; syntax: common-lisp; package: asdf; base: 10 -*-
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
;;; This file must be located at OWL directory that includes leanOWL and OneOf files.
;;; This file must be used without compiling.

(defpackage gx-system (:use :common-lisp :asdf))  
 
(in-package :gx-system)

(eval-when (:load-toplevel :execute)
  (defparameter *owl-directory*
    (make-pathname :host (pathname-host *load-truename*)
                   :device (pathname-device *load-truename*)
                   :directory (pathname-directory *load-truename*)))
  (setf (logical-pathname-translations "OWL")
    `(("**;*.*"
       ,(make-pathname
         :host (pathname-host *owl-directory*)
         :device (pathname-device *owl-directory*)
         :directory (append (pathname-directory *owl-directory*)
                            (list :wild-inferiors))
         :name :wild
         :type :wild
         ))))
) ; end of eval-when

(eval-when (:load-toplevel :execute)
  (unless (asdf:find-system "rdfs" nil)
    (defparameter *rdfs-directory*
      (merge-pathnames
       (make-pathname
        :directory (substitute "RDFS" "OWL"
                               (pathname-directory *owl-directory*)
                               :test #'string=))
       *owl-directory*))
    (setf (logical-pathname-translations "RDFS")
      `(("**;*.*"
         ,(make-pathname
           :host (pathname-host *rdfs-directory*)
           :device (pathname-device *rdfs-directory*)
           :directory (append (pathname-directory *rdfs-directory*)
                              (list :wild-inferiors))
           :name :wild
           :type :wild
           ))))
    (load "RDFS:RDFS.asd"))
)

(defsystem :owl
    :name "SWCLOS OWL system"
  :author "Seiji Koide <SeijiKoide@aol.com>"
  :maintainer "Seiji Koide <SeijiKoide@aol.com>"
  :version "2.0.0"
  :licence "SWCLOS"
  :description "OWL subsystem of SWCLOS, which is an OWL Full processor on top of CLOS."
  :long-description "This code is written at Galaxy Express Corporation, Japan, for the realization of the MEXT IT Program in Japan, and is updated by Seiji Koide."
  :depends-on ("rdfs")
  :in-order-to ((compile-op (load-op "rdfs"))  
                (load-op (load-op "rdfs")))
  :pathname #p"OWL:" ; (translate-logical-pathname "OWL:")
  :serial t
  :components
  ((:file "owlerror"  :depends-on ())
   (:file "owlkernel" :depends-on ())
   (:file "owlsamedifferent" :depends-on ())
   (:file "owlequivalentdisjoint" :depends-on ())
   (:file "NNF"     :depends-on ())
   (:file "tunify"  :depends-on ())
   (:file "subsume" :depends-on ("NNF" "tunify" "owlkernel" "owlequivalentdisjoint" "owlsamedifferent"))
   (:file "OWL"     :depends-on ("subsume"))
   ))

(in-package #:cl-user)
(format t "~%;;To compile, execute these forms:~%~s~%"
  '(asdf:operate 'asdf:compile-op :owl))

(format t "~%;;To load, execute these forms:~%~s~%"
  '(asdf:operate 'asdf:load-op :owl))
