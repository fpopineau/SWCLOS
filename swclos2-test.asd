;;; -*- Mode: lisp -*-
;;;

(in-package :cl-user)

(defpackage :gx-test-system
  (:use :cl :asdf))

(in-package :gx-test-system)

(defsystem :gx-test
  :name "Simple Common Lisp unit tests library."
  :author "dan lentz"
  :depends-on (:gx)
  :serial t
  :components
  
  ((:module TEST :serial t :components
    ((:file "package")
      (:file "lisp-unit")
      (:file "rational")
      (:file "floating-point")
      (:file "lisp-unit-tests")
      (:file "extend-match")
      (:file "tables")
      (:file "write-wrap")))

  (:module RDF :serial t :components
    ((:file "rdf-test")))

  (:module RDFS :serial t :components
    ((:file "rdfs-test")))

  (:module OWL  :serial t :components
    ((:file "owl-test"))))
  
  )

