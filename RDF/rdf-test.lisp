;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; rdf-test.lisp
;;;;;
;;;;; Maintainer:  <danlentz@gmail.com>
;;;;;

(in-package :gx-user)

(define-test test/xsd/simple-types
  (assert-true (cl:typep 1  'xsd:positiveInteger))
  (assert-true (cl:typep -1 'xsd:negativeInteger))
  (assert-true (cl:typep 0 'xsd:nonNegativeInteger))
  (assert-true (cl:typep 0 'xsd:nonPositiveInteger))
  (assert-true (cl:typep 32767 'xsd:short))
  (assert-true (cl:typep 32768 'xsd:int))
  (assert-true (cl:typep 2147483647 'xsd:int))
  (assert-true (cl:typep 2147483648 'xsd:long))
  (assert-true (cl:typep 9223372036854775807 'xsd:long))
  (assert-true (cl:typep 9223372036854775808 'xsd:integer))
  (assert-true (cl:typep 1 'xsd:decimal))
  (assert-true (cl:typep 1.0e0 'xsd:float))
  (assert-true (cl:typep 1.0d0 'xsd:double))
  (assert-true (cl:typep (rational 1) 'xsd:decimal))
  (assert-true (cl:typep (rational 0.000001) 'xsd:decimal))
  (assert-true (cl:typep 0.000001 'xsd:float))
  (assert-true (cl:typep "string?" 'xsd:string))
  (assert-true (cl:typep (uri "http://somewhere") 'xsd:anyURI))
  (assert-true (cl:typep 'xsd:false 'xsd:boolean)))

(define-test test/xsd/class
  (assert-true (cl:subtypep 'xsd:unsignedByte 'xsd:unsignedShort))
  (assert-true (cl:subtypep 'xsd:unsignedByte 'xsd:unsignedInt))
  (assert-true (cl:subtypep 'xsd:unsignedByte 'xsd:unsignedLong))
  (assert-true (cl:subtypep 'xsd:unsignedByte 'xsd:nonNegativeInteger))
  (assert-true (cl:subtypep 'xsd:unsignedByte 'xsd:integer))
  (assert-true (cl:subtypep 'xsd:unsignedByte 'xsd:decimal))
  (assert-true (cl:subtypep 'xsd:unsignedByte 'xsd:anySimpleType))
  (assert-true (cl:subtypep 'xsd:unsignedShort 'xsd:unsignedInt))
  (assert-true (cl:subtypep 'xsd:unsignedShort 'xsd:unsignedLong))
  (assert-true (cl:subtypep 'xsd:unsignedShort 'xsd:nonNegativeInteger))
  (assert-true (cl:subtypep 'xsd:unsignedShort 'xsd:integer))
  (assert-true (cl:subtypep 'xsd:unsignedShort 'xsd:decimal))
  (assert-true (cl:subtypep 'xsd:unsignedShort 'xsd:anySimpleType))
  (assert-true (cl:subtypep 'xsd:unsignedInt 'xsd:unsignedLong))
  (assert-true (cl:subtypep 'xsd:unsignedInt 'xsd:nonNegativeInteger))
  (assert-true (cl:subtypep 'xsd:unsignedInt 'xsd:integer))
  (assert-true (cl:subtypep 'xsd:unsignedInt 'xsd:decimal))
  (assert-true (cl:subtypep 'xsd:unsignedInt 'xsd:anySimpleType))
  (assert-true (cl:subtypep 'xsd:unsignedLong 'xsd:nonNegativeInteger))
  (assert-true (cl:subtypep 'xsd:unsignedLong 'xsd:integer))
  (assert-true (cl:subtypep 'xsd:unsignedLong 'xsd:decimal))
  (assert-true (cl:subtypep 'xsd:unsignedLong 'xsd:anySimpleType))
  (assert-true (cl:subtypep 'xsd:nonNegativeInteger 'xsd:integer))
  (assert-true (cl:subtypep 'xsd:nonNegativeInteger 'xsd:decimal))
  (assert-true (cl:subtypep 'xsd:nonNegativeInteger 'xsd:anySimpleType))
  (assert-true (cl:subtypep 'xsd:integer 'xsd:decimal))
  (assert-true (cl:subtypep 'xsd:integer 'xsd:anySimpleType))
  (assert-true (cl:subtypep 'xsd:decimal 'xsd:anySimpleType))
  (assert-true (cl:subtypep 'xsd:byte 'xsd:short))
  (assert-true (cl:subtypep 'xsd:byte 'xsd:int))
  (assert-true (cl:subtypep 'xsd:byte 'xsd:long))
  (assert-true (cl:subtypep 'xsd:byte 'xsd:integer))
  (assert-true (cl:subtypep 'xsd:byte 'xsd:decimal))
  (assert-true (cl:subtypep 'xsd:byte 'xsd:anySimpleType))
  (assert-true (cl:subtypep 'xsd:short 'xsd:int))
  (assert-true (cl:subtypep 'xsd:short 'xsd:long))
  (assert-true (cl:subtypep 'xsd:short 'xsd:integer))
  (assert-true (cl:subtypep 'xsd:short 'xsd:decimal))
  (assert-true (cl:subtypep 'xsd:short 'xsd:anySimpleType))
  (assert-true (cl:subtypep 'xsd:int 'xsd:long))
  (assert-true (cl:subtypep 'xsd:int 'xsd:integer))
  (assert-true (cl:subtypep 'xsd:int 'xsd:decimal))
  (assert-true (cl:subtypep 'xsd:int 'xsd:anySimpleType))
  (assert-true (cl:subtypep 'xsd:long 'xsd:integer))
  (assert-true (cl:subtypep 'xsd:long 'xsd:decimal))
  (assert-true (cl:subtypep 'xsd:long 'xsd:anySimpleType))
  (assert-true (cl:subtypep 'xsd:positiveInteger 'xsd:nonNegativeInteger))
  (assert-true (cl:subtypep 'xsd:positiveInteger 'xsd:integer))
  (assert-true (cl:subtypep 'xsd:positiveInteger 'xsd:decimal))
  (assert-true (cl:subtypep 'xsd:positiveInteger 'xsd:anySimpleType))
  (assert-true (cl:subtypep 'xsd:negativeInteger 'xsd:nonPositiveInteger))
  (assert-true (cl:subtypep 'xsd:negativeInteger 'xsd:integer))
  (assert-true (cl:subtypep 'xsd:negativeInteger 'xsd:decimal))
  (assert-true (cl:subtypep 'xsd:negativeInteger 'xsd:anySimpleType))
  (assert-true (cl:subtypep 'xsd:nonPositiveInteger 'xsd:integer))
  (assert-true (cl:subtypep 'xsd:nonPositiveInteger 'xsd:decimal))
  (assert-true (cl:subtypep 'xsd:nonPositiveInteger 'xsd:anySimpleType))
  (assert-true (cl:subtypep 'xsd:boolean 'xsd:anySimpleType))
  (assert-true (cl:subtypep 'xsd:anyURI 'xsd:anySimpleType))
  (assert-true (cl:subtypep 'xsd:string 'xsd:anySimpleType))
  (assert-true (cl:subtypep 'xsd:float 'xsd:anySimpleType))
  (assert-true (cl:subtypep 'xsd:double 'xsd:anySimpleType)))

(define-test test/namespace/uri-mapping
  (assert-true (get-uri-namedspace "http://www.w3.org/2000/01/rdf-schema"))
  (assert-true (get-uri-namedspace "http://www.w3.org/2002/07/owl"))
  (assert-true (get-uri-namedspace "http://www.w3.org/1999/02/22-rdf-syntax-ns"))
  (assert-true (get-uri-namedspace "http://www.w3.org/2001/XMLSchema")))

(define-test test/namespace/package-mapping
  (assert-eq (find-package :rdfs) (uri2package "http://www.w3.org/2000/01/rdf-schema"))
  (assert-eq (find-package :owl) (uri2package "http://www.w3.org/2002/07/owl"))
  (assert-eq (find-package :rdf) (uri2package "http://www.w3.org/1999/02/22-rdf-syntax-ns"))
  (assert-eq (find-package :xsd) (uri2package "http://www.w3.org/2001/XMLSchema")))

(define-test test/namespace/blank-node
  (assert-equal "_:abc" (write-to-string (nodeID2symbol "abc")))
  (assert-equal "_:def" (write-to-string (nodeID2symbol "def")))
  (let (nodelist)
    (dotimes (i 10)
      (pushnew (make-unique-nodeID "abc") nodelist)
      (assert-eq (+ 1 i) (length nodelist)))))

;; (pprint
;; (mapcar #'cl:type-of
;; (with-open-file (file #p"GX:data;Intro.rdf")
;;   (gx::parse-rdf file))))

;;;;;
;; Local Variables:
;; indent-tabs: nil
;; outline-regexp: ";;[;]+"
;; End:
;;;;;
