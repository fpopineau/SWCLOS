;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; rdfs-test.lisp
;;;;;
;;;;; Maintainer:  <danlentz@gmail.com>
;;;;;

(in-package :gx-user)

(define-test test/rdfs/compute-type-in-effective-slot-definition
  (assert-equal (find-class 'cl:standard-class)
    (class-of (defclass C1 () ((s :type cl:number)))))
  (assert-equal (find-class 'cl:standard-class)
    (class-of (defclass C2 (C1) ((s :type cl:float)))))
  (assert-equal (find-class 'cl:standard-class)
    (class-of (defclass C3 (C1) ((s :type cl:integer)))))
  (assert-equal (find-class 'cl:standard-class)
    (class-of (defclass C4 (C3) ((s :type cl:fixnum)))))
  #+nil (defclass C5 (C4 C2) ())
  #+nil (mop:slot-definition-type (first (mop:class-slots
                                     (mop:finalize-inheritance (find-class 'C5))))))

(define-test test/rdfs/equality-semantics
  (assert-true  (rdf-equalp "this is string." "this is string."))
  (assert-false (rdf-equalp "string" "string"@en))
  (assert-true  (rdf-equalp "wine"@en (@ "wine" "EN")))
  (assert-true  (rdf-equalp 1 1.0))
  (assert-false (rdf-equalp 1 "1"^^|xsd|:|integer|))
  (assert-true  (rdf-equalp "1"^^|xsd|:|integer| (^^ 1 |xsd|:|integer|)))
  (assert-false (rdf-equalp "1"^^|xsd|:|integer| (^^ 1 |xsd|:|nonNegativeInteger|)))
  (assert-true  (rdf-equalp (uri "http://somewhere") (uri "http://somewhere")))
  (assert-false (rdf-equalp (uri "http://somewhere") (uri "http://some%20where")))
  (assert-false (rdf-equalp 'foo 'bar))
  (assert-equal '|rdfs:Resource| (type-of (defIndividual foo)))
  (assert-equal '|rdfs:Resource| (type-of (defIndividual bar)))
  (assert-false (rdf-equalp foo bar)))

(define-test test/rdfs/non-unique-name-assumption
  (assert-equal '|rdfs:Resource| (type-of (defIndividual foo)))
  (assert-equal '|rdfs:Resource| (type-of (defIndividual bar)))
  (assert-false (let ((gx::*nonUNA* t)) (rdf-equalp foo bar)))
  (assert-false (rdf-equalp <http://somewhere> <http://anotherplace>))
  (assert-equal "#<|rdfs:Resource| :anonymous>"
    (write-to-string
      (setf (uri-value <http://somewhere>) (addForm '(|rdf|:|Description|)))))
  (assert-equal "#<|rdfs:Resource| :anonymous>"
    (write-to-string
      (setf (uri-value <http://anotherplace>) (addForm '(|rdf|:|Description|)))))
  (assert-false (rdf-equalp <http://somewhere> <http://anotherplace>))
  #+nil (let ((*nonUNA* t))
          (assert-true (rdf-equalp <http://somewhere> <http://anotherplace>))))


(define-test test/rdfs/object-classification
  (assert-eq   (symbol-value '|rdfs|:|Class|) (find-class '|rdfs|:|Class|))
  (assert-eq   (symbol-value '|rdfs|:|Resource|) (find-class '|rdfs|:|Resource|))
  (assert-eq   (symbol-value '|rdf|:|Property|) (find-class '|rdf|:|Property|))
  (assert-true (cl:typep (find-class 'gx::meta-galaxy) (find-class 'cl:standard-class)))
  (assert-true (cl:typep (find-class 'gx::galaxy) (find-class 'gx::meta-galaxy)))
  (assert-true (cl:typep (find-class 'gx::_rdfsClass) (find-class 'cl:standard-class)))
  (assert-true (cl:typep (find-class '|rdfs|:|Class|) (find-class 'gx::_rdfsClass)))
  (assert-true (cl:typep (find-class 'gx::_rdfsClass) (find-class 'gx::meta-galaxy)))
  (assert-true (cl:typep (find-class '|rdfs|:|Resource|) (find-class '|rdfs|:|Class|)))
  (assert-true (cl:typep (find-class '|rdf|:|Property|) (find-class '|rdfs|:|Class|)))
  (assert-true (cl:subtypep (find-class 'gx::galaxy) (find-class 'cl:standard-object)))
  (assert-true (cl:subtypep (find-class 'gx::meta-galaxy) (find-class 'cl:standard-class)))
  (assert-true (cl:subtypep (find-class 'gx::_rdfsClass) (find-class 'cl:standard-class)))
  (assert-true (cl:subtypep (find-class 'gx::_rdfsClass) (find-class 'gx::meta-galaxy)))
  (assert-true (cl:subtypep (find-class '|rdfs|:|Class|) (find-class 'gx::meta-galaxy)))
  (assert-true (cl:subtypep (find-class '|rdfs|:|Resource|) (find-class 'gx::galaxy)))
  (assert-true (cl:subtypep (find-class '|rdfs|:|Class|) (find-class '|rdfs|:|Resource|)))
  (assert-true (cl:subtypep (find-class '|rdf|:|Property|) (find-class '|rdfs|:|Resource|))))

(define-test test/rdfs/rdf-subtype-inclusion
  (assert-equal '(t t)     (multiple-value-list (rdf-subtypep |xsd|:|long| |xsd|:|decimal|)))
  (assert-equal '(t t)     (multiple-value-list (subtypep |xsd|:|long| |xsd|:|decimal|)))
  (assert-equal '(nil t)   (multiple-value-list (rdf-subtypep `(not ,|xsd|:|long|) |xsd|:|decimal|)))
  (assert-equal '(nil nil) (multiple-value-list (subtypep `(not ,|xsd|:|long|) |xsd|:|decimal|)))
  (assert-equal '(nil t)   (multiple-value-list (rdf-subtypep |xsd|:|decimal| `(not ,|xsd|:|long|))))
  (assert-equal '(nil nil) (multiple-value-list (subtypep |xsd|:|decimal| `(not ,|xsd|:|long|)))))

(define-test test/rdfs/most-specific-concepts
  (assert-equal
    (list
      (find-class '|xsd|:|unsignedInt|)
      (find-class '|xsd|:|positiveInteger|)
      (find-class '|xsd|:|int|))
    (most-specific-concepts
      (list
        |xsd|:|integer| |xsd|:|int|
        |xsd|:|positiveInteger|
        |xsd|:|nonNegativeInteger|
        |xsd|:|unsignedInt|))))

(define-test test/rdfs/lisp-type-mapping
  (assert-eq  (type-of 32767)               '|xsd|:|short|)
  (assert-eq  (type-of 2147483647)          '|xsd|:|int|)
  (assert-eq  (type-of 9223372036854775807) '|xsd|:|long|)
  (assert-eq  (type-of "string?")           '|xsd|:|string|)
  (assert-eq  (type-of "Literal?"@en)       '|rdf|:|XMLLiteral|)
  (assert-eq  (type-of ())                  '|rdf|:|List|)
  (assert-eq  (type-of '(a b c))            '|rdf|:|List|)
  (assert-eq  (type-of |xsd|:|true|)            '|xsd|:|boolean|)
  (assert-eq  (type-of |rdf|:|Property|)        '|rdfs|:|Class|)
  (assert-eq  (type-of |rdfs|:|Class|)          '|rdfs|:|Class|))

(define-test test/rdfs/extended-rdf-type-predicate-xsd-datatypes
  (assert-true (typep 1 |xsd|:|positiveInteger|))
  (assert-true (typep -1 |xsd|:|negativeInteger|))
  (assert-true (typep 0 |xsd|:|nonNegativeInteger|))
  (assert-true (typep 0 |xsd|:|nonPositiveInteger|))
  (assert-true (typep 32767 |xsd|:|short|))
  (assert-true (typep 32768 |xsd|:|int|))
  (assert-true (typep 2147483647 |xsd|:|int|))
  (assert-true (typep 2147483648 |xsd|:|long|))
  (assert-true (typep 9223372036854775807 |xsd|:|long|))
  (assert-true (typep 9223372036854775808 |xsd|:|integer|))
  (assert-true (typep 1 |xsd|:|decimal|))
  (assert-true (typep (cl:rational 1.0) |xsd|:|decimal|))
  (assert-true (typep 1.0e0 |xsd|:|float|))
  (assert-true (typep 1.0d0 |xsd|:|double|))
  (assert-true (typep "string?" |xsd|:|string|))
  (assert-true (typep "string?"@en |xsd|:|string|))
  (assert-true (typep (net.uri:uri "http://somewhere") |xsd|:|anyURI|))
  (assert-true (typep |xsd|:|false| |xsd|:|boolean|))
  (assert-true (typep 1 |xsd|:|anySimpleType|))
  (assert-true (typep 1 |rdf|:|XMLLiteral|))
  (assert-true (typep "1"^^|xsd|:|positiveInteger| |xsd|:|positiveInteger|))
  (assert-true (typep "1"^^|xsd|:|positiveInteger| |xsd|:|anySimpleType|))
  (assert-true (typep "1"^^|xsd|:|positiveInteger| |rdf|:|XMLLiteral|)))

(define-test test/rdfs/extended-rdf-type-predicate-rdf-literals
  (assert-true (typep 1 |rdfs|:|Literal|))
  (assert-true (typep 1 |rdfs|:|Resource|))
  (assert-true (typep "1"^^|xsd|:|positiveInteger| |rdfs|:|Literal|))
  (assert-true (typep "1"^^|xsd|:|positiveInteger| |rdfs|:|Resource|))
  (assert-true (typep "subway"@en |rdf|:|XMLLiteral|))
  (assert-true (typep "subway"@en |rdfs|:|Literal|))
  (assert-true (typep |rdfs|:|label| |rdf|:|Property|)))

(define-test test/rdfs/extended-rdf-type-predicate-other
  (assert-true (typep (list 1 2 3) |rdf|:|List|))
  (assert-true (typep 1 (list 'and |xsd|:|integer| |rdf|:|XMLLiteral|)))
  (assert-true (typep 1 (list 'or |xsd|:|integer| |xsd|:|float|)))
  (assert-true (typep |rdf|:|Property| |rdfs|:|Class|))
  (assert-true (typep |rdfs|:|Class| |rdfs|:|Class|))
  (assert-true (typep |rdfs|:|label| |rdfs|:|Resource|))
  (assert-true (typep |rdf|:|Property| |rdfs|:|Resource|))
  (assert-true (typep |rdfs|:|Class| |rdfs|:|Resource|)))

(define-test test/rdfs/negation-normal-form-reduction
  (assert-true (tree-equal
                 '(or (not A) (and C (not D)))
                 (gx::->nnf '(not (and (not (or (not A) (and C (not D))))))))))

(define-test test/rdfs/most-specific-concept-among-domain-constraints
  (addForm
   '(|rdfs|:|Class| Species
      (|rdfs|:|subClassOf| |rdfs|:|Class|)        ; this makes Species a metaclass
      (|rdfs|:|comment| "This example is for the demonstration of addForm.")))
  (addForm
    '(|rdfs|:|Class| EndangeredSpecies
       (|rdfs|:|subClassOf| Species)))         ; a subclass of metaclass is a metaclass
  (addForm
    '(|rdf|:|Property| estimatedPopulation
       (|rdfs|:|domain| EndangeredSpecies)
       (|rdfs|:|range| |xsd|:|nonNegativeInteger|)))
  (assert-eq (find-class 'EndangeredSpecies)
    (class-of
      (addForm
        '(|rdfs|:|Class| Hawk
           (estimatedPopulation 2000)))))         ; MSC is computed as EndangeredSpecies
  (addForm '(Hawk Harry)))


(define-test test/rdfs/rdf-axioms
  (assert-true (typep |rdf|:|type| |rdf|:|Property|))
  (assert-true (typep |rdf|:|subject| |rdf|:|Property|))
  (assert-true (typep |rdf|:|predicate| |rdf|:|Property|))
  (assert-true (typep |rdf|:|object| |rdf|:|Property|))
  (assert-true (typep |rdf|:|first| |rdf|:|Property|))
  (assert-true (typep |rdf|:|rest| |rdf|:|Property|))
  (assert-true (typep |rdf|:|value| |rdf|:|Property|))
  (assert-true (typep |rdf|:|nil| |rdf|:|List|)))

(define-test test/rdfs/rdfs-axioms
  (assert-true (subtypep |rdf|:|Alt| |rdfs|:|Container|))
  (assert-true (subtypep |rdf|:|Bag| |rdfs|:|Container|))
  (assert-true (subtypep |rdf|:|Seq| |rdfs|:|Container|))
  (assert-true (subtypep |rdfs|:|ContainerMembershipProperty| |rdf|:|Property|))
  (assert-true (typep |rdf|:|XMLLiteral| |rdfs|:|Datatype|))
  (assert-true (subtypep |rdf|:|XMLLiteral| |rdfs|:|Literal|))
  (assert-true (subtypep |rdfs|:|Datatype| |rdfs|:|Class|)))

(define-test test/rdfs/rdfs-valid-axioms
  (assert-true (typep |rdfs|:|Resource| |rdfs|:|Class|))
  (assert-true (typep |rdfs|:|Class| |rdfs|:|Class|))
  (assert-true (typep |rdfs|:|Literal| |rdfs|:|Class|))
  (assert-true (typep |rdf|:|XMLLiteral| |rdfs|:|Class|))
  (assert-true (typep |rdfs|:|Datatype| |rdfs|:|Class|))
  (assert-true (typep |rdf|:|Seq| |rdfs|:|Class|))
  (assert-true (typep |rdf|:|Bag| |rdfs|:|Class|))
  (assert-true (typep |rdf|:|Alt| |rdfs|:|Class|))
  (assert-true (typep |rdfs|:|Container| |rdfs|:|Class|))
  (assert-true (typep |rdf|:|List| |rdfs|:|Class|))
  (assert-true (typep |rdfs|:|ContainerMembershipProperty| |rdfs|:|Class|))
  (assert-true (typep |rdf|:|Property| |rdfs|:|Class|))
  (assert-true (typep |rdf|:|Statement| |rdfs|:|Class|))

  (assert-true (typep |rdfs|:|domain| |rdf|:|Property|))
  (assert-true (typep |rdfs|:|range| |rdf|:|Property|))
  (assert-true (typep |rdfs|:|subPropertyOf| |rdf|:|Property|))
  (assert-true (typep |rdfs|:|subClassOf| |rdf|:|Property|))
  (assert-true (typep |rdfs|:|member| |rdf|:|Property|))
  (assert-true (typep |rdfs|:|seeAlso| |rdf|:|Property|))
  (assert-true (typep |rdfs|:|isDefinedBy| |rdf|:|Property|))
  (assert-true (typep |rdfs|:|comment| |rdf|:|Property|))
  (assert-true (typep |rdfs|:|label| |rdf|:|Property|)))

(define-test test/rdfs/rdfs-domain-axioms
  (assert-eq (|rdfs|:|domain| |rdf|:|type|) |rdfs|:|Resource|)
  (assert-eq (|rdfs|:|domain| |rdfs|:|domain|) |rdf|:|Property|)
  (assert-eq (|rdfs|:|domain| |rdfs|:|range|) |rdf|:|Property|)
  (assert-eq (|rdfs|:|domain| |rdfs|:|subPropertyOf|) |rdf|:|Property|)
  (assert-eq (|rdfs|:|domain| |rdfs|:|subClassOf|) |rdfs|:|Class|)
  (assert-eq (|rdfs|:|domain| |rdf|:|subject|) |rdf|:|Statement|)
  (assert-eq (|rdfs|:|domain| |rdf|:|predicate|) |rdf|:|Statement|)
  (assert-eq (|rdfs|:|domain| |rdf|:|object|) |rdf|:|Statement|)
;;  (assert-eq (|rdfs|:|domain| |rdfs|:|member|) |rdfs|:|Resource|)
  (assert-eq (|rdfs|:|domain| |rdf|:|first|) |rdf|:|List|)
  (assert-eq (|rdfs|:|domain| |rdf|:|rest|) |rdf|:|List|)
  (assert-eq (|rdfs|:|domain| |rdfs|:|seeAlso|) |rdfs|:|Resource|)
  (assert-eq (|rdfs|:|domain| |rdfs|:|isDefinedBy|) |rdfs|:|Resource|)
  (assert-eq (|rdfs|:|domain| |rdfs|:|comment|) |rdfs|:|Resource|)
  (assert-eq (|rdfs|:|domain| |rdfs|:|label|) |rdfs|:|Resource|)
  (assert-eq (|rdfs|:|domain| |rdf|:|value|) |rdfs|:|Resource|))

(define-test test/rdfs/rdfs-range-axioms
  (assert-eq (|rdfs|:|range| |rdf|:|type|) |rdfs|:|Class|)
  (assert-eq (|rdfs|:|range| |rdfs|:|domain|) |rdfs|:|Class|)
  (assert-eq (|rdfs|:|range| |rdfs|:|range|) |rdfs|:|Class|)
  (assert-eq (|rdfs|:|range| |rdfs|:|subPropertyOf|) |rdf|:|Property|)
  (assert-eq (|rdfs|:|range| |rdfs|:|subClassOf|) |rdfs|:|Class|)
  (assert-eq (|rdfs|:|range| |rdf|:|subject|) |rdfs|:|Resource|)
  (assert-eq (|rdfs|:|range| |rdf|:|predicate|) |rdf|:|Property|) ;|rdfs|:|Resource| in "rdf-mt" is wrong.
  (assert-eq (|rdfs|:|range| |rdf|:|object|) |rdfs|:|Resource|)
  (assert-eq (|rdfs|:|range| |rdfs|:|member|) |rdfs|:|Resource|)
  (assert-eq (|rdfs|:|range| |rdf|:|first|) |rdfs|:|Resource|)
  (assert-eq (|rdfs|:|range| |rdf|:|rest|) |rdf|:|List|)
  (assert-eq (|rdfs|:|range| |rdfs|:|seeAlso|) |rdfs|:|Resource|)
  (assert-eq (|rdfs|:|range| |rdfs|:|isDefinedBy|) |rdfs|:|Resource|)
  (assert-eq (|rdfs|:|range| |rdfs|:|comment|) |rdfs|:|Literal|)
  (assert-eq (|rdfs|:|range| |rdfs|:|label|) |rdfs|:|Literal|)
  (assert-eq (|rdfs|:|range| |rdf|:|value|) |rdfs|:|Resource|))

(define-test test/rdfs/rdfs-other-axioms
  (assert-true (subtypep |rdf|:|Alt| |rdfs|:|Container|))
  (assert-true (subtypep |rdf|:|Bag| |rdfs|:|Container|))
  (assert-true (subtypep |rdf|:|Seq| |rdfs|:|Container|))
  (assert-true (subtypep |rdfs|:|ContainerMembershipProperty| |rdf|:|Property|))
  (assert-eq   (cl:first (superproperty-of |rdfs|:|isDefinedBy|)) |rdfs|:|seeAlso|)
  (assert-true (typep |rdf|:|XMLLiteral| |rdfs|:|Datatype|))
  (assert-true (subtypep |rdf|:|XMLLiteral| |rdfs|:|Literal|))
  (assert-true (subtypep |rdfs|:|Datatype| |rdfs|:|Class|)))


;;;;;
;; Local Variables:
;; indent-tabs: nil
;; outline-regexp: ";;[;]+"
;; End:
;;;;;


(in-package :|gx|)
(eq (symbol-value '|rdfs|:|Class|) (find-class '|rdfs|:|Class|))
(eq (symbol-value '|rdfs|:|Resource|) (find-class '|rdfs|:|Resource|))
(eq (symbol-value '|rdf|:|Property|) (find-class '|rdf|:|Property|))
(cl:typep (find-class 'rdf-node) (find-class 'cl:standard-class))
(cl:typep (find-class 'gnode) (find-class 'rdf-node))
;; (cl:typep (find-class 'metaRDFSclass) (find-class 'cl:standard-class))
;; (cl:typep (find-class 'RDFSclass) (find-class 'metaRDFSclass))
(cl:typep (find-class '|rdfs|:|Class|) (find-class 'RDFSclass))
(cl:typep (find-class '|rdfs|:|Resource|) (find-class '|rdfs|:|Class|))
(cl:typep (find-class '|rdf|:|Property|) (find-class '|rdfs|:|Class|))
;
(cl:subtypep (find-class 'gnode) (find-class 'cl:standard-object))
(cl:subtypep (find-class 'rdf-node) (find-class 'cl:standard-class))
;; (cl:subtypep (find-class 'metaRDFSclass) (find-class 'cl:standard-class))
(cl:subtypep (find-class 'RDFSclass) (find-class 'rdf-node))
(cl:subtypep (find-class '|rdfs|:|Class|) (find-class 'rdf-node))
(cl:subtypep (find-class '|rdfs|:|Resource|) (find-class 'gnode))
(cl:subtypep (find-class '|rdfs|:|Class|) (find-class '|rdfs|:|Resource|))
(cl:subtypep (find-class '|rdf|:|Property|) (find-class '|rdfs|:|Resource|))

(defpackage :|vin|)
(defResource |vin|::|Zinfandel| (|rdfs|:|subClassOf| |vin|::|Wine|))

(defProperty |vin|::|hasWineDescriptor| (|rdfs|:|domain| |vin|::|Wine|))
(defProperty |vin|::|hasColor|
    (|rdfs|:|subPropertyOf| |vin|:|hasWineDescriptor|))


(defIndividual |vin|::|ElyseZinfandel| (|rdf|:|type| vin::Zinfandel)
  (|vin|::|hasColor| |vin|::|Red|)
  (|vin|::|hasMaker| |vin|::|Elyse|))
(defProperty |vin|:|hasMaker| (|rdfs|:|range| vin::|Winery|))
(defProperty |vin|:|hasColor| (|rdfs|:|range| vin::|Color|))
