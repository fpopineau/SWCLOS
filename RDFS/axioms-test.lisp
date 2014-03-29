;;;-*- mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;; IT Program Project in Japan:
;;;          Building Operation-Support System for Large-scale System using IT
;;;
;;; RDF/S Test Forms
;;;
;;; Copyright © 2004 by Galaxy Express Corporation
;;;
;;; History
;;; -------
;;; 2004.10.09    Axioms test is separated.
;;; 2003.11.20    File created
;;;

(in-package :gx-user)

(defparameter RDF-axioms
  '((typep |rdf|:|type| |rdf|:|Property|)
    (typep |rdf|:|subject| |rdf|:|Property|)
    (typep |rdf|:|predicate| |rdf|:|Property|)
    (typep |rdf|:|object| |rdf|:|Property|)
    (typep |rdf|:|first| |rdf|:|Property|)
    (typep |rdf|:|rest| |rdf|:|Property|)
    (typep |rdf|:|value| |rdf|:|Property|)
    (typep |rdf|:|nil| |rdf|:|List|)))

(defparameter RDFS-axioms
  '((subtypep |rdf|:|Alt| |rdfs|:|Container|)
    (subtypep |rdf|:|Bag| |rdfs|:|Container|)
    (subtypep |rdf|:|Seq| |rdfs|:|Container|)
    (subtypep |rdfs|:|ContainerMembershipProperty| |rdf|:|Property|)
    (typep |rdf|:|XMLLiteral| |rdfs|:|Datatype|)
    (subtypep |rdf|:|XMLLiteral| |rdfs|:|Literal|)
    (subtypep |rdfs|:|Datatype| |rdfs|:|Class|)))

(defparameter RDFSvalid-axioms
  '((typep |rdfs|:|Resource| |rdfs|:|Class|)
    (typep |rdfs|:|Class| |rdfs|:|Class|)
    (typep |rdfs|:|Literal| |rdfs|:|Class|)
    (typep |rdf|:|XMLLiteral| |rdfs|:|Class|)
    (typep |rdfs|:|Datatype| |rdfs|:|Class|)
    (typep |rdf|:|Seq| |rdfs|:|Class|)
    (typep |rdf|:|Bag| |rdfs|:|Class|)
    (typep |rdf|:|Alt| |rdfs|:|Class|)
    (typep |rdfs|:|Container| |rdfs|:|Class|)
    (typep |rdf|:|List| |rdfs|:|Class|)
    (typep |rdfs|:|ContainerMembershipProperty| |rdfs|:|Class|)
    (typep |rdf|:|Property| |rdfs|:|Class|)
    (typep |rdf|:|Statement| |rdfs|:|Class|)

    (typep |rdfs|:|domain| |rdf|:|Property|)
    (typep |rdfs|:|range| |rdf|:|Property|)
    (typep |rdfs|:|subPropertyOf| |rdf|:|Property|)
    (typep |rdfs|:|subClassOf| |rdf|:|Property|)
    (typep |rdfs|:|member| |rdf|:|Property|)
    (typep |rdfs|:|seeAlso| |rdf|:|Property|)
    (typep |rdfs|:|isDefinedBy| |rdf|:|Property|)
    (typep |rdfs|:|comment| |rdf|:|Property|)
    (typep |rdfs|:|label| |rdf|:|Property|)))

(defparameter Domain-axioms
  '((eq (|rdfs|:|domain| |rdf|:|type|) |rdfs|:|Resource|)
    (eq (|rdfs|:|domain| |rdfs|:|domain|) |rdf|:|Property|)
    (eq (|rdfs|:|domain| |rdfs|:|range|) |rdf|:|Property|)
    (eq (|rdfs|:|domain| |rdfs|:|subPropertyOf|) |rdf|:|Property|)
    (eq (|rdfs|:|domain| |rdfs|:|subClassOf|) |rdfs|:|Class|)
    (eq (|rdfs|:|domain| |rdf|:|subject|) |rdf|:|Statement|)
    (eq (|rdfs|:|domain| |rdf|:|predicate|) |rdf|:|Statement|)
    (eq (|rdfs|:|domain| |rdf|:|object|) |rdf|:|Statement|)
    (eq (|rdfs|:|domain| |rdfs|:|member|) |rdfs|:|Resource|)
    (eq (|rdfs|:|domain| |rdf|:|first|) |rdf|:|List|)
    (eq (|rdfs|:|domain| |rdf|:|rest|) |rdf|:|List|)
    (eq (|rdfs|:|domain| |rdfs|:|seeAlso|) |rdfs|:|Resource|)
    (eq (|rdfs|:|domain| |rdfs|:|isDefinedBy|) |rdfs|:|Resource|)
    (eq (|rdfs|:|domain| |rdfs|:|comment|) |rdfs|:|Resource|)
    (eq (|rdfs|:|domain| |rdfs|:|label|) |rdfs|:|Resource|)
    (eq (|rdfs|:|domain| |rdf|:|value|) |rdfs|:|Resource|)))

(defparameter Range-axioms
  '((eq (|rdfs|:|range| |rdf|:|type|) |rdfs|:|Class|)
    (eq (|rdfs|:|range| |rdfs|:|domain|) |rdfs|:|Class|)
    (eq (|rdfs|:|range| |rdfs|:|range|) |rdfs|:|Class|)
    (eq (|rdfs|:|range| |rdfs|:|subPropertyOf|) |rdf|:|Property|)
    (eq (|rdfs|:|range| |rdfs|:|subClassOf|) |rdfs|:|Class|)
    (eq (|rdfs|:|range| |rdf|:|subject|) |rdfs|:|Resource|)
    (eq (|rdfs|:|range| |rdf|:|predicate|) |rdf|:|Property|) ;|rdfs|:|Resource| in "rdf-mt" is wrong.
    (eq (|rdfs|:|range| |rdf|:|object|) |rdfs|:|Resource|)
    (eq (|rdfs|:|range| |rdfs|:|member|) |rdfs|:|Resource|)
    (eq (|rdfs|:|range| |rdf|:|first|) |rdfs|:|Resource|)
    (eq (|rdfs|:|range| |rdf|:|rest|) |rdf|:|List|)
    (eq (|rdfs|:|range| |rdfs|:|seeAlso|) |rdfs|:|Resource|)
    (eq (|rdfs|:|range| |rdfs|:|isDefinedBy|) |rdfs|:|Resource|)
    (eq (|rdfs|:|range| |rdfs|:|comment|) |rdfs|:|Literal|)
    (eq (|rdfs|:|range| |rdfs|:|label|) |rdfs|:|Literal|)
    (eq (|rdfs|:|range| |rdf|:|value|) |rdfs|:|Resource|)))

(defparameter Other-axioms
  '((subtypep |rdf|:|Alt| |rdfs|:|Container|)
    (subtypep |rdf|:|Bag| |rdfs|:|Container|)
    (subtypep |rdf|:|Seq| |rdfs|:|Container|)
    (subtypep |rdfs|:|ContainerMembershipProperty| |rdf|:|Property|)

    (eq (cl:first (superproperty-of |rdfs|:|isDefinedBy|)) |rdfs|:|seeAlso|)

    (typep |rdf|:|XMLLiteral| |rdfs|:|Datatype|)
    (subtypep |rdf|:|XMLLiteral| |rdfs|:|Literal|)
    (subtypep |rdfs|:|Datatype| |rdfs|:|Class|)))

(defun check (axioms)
  (loop for axiom in axioms
      do (print axiom)
        ;(format t " -> ")
        (prin1 (eval axiom))))

#|
(check RDF-axioms)
(check RDFS-axioms)
(check RDFSvalid-axioms)
(check Domain-axioms)
(check Range-axioms)
(check Other-axioms)
|#


