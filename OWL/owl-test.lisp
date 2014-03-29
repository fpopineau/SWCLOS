;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; owl-test.lisp
;;;;;
;;;;; Maintainer:  <danlentz@gmail.com>
;;;;;

(in-package :gx-user)

(with-open-file (rdfxml-source-file #p"gx:ontology;wine.rdf"
                  :direction :input :if-does-not-exist :error)
  (gx::parse-rdf rdfxml-source-file))


(define-test test/owl/parse-instantiate-wine-ontology
  (assert-true
    (read-rdf-file #'addRdfXml #p"gx:ontology;wine.rdf")))

(define-test test/owl/parse-instantiate-food-ontology
  (assert-true
    (read-rdf-file #'addRdfXml #p"gx:ontology;food.rdf"))
#+nil  (assert-true
    (tree-equal (get-form food:Fruit)
      '(owl:Class food:Fruit (owl:unionOf food:SweetFruit food:NonSweetFruit)))))
  
  ;; (write-xml vin:Wine)
  ;;   (get-form food:Duck)
  ;; (write-xml food:Fruit)

(defpackage :ex (:use)
  (:documentation "http://www.example.com"))

(defpackage eg
  (:documentation "http://somewhere-for-eg/eg"))


(define-test test/owl/practical0
  (assert-true (defResource eg::Work (rdfs:subClassOf rdfs:Resource)))
  (assert-true (defResource eg::Agent (rdfs:subClassOf rdfs:Resource)))
  (assert-true (defResource eg::Person (rdfs:subClassOf eg::Agent)))
  (assert-true (defResource eg::Document (rdfs:subClassOf eg::Work)))
  (assert-true (defProperty eg::author 
                 (rdfs:domain eg::Document)
                 (rdfs:range eg::Person)))
  (assert-true (defProperty eg::name
                 (rdfs:domain eg::Person)
                 (rdfs:range rdfs:Literal)))
  (assert-true (defProperty ex::title
                 (rdfs:domain eg::Document)
                 (rdfs:range rdfs:Literal)))
  (assert-true (defIndividual eg::Proposal (rdf:type eg::Document)
                 (eg::author (eg::Person (eg::name "Tim Berners-Lee")))
                 (ex::title "Information Management: A Proposal")
                 (rdf:about "http://somewhere-for-eg/eg/Proposal/")))
  (assert-equal "Information Management: A Proposal" (-> eg::Proposal ex::title))
  (assert-equal "Tim Berners-Lee" (-> eg::Proposal eg::author eg::name))
  (assert-eq    'eg::Person (-> eg::Proposal eg::author rdf:type))
  (assert-equal `(rdfs:Resource eg::Proposal
                   (rdf:about ,(uri "http://somewhere-for-eg/eg/Proposal/"))
                   (rdf:type eg::Document)
                   (eg::author (eg::Person (eg::name "Tim Berners-Lee")))
                   (ex::title "Information Management: A Proposal"))
    (get-form eg::Proposal)))

;; (define-test test/owl/practical1
;;   (assert-true (defclass eg::Work (rdfs:Resource) ( )))
;;   (assert-true (defclass eg::Agent (rdfs:Resource) ( )))
;;   (assert-true (defclass eg::Person (eg::Agent)
;;                  ((eg::name :initarg :name :type string))))
;;   (assert-true (defclass eg::Document (eg::Work) 
;;                  ((eg::author :initarg :author :type eg::Person)
;;                    (ex::title :initarg :title)))))

  
(define-test test/owl/typed-unification
  (assert-true (defResource ex::Human (rdf:type owl:Class)))
  (assert-true (defResource ex::Pet (rdf:type owl:Class)))
  (assert-true (defResource ex::Cat (rdf:type owl:Class)
                 (rdfs:subClassOf ex::Pet)))
  (assert-true (defIndividual ex::John (rdf:type ex::Human)
                 (ex::Knows ex::Jane)
                 (ex::Knows ex::Leonid)
                 (ex::Knows ex::Elizabeth)))
  (assert-true (defIndividual ex::Jane (rdf:type ex::Human)))
  (assert-true (defIndividual ex::Leonid (rdf:type ex::Human)))
  (assert-true (defIndividual ex::Elizabeth (rdf:type ex::Cat)))

  (assert-equal '(rdfs:Class ex::Human (rdf:type owl:Class)) (get-form ex::Human))
  
  ;; (tunify '(ex::Knows ex::John ?x) '(ex::Knows ex::John ex::Jane))
  ;; (tunify '(Knows John ?x) '(Knows ?y Leonid))
  ;; (tunify '(Knows John ?x) '(Knows ?y Elizabeth))
  ;; (tunify '(Knows John ?x) '(Knows ?y Elizabeth)
  ;;   `((?x ,Human)))

  (assert-true (defResource ex::Pet 
                 (owl:disjointWith ex::Human)))

  ;; (tunify '(Knows John ?x) '(Knows ?y Elizabeth)
  ;;       `((?x ,Human)))
  ;; (tunify '(HasChild ?x ?y) '(HasChild John Elizabeth))
  ;; (tunify '(HasChild John ?y) '(HasChild ?x Elizabeth))
  ;; (tunify '(HasChild ?x ?y) '(HasChild John Elizabeth)
  ;;   `((?x ,Cat) (?y ,Cat)))
  ;; (tunify '(HasChild ?x ?y) '(HasChild John Elizabeth)
  ;;   `((?x ,Human) (?y ,Cat)))

  (assert-true (defResource ex::Vehcle (rdf:type owl:Class)))
  (assert-true (defResource ex::Car (rdf:type owl:Class)
                 (rdfs:subClassOf ex::Vehcle)))
  (assert-true (defResource ex::Ship (rdf:type owl:Class)
                 (rdfs:subClassOf ex::Vehcle)))
  (assert-true (defResource ex::AmphibiousVehcle (rdf:type owl:Class)
                 (owl:intersectionOf ex::Car ex::Ship)))
  (assert-true (defIndividual ex::MyCar (rdf:type ex::Car))))

  ;;  (tunify '(ex::MyCar) '(?y) `((?y ex::Ship)))
