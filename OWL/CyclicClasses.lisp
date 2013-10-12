(in-package :gx-user)

;;; Search for (*) for reading the file

;;; Concept model (*)
#|
Wallb  -- has-viewb -->   Wall-Viewb
Wallb  <-- view-ofb  --   Wall-Viewb
  |                           |   
  |                           |   
Facadeb  -- has-viewb --> Facade-Viewb
Facadeb  <-- view-ofb  -- Facade-Viewb
|#

;;; Individuals: (*)
;;; eo::Wall-Viewb-10018 with view-ofb eo::Wall-10018   OK
;;; eo::Wall-10018 with has-viewb eo::Wall-Viewb-10018  stack overflow

;;; Is somehow the model not allowed?

;; by Seiji
(defpackage eo 
  (:use ) ; supressing using common lisp package
  )
;; end by Seiji

;;; Concept model (*)

(defProperty eo::has-viewb
    (rdfs:domain eo::So)
  (rdfs:range eo::Viewb))

(defProperty eo::view-ofb
  (rdfs:domain eo::Viewb)
  (rdfs:range eo::So))

(defResource eo::So (rdf:type owl:Class))

(defResource eo::Viewb (rdf:type owl:Class))

(defResource eo::Wallb (rdf:type owl:Class)
  (owl:intersectionOf
   eo::So
   (owl:Restriction (owl:onProperty eo::has-viewb)
                    (owl:allValuesFrom eo::Wall-Viewb))
   (owl:Restriction (owl:onProperty eo::has-viewb)      ;  by Seiji
                    (owl:minCardinality 0))
   (owl:Restriction (owl:onProperty eo::has-viewb)      ;  by Seiji
                    (owl:maxCardinality 1))
   ))

(defResource eo::Facadeb (rdf:type owl:Class)
  (owl:intersectionOf
   eo::Wallb
   (owl:Restriction (owl:onProperty eo::has-viewb)
                    (owl:allValuesFrom eo::Facade-Viewb))
   (owl:Restriction (owl:onProperty eo::has-viewb)    ; by Seiji
                    (owl:minCardinality 0))
   (owl:Restriction (owl:onProperty eo::has-viewb)    ; by Seiji
                    (owl:maxCardinality 1))
   ))

(defResource eo::Wall-Viewb
    (owl:intersectionOf 
     eo::Viewb
     (owl:Restriction (owl:onProperty eo::view-ofb)
                      (owl:allValuesFrom eo::Wallb))
     (owl:Restriction (owl:onProperty eo::view-ofb)    ; by Seiji
                      (owl:minCardinality 1))
     (owl:Restriction (owl:onProperty eo::view-ofb)    ; by Seiji
                      (owl:maxCardinality 1))
     ))

(defResource eo::Facade-Viewb 
    (owl:intersectionOf 
     eo::Wall-Viewb
     (owl:Restriction (owl:onProperty eo::view-ofb)
                      (owl:allValuesFrom eo::Facadeb))
     (owl:Restriction (owl:onProperty eo::view-ofb)     ; by Seiji
                      (owl:minCardinality 1))
     (owl:Restriction (owl:onProperty eo::view-ofb)     ; by Seiji
                      (owl:maxCardinality 1))))


;;; Individuals (*)
(defIndividual eo::Wall-Viewb-10018 (rdf:type eo::Wall-Viewb)
  (eo::view-ofb eo::Wall-10018))

;;; ok up to here 
;;; with this form: Stack overflow (see below)   <--- fixed by Seiji
(defIndividual eo::Wall-10018 (rdf:type eo::Wallb)
  (eo::has-viewb eo::Wall-Viewb-10018))

