;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;
;;; SWCLOS Dependency Module
;;

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :gx
    (:export )
    )
  (require :rdfscore)
  )

(in-package :gx)

;; The dependee is an origin of effects or dependents.
;; Usually, the number of dependee is less than the number of dependents in the system.
;; If an entry in the system has no dependent, the entry is removal without side effects.
;; If an entry has dependents, truth in the system must be maintain. For exmaple, these
;; triples
;; <Person> <|rdfs|:|subClassOf|> <Animal>.
;; <isParentOf> <|rdfs|:|domain|> <Animal>.
;; <Seiji> <isParentOf> <Takuto>.
;; make the following dependency.
;; dependee:actor,(<|rdf|:|type|> <|rdfs|:|domain|> <|rdfs|:|Resource|>) -> dependent:(<Person> <|rdf|:|type|> <|rdfs|:|Resource|>)
;; dependee:actor,(<|rdf|:|type|> <|rdfs|:|range|> <|rdfs|:|Resource|>)  -> dependent:(<Animal> <|rdf|:|tyep|> <|rdfs|:|Resource|>)
;; dependee:actor                                            -> dependent:(<Person> <|rdfs|:|subClassOf|> <Animal>)
;;
;; dependee:actor,(<|rdfs|:|domain|> <|rdfs|:|domain|> <|rdf|:|Property|>) -> dependent:(<isParent> <|rdf|:|type|> <|rdf|:|Property|>)
;; dependee:actor,(<|rdfs|:|domain|> <|rdfs|:|range|> <|rdf|:|Property|>)  -> dependent:(<Animal> <|rdf|:|tyep|> <|rdfs|:|Resource|>)
;; dependee:actor                                              -> dependent:(<isParentOf> <|rdfs|:|domain|> <Animal>)
;;
;; dependee:actor,(<isParentOf> <|rdfs|:|domain|> <Animal>)       -> dependent:(<Seiji> <|rdf|:|type|> <Animal>)
;; dependee:actor, RDF universe                               -> dependent:(<Takuto> <|rdf|:|type|> <|rdfs|:|Resource|>)
;; dependee:actor                                             -> dependent:(<Seiji> <isParentOf> <Takuto>)
;;
;; Here, dependent:(<Seiji> <isParentOf> <Takuto>) has no more dependents but
;; dependent:(<isParentOf> <|rdfs|:|domain|> <Animal>) is also dependee and has another dependent.
;; The retraction of dependee:(<isParentOf> <|rdfs|:|domain|> <Animal>) affects to
;; dependent:(<Seiji> <|rdf|:|type|> <Animal>).

(defmethod mop:update-dependent ((class |rdfs|:|Resource|) dependent &rest reinitargs)
  (format t "UPDATE-DEPENDENT ~S ~S ~S" class dependent reinitargs))

(defun del-direct-slot-definition (class slotname)
  (when (find slotname (mop:class-direct-slots class) :key #'mop:slot-definition-name)
    (ensure-instance-slot-unbound class slotname)
    (mapcar #'mop:slot-definition-name
      (remove-if-not #'property-direct-slotd-p (mop:class-direct-slots class)))
    (let ((args (make-initargs-from-slotds
                 (delete slotname (mop:class-direct-slots class) :key #'mop:slot-definition-name))))
      ;(format t "~%deletedArgs:~S" args)
      (reinitialize-instance class :direct-slots args))))

(defun ensure-instance-slot-unbound (class slot-name)
  (let ((slotd (find slot-name (mop:class-slots class) :key #'mop:slot-definition-name)))
    (loop for individual in (collect-all-instances (slot-definition-subject-type slotd))
        when (and (slot-exists-p individual slot-name)
                  (slot-boundp individual slot-name)
                  (slot-value individual slot-name))
        do ;(format t "~&Slot ~S of ~S forced unbound." class slot-name)
          (slot-makunbound class slot-name))))
