;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; lean OWL Module
;;;
;;; IT Program Project in Japan:
;;:    Building Operation-Support System for Large-scale System using IT.
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan.
;;;
;;; Copyright (c) 2003, 2004, 2006 by Galaxy Express Corporation
;;;
;;; Copyright (c) 2007, 2008, 2009 Seiji Koide
;;;
;; History
;; -------
;; 2009.10.16    collect-all-instances is renamed to collect-all-instances-of.
;; 2009.10.16    instance-p is renamed to rdf-instance-p
;; 2009.09.04    name RDFSclass is changed to _rdfsClass.
;; 2008.12.11    resource-p is renamed to rdf-objectp
;; 2008.01.18    OneOf module is extracted from leanOWL
;; 2006.02.20    leanOWL file is created in order to include leanTap inference engine
;; 2005.11.25    Restriction subclasses are created.
;; 2003.06.03    File created
;;; ==================================================================================
;;; Loading this file onto RDF modules creats the OWL universe as part of the RDF universe.
;;;
;;; ----------------------------------------------------------------------------------
;;;
;;;                                           ............................
;;;                                           :        :                 :
;;; meta-galaxy -----------------|rdfs|:|Class| --:-----|owl|:|Class| --- |owl|:|Restriction|
;;;     :                     /      : :......:     /
;;;     :               ...../.......:             /
;;;     :               :   /        :            /
;;;     :               :  /  +------:-- |owl|:|Thing|
;;;     :               : /  /       :
;;;  galaxy -- |rdfs|:|Resource| --- |rdf|:|Property|
;;;
;;;  <--, /     super/sub class relation, the direction of super is right to left.
;;;  ..., :     class/instance relation, the direction of class is upward and left to right.
;;; ----------------------------------------------------------------------------------
;;;
;;;; Compatibility between RDF and OWL Universes in SWCLOS
;;; W3C specifies two styles on the compatibility between RDF universe and OWL universe.
;;; One is called OWL Full style in which |rdf|:|Class| is identical to |owl|:|Class|, |rdfs|:|Resource| is
;;; identical to |owl|:|Thing|, and |rdf|:|Property| is identical to |owl|:|ObjectProperty|. Another is
;;; called DL style and in which the semantics does not follow RDF semantics. The class
;;; extensions between |owl|:|Thing|, |owl|:|Class|, and |owl|:|ObjectProperty| are mutually disjoint.
;;; See, http://www.w3.org/TR/owl-semantics/rdfs.html.
;;;
;;; In SWCLOS implementation, |owl|:|Thing| is a subclass of |rdfs|:|Resource|, |owl|:|Class| is a subclass
;;; of |rdfs|:|Class|, and |owl|:|ObjectProperty| is a subclass of |rdf|:|Property|. Thus, OWL universe is
;;; included in RDF universe, and the semantics of RDF is realized in OWL unvierse due to the
;;; CLOS Object-Oriented ususal manner.
;;;
;;;; OWL Full Semantics (classes are also instances).
;;; In OWL Full, every class can be an instance of metaclass(es). This is naturally realized in
;;; SWCLOS with |owl|:|Class| subclassing to |rdfs|:|Class| (eventually to cl:standard-class). In
;;; addition, we set |owl|:|Class| as subclass of |owl|:|Thing| (eventually to |rdfs|:|Resource|). Thus,
;;; classes in OWL (the extension of |owl|:|Class|) inherit the slots of |rdf|:|Resource|, |rdfs|:|label|,
;;; |rdfs|:|comment|, |rdf|:|type|, etc. from |rdfs|:|Resource|.

(cl:provide :owl)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :rdfscore)
  )

(defpackage :gx
  (:use :common-lisp)
  (:export owl-same-p owl-different-p owl-equivalent-p disjoint-p owl-complement-p
           subsumed-p individual-p owl-equalp owl-class-p owl-thing-p
           OneOf intersection-of
           *autoepistemic-local-closed-world*))

(in-package :gx)

;;
;; Unsatisfiable Error in OWL
;;

(define-condition owl-unsatiafiable (error)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (cond ((and (numberp *line-number*) (plusp *line-number*))
              (format stream "~A~%Check the form before line ~S in the file."
                (apply #'format nil fcont args)
                *line-number*))
             (t (format stream "~A"
                  (apply #'format nil fcont args)))))))
  (:documentation "Top error for unsatisfiability in OWL universe")
  )

(define-condition oneof-condition-unsatiafiable (owl-unsatiafiable)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "oneof condition unsatisfiable: ~A"
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable in oneOf")
  )

(define-condition equivalentclass-condition-unsatiafiable (owl-unsatiafiable)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "equivalent class condition unsatisfiable: ~A"
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable in equivalentClasses")
  )

(define-condition disjointwith-condition-unsatiafiable (owl-unsatiafiable)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "disjoint with condition unsatisfiable: ~A"
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable in disjointWith")
  )

(define-condition complementof-condition-unsatiafiable (owl-unsatiafiable)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "complement of condition unsatisfiable: ~A"
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable in complementOf")
  )

(define-condition sameas-condition-unsatiafiable (owl-unsatiafiable)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "sameAs condition unsatisfiable: ~A"
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable in sameAs")
  )

(define-condition differentfrom-condition-unsatiafiable (owl-unsatiafiable)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "differentFrom condition unsatisfiable: ~A"
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable in differentFrom")
  )

;;;
;;;; OWL Property Slot Definition
;;;
(excl:without-redefinition-warnings
(defparameter *default-slot-definition-class* 'OwlProperty-direct-slot-definition
  "Every slot of which name corresponds to RDF property is defined as an instance of
Property-direct-slot-definition. This variable is set to symbol
gx::Property-direct-slot-definition in RDFS module and set to
OwlProperty-direct-slot-definition in OWL module.")
)
(eval-when (:execute :load-toplevel :compile-toplevel)
  (mop:finalize-inheritance (find-class *default-slot-definition-class*))
  (mop:finalize-inheritance (find-class 'OwlProperty-effective-slot-definition))
  )

;;;
;;;; |owl|:|Class| and |owl|:|Thing| with Houskeeping Slots
;;;
;;; Note that the following programming style is useful to use SWCLOS as APIs for applications
;;; that extend semantics of RDF(S) or OWL.
;;;
;;; Before loading RDF(S) or OWL knowledge into SWCLOS, you can define the housekeeping or
;;; application specific methods at CLOS objects and classes in the ontology of application,
;;; then load the ontology in RDF(S) or OWL.
;;;
;;; Adding function of SWCLOS in RDF semantics adds new definition without any collisions onto
;;; CLOS slot definitions and methods. As a result, you can obtain ontological knowledge and
;;; application methods around ontologies.
;;;
;;; In case of no extension of OWL semantics, you can define application methods after loading
;;; OWL ontologies.

(eval-when (:execute :load-toplevel :compile-toplevel)
  (reinitialize-instance
   |rdfs|:|Resource|
   :direct-slots
   `((:name funprop-inverse :initform nil :initfunction ,(load-time-value #'excl::false)
            :initargs (:funprop-inverse))
     (:name inverse-funprop-inverse :initform nil
            :initfunction ,(load-time-value #'excl::false)
            :initargs (:inverse-funprop-inverse))))
  )

(defclass |owl|:|Class| (|rdfs|:|Class|)
  ((complement-class :initarg :complement-class)
   (disjoint-classes :initarg :disjoint-classes :initform ())
   (equivalent-classes :initarg :equivalent-classes :initform ()))
  (:metaclass |rdfs|:|Class|)
  (:documentation "The meta class in OWL universe. This class is a subclass and instance of
|rdfs|:|Class|."))

(defclass |owl|:|Thing| (|rdfs|:|Resource|)
  ((different-from  :initarg :different-from :initform ())
   (same-as         :initarg :same-as :initform ())
   (inverse-transitive :initarg :inverse-transitive :initform ()))
  (:metaclass |owl|:|Class|)
  (:documentation "The top class in OWL universe. This class is a subclass of |rdfs|:|Resource|
and instance of |owl|:|Class|."))

(defun %equivalent-classes-of (c)
  "returns nil if no definition on |owl|:|equivalentClass|."
  (declare (inline))
  (and (slot-exists-p c 'equivalent-classes)
       (slot-value c 'equivalent-classes)))

(defun equivalent-classes-of (c)
  "returns a list of equivalences to <c>. Note that this function
   returns one element list of <c>, when no equivalences defined."
  (declare (inline))
  (or (and (slot-exists-p c 'equivalent-classes)
           (slot-value c 'equivalent-classes))
      (list c)))

(defun owl-class-p (obj)
  "Is this <obj> an instance of |owl|:|Class|?
   Note that |owl|:|Class| and |owl|:|Restriction| is not owl class."
  ;;this is same as '(cl:typep <obj> |owl|:|Class|)'
  (declare (inline))
  (and (excl::standard-instance-p obj)
       (%owl-class-subtype-p (class-of obj))))
(defun %owl-class-subtype-p (class)
  "If you are sure that <class> is a metaobject of CLOS, use this instead of
   (cl:subtypep <class> |owl|:|Class|)."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((eq class (load-time-value |owl|:|Class|)))
        ((mop:class-finalized-p class)
         (and (member (load-time-value |owl|:|Class|)
                         (mop:class-precedence-list class)
                         :test #'eq)
              t))
        ((labels ((walk-partial-cpl (c)
                    (let ((supers (mop:class-direct-superclasses c)))
                      (when (member (load-time-value (find-class '|owl|:|Class|))
                                       supers
                                       :test #'eq)
                        (return-from %owl-class-subtype-p t))
                      (mapc #'walk-partial-cpl supers))))
           (declare (dynamic-extent #'walk-partial-cpl))
           (walk-partial-cpl class)
           nil))))

(defun owl-class? (symbol)
  "Is this bound value to <symbol> is owl class?"
  (declare (inline))
  (and (symbolp symbol) (boundp symbol) (owl-class-p (symbol-value symbol))))

(defun %same-as-of (x)
  "returns nil if no definition on |owl|:|sameAs|."
  (declare (inline))
  (and (slot-exists-p x 'same-as)
       (slot-boundp x 'same-as)
       (slot-value x 'same-as)))
(excl:without-redefinition-warnings
(defun same-as-of (x)
  "returns a list of sames as <x>. Note that this function
   returns one element list of <x>, when no same individuals defined."
  (declare (inline))
  (or (and (slot-exists-p x 'same-as)
           (slot-boundp x 'same-as)
           (slot-value x 'same-as))
      (list x)))
)

(defvar |owl|:|Nothing|)

(excl:without-redefinition-warnings
(defun owl-thing-p (obj)
  "Is this <obj> an instance of |owl|:|Thing|?
   Note that |owl|:|Class| and |owl|:|Thing| is not owl thing."
  ;;this is same as '(cl:typep <obj> |owl|:|Thing|)'
  (declare (inline))
  (and (excl::standard-instance-p obj)
       (%owl-thing-subclass-p (class-of obj))
       (not (eq (name obj) '|owl|:|Nothing|))))
(defun %owl-thing-subclass-p (class)
  "If you are sure that <class> is a metaobject of CLOS, use this instead of
  (cl:subtypep <class> |owl|:|Thing|)."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((eq class (load-time-value |owl|:|Thing|)))
        ((mop:class-finalized-p class)
         (and (member (load-time-value |owl|:|Thing|)
                         (mop:class-precedence-list class)
                         :test #'eq)
              t))
        ((labels ((walk-partial-cpl (c)
                    (let ((supers (mop:class-direct-superclasses c)))
                      (when (member (load-time-value |owl|:|Thing|)
                                       supers
                                       :test #'eq)
                        (return-from %owl-thing-subclass-p t))
                      (mapc #'walk-partial-cpl supers))))
           (declare (dynamic-extent #'walk-partial-cpl))
           (walk-partial-cpl class)
           nil))))
)
;;;
;;; Note the domain & range of |owl|:|equivalentProperty| is |rdf|:|Property| rather than
;;; |owl|:|ObjectProperty|.
;;;

;;;
;;;; Object Property for |owl|:|inverseOf| Book-keeping
;;;

(defclass |owl|:|ObjectProperty| (|rdf|:|Property|)
  ((inverse-inverse-of :initarg :inverse-inverse-of :initform ())
   )
  (:metaclass |rdfs|:|Class|)
  (:documentation "This class defines an inverse slot of the inverse-of property."))

(defun equivalent-property-of (c)
  (declare (inline))
  ;; rule9, rule10
  (or (slot-value c 'equivalent-property) (list c)))

(defmethod change-class :after ((instance |rdf|:|Property|) (new-class |rdfs|:|Class|) &rest initargs)
  "In case that <new-class> is |owl|:|ObjectProperty|, the domain of <instance> is retrieved and
   slot definitions named its domain for <instance> is changed to an instance of
   <OwlProperty-direct-slot-definition>."
  (case (name new-class)
    (|owl|:|ObjectProperty|
     (loop for domain in (mklist (and (slot-boundp instance '|rdfs|:|domain|)
                                      (|rdfs|:|domain| instance)))
         do (loop for slotd in (mop:class-direct-slots domain)
                when (eq (name instance) (mop:slot-definition-name slotd))
                do (unless (cl:typep slotd 'OwlProperty-direct-slot-definition)
                     (change-class slotd (find-class 'OwlProperty-direct-slot-definition))))))))

;;;
;;;; Restriction Subclasses
;;;

(defclass |owl|:|Restriction| (|rdfs|:|Resource|)
  ()
  (:metaclass |rdfs|:|Class|)
  (:documentation "|owl|:|Restriction| is a metaclass a subclass of |owl|:|Class| in OWL."))

(defmethod excl::default-direct-superclasses ((class |owl|:|Restriction|))
  "The default direct superclass of restrictions is |rdfs|:|Resource|."
  (list (load-time-value (find-class '|rdfs|:|Resource|))))

(defclass |owl|:|allValuesFromRestriction| (|owl|:|Restriction|) () (:metaclass |rdfs|:|Class|)
  (:documentation "A class for value restrictions is a subclass of |owl|:|Restriction|."))
(defmethod print-object ((obj |owl|:|allValuesFromRestriction|) stream)
  (cond ((and (slot-exists-p obj '|owl|:|onProperty|)
              (name (slot-value obj '|owl|:|onProperty|))
              (slot-exists-p obj '|owl|:|allValuesFrom|))
         (print-unreadable-object (obj stream :type nil)
           (format stream "Å‡§Ø~S.~S"
             (name (slot-value obj '|owl|:|onProperty|))
             (or (name (slot-value obj '|owl|:|allValuesFrom|))
                 (get-form (slot-value obj '|owl|:|allValuesFrom|))))))
        (t (call-next-method))))

(defmethod name ((object |owl|:|allValuesFromRestriction|))
  (cond ((and (slot-boundp object 'excl::name) (slot-value object 'excl::name)))
        (t (cons '|owl|:|allValuesFromRestriction|
                 (mapcar #'name (mklist (slot-value object '|owl|:|allValuesFrom|)))))))

(defclass |owl|:|someValuesFromRestriction| (|owl|:|Restriction|) () (:metaclass |rdfs|:|Class|))
(defmethod print-object ((obj |owl|:|someValuesFromRestriction|) stream)
  (cond ((and (slot-exists-p obj '|owl|:|onProperty|)
              (name (slot-value obj '|owl|:|onProperty|))
              (slot-exists-p obj '|owl|:|someValuesFrom|))
         (print-unreadable-object (obj stream :type nil)
           (format stream "Å‡•ü~S.~S"
             (name (slot-value obj '|owl|:|onProperty|))
             (or (name (slot-value obj '|owl|:|someValuesFrom|))
                 :anonymous))))
        (t (call-next-method))))

(defmethod name ((object |owl|:|someValuesFromRestriction|))
  (cond ((and (slot-boundp object 'excl::name) (slot-value object 'excl::name)))
        (t (cons '|owl|:|someValuesFromRestriction|
                 (mapcar #'name (mklist (slot-value object '|owl|:|someValuesFrom|)))))))

(defclass |owl|:|hasValueRestriction| (|owl|:|Restriction|) () (:metaclass |rdfs|:|Class|))
(defmethod print-object ((obj |owl|:|hasValueRestriction|) stream)
  (cond ((and (slot-exists-p obj '|owl|:|onProperty|)
              (name (slot-value obj '|owl|:|onProperty|))
              (slot-exists-p obj '|owl|:|hasValue|))
         (print-unreadable-object (obj stream :type nil)
           (format stream "~SÅ‡•É{~S}"
             (name (slot-value obj '|owl|:|onProperty|))
             (cond ((slot-boundp obj '|owl|:|hasValue|)
                    (let ((x (slot-value obj '|owl|:|hasValue|)))
                      (cond ((eq x t) t)
                            ((eq x nil) nil)
                            ((excl::standard-instance-p x)
                             (cond ((name x))
                                   ((datatype-p (class-of x)) x)
                                   (t :anonymous)))
                            (t x))))
                   (t :unbound)))))
        (t (call-next-method))))

(defmethod name ((object |owl|:|hasValueRestriction|))
  (cond ((and (slot-boundp object 'excl::name) (slot-value object 'excl::name)))
        (t (cons '|owl|:|hasValueRestriction|
                 (mapcar #'name (mklist (slot-value object '|owl|:|hasValue|)))))))

(defclass |owl|:|cardinalityRestriction| (|owl|:|Restriction|) () (:metaclass |rdfs|:|Class|))
(defmethod print-object ((obj |owl|:|cardinalityRestriction|) stream)
  (cond ((slot-value obj '|owl|:|onProperty|)
         (print-unreadable-object (obj stream :type nil)
           (prin1 (name (slot-value obj '|owl|:|onProperty|)) stream)
           (and (slot-boundp obj '|owl|:|minCardinality|)
                (format stream "ÅÜ ~S" (slot-value obj '|owl|:|minCardinality|)))
           (and (slot-boundp obj '|owl|:|cardinality|)
                (format stream "= ~S" (slot-value obj '|owl|:|cardinality|)))
           (and (slot-boundp obj '|owl|:|maxCardinality|)
                (format stream "ÅÖ ~S" (slot-value obj '|owl|:|maxCardinality|)))))
        (t (call-next-method))))

(defun owl-cardinality-p (x) (cl:typep x |owl|:|cardinalityRestriction|))

;;;
;;;; OneOf class
;;;
;;; Note that oneOf elements may be in the RDF universe.
;;; Note that the domain of |owl|:|oneOf| is |rdfs|:|Class|. Namely,
;;; The |owl|:|oneOf| slot definition is attached to |rdfs|:|Class|.
(excl:without-redefinition-warnings
(defmethod print-object ((obj |rdfs|:|Class|) stream)
  (cond ((and (slot-boundp obj 'excl::name)
              (slot-value obj 'excl::name))
         (print-unreadable-object (obj stream :type t)
           (prin1 (slot-value obj 'excl::name) stream)))
        ((slot-boundp obj '|owl|:|oneOf|)
         (let ((ones (slot-value obj '|owl|:|oneOf|)))
           (print-unreadable-object (obj stream :type t)
             (when (and (slot-boundp obj 'excl::name)
                        (slot-value obj 'excl::name))
               (prin1 (slot-value obj 'excl::name) stream))
             (princ #\{ stream)
             (prin1 (cond ((delay-p (car ones)) :delayed)
                          ((anonymous-p (car ones)) nil)
                          ((name (car ones))))
                    stream)
             (mapc #'(lambda (one)
                       (princ #\Space stream)
                       (prin1 (cond ((delay-p one) :delayed)
                                    ((anonymous-p one) nil)
                                    ((name one)))
                              stream))
               (cdr ones))
             (princ #\} stream))))
        (t (print-unreadable-object (obj stream :type t)
             (prin1 :unbound stream)))))
)

(excl:without-redefinition-warnings
(defun owl-oneof-p (x)
  "Is this <x> an object that holds |owl|:|oneOf| data?"
  (and (rdf-class-p x)
       (slot-exists-p x '|owl|:|oneOf|)
       (slot-boundp x '|owl|:|oneOf|)
       ))
)

;;;
;;;; How to Calculate a Type Value in Effective Slot Definition in OWL
;;;
;;; After the calculation of initargs as standard slot definition and rdf property slot
;;; definition, the type option is recalculated, if OWL module is loaded. Because, it may
;;; include existential restriction for slot value, and it must be treated in OWL semantics.
;;;
;;; The form of type option is one of the followings.
;;; ----------------------------------------------------------------------------------
;;; <fval> ::= t | <class-meta-object> | (and <fval> ...) |
;;;            (forall <slot-name> <fval>) | (exists <slot-name> <fval>) |
;;;            (fills <slot-name> <fval>)
;;;            (<= <slot-name> <fval>)  | (>= <slot-name> <fval>)  |
;;;            (= <slot-name> <fval>)
;;; ----------------------------------------------------------------------------------
;;; Here, <class-meta-object> is a class object which comes from direct slot definition in RDF,
;;; (and <fval> ...) is computed by <excl::compute-effective-slot-definition-initargs> method
;;; at standard slot definition, (forall <slot-name> <fval>) originates from |owl|:|allValuesFrom|
;;; constraint and (exists <slot-name> <fval>) originates from |owl|:|someValuesFrom| constraint.
;;; (fills <slot-name> <fval>) represents |owl|:|hasValueRestriction|.
;;;
;;; On the other hand, (\<= <slot-name> <fval>) is from |owl|:|minCardinality|,
;;; (>= <slot-name> <fval>) is from |owl|:|maxCardinality|, and (= <slot-name> <fval>) is from
;;; |owl|:|cardinality|.
;;;
;;; To compute the most specific concepts which include the above special <fval> forms,
;;; <most-specific-concepts-for-slotd-type>, <strict-supertype-p-for-slotd-type> and
;;; <strict-subtype-p-for-slotd-type> are defined, which specifically process these <fval>
;;; with <subsumed-p> and <owl-equivalent-p>.

(excl:without-redefinition-warnings
(defmethod excl::compute-effective-slot-definition-initargs ((class |rdfs|:|Class|) direct-slotds)
  (let ((initargs (call-next-method)))
    (let ((type (getf initargs ':type)))
      (when (consp type)
        (cond ((eq (car type) 'and)
               ; this 'and' comes from standard routine in ACL
               ;(assert (not (disjoint-pairs-p (cdr type))) ()
               ;        "Disjoint pairs in computing slot definition in ~S." class)
               ;(setq type (most-specific-concepts-for-slotd-type (cdr type)))
               )
              ((error "Cant happen!")))
        (setf (getf initargs ':type) type)))
    (%compute-effective-slot-definition-initargs
     class (class-name class) (mop:slot-definition-name (car direct-slotds)) direct-slotds initargs)))
(defun %compute-effective-slot-definition-initargs (class class-name slot-name direct-slotds initargs)
  (declare (optimize (speed 3) (safety 0)))
  (cond ((member-if #'owl-property-direct-slotd-p direct-slotds)
         (setq initargs `(:subject-type ,class ,@initargs))
         (loop for slotd in direct-slotds
             with maxc and minc and initform and initfunc
             as slotd-maxc = (and (slot-exists-p slotd 'maxcardinality)
                                  (slot-definition-maxcardinality slotd))
             and
               slotd-minc = (and (slot-exists-p slotd 'mincardinality)
                                 (slot-definition-mincardinality slotd))
             and
               slotd-initfunc = (mop:slot-definition-initfunction slotd) and
               slotd-initform = (mop:slot-definition-initform slotd)
             do
               ;; get minimum maxc over slotds
               ;(describe slotd)
               ;(format t "~%Maxc:~S Slotd-maxc:~S" maxc slotd-maxc)
               ;(format t "~%Minc:~S Slotd-minc:~S" minc slotd-minc)
               (cond ((and maxc slotd-maxc) (setq maxc (min slotd-maxc maxc)))
                     (slotd-maxc (setq maxc slotd-maxc)))
               ;; get maximum minc over slotds
               (cond ((and minc slotd-minc) (setq minc (max slotd-minc minc)))
                     (slotd-minc (setq minc slotd-minc)))
               (when (and slotd-initfunc (property? slot-name) (find-class '|owl|:|TransitiveProperty| nil))
                 (cond ((cl:typep (symbol-value slot-name) (find-class '|owl|:|TransitiveProperty|))
                        (cond ((and initform slotd-initform)
                               (cond ((and (consp initform) (consp slotd-initform))
                                      (error "Not Yet!"))
                                     ((consp initform)
                                      (cond ((some
                                              #'(lambda (x)
                                                  (transitive-subp slot-name slotd-initform x))
                                              initform))
                                            (t (setq initform
                                                     (cons slotd-initform initform))
                                               (setq initfunc
                                                     (cons slotd-initfunc initfunc)))))
                                     ((consp slotd-initform)
                                      (cond ((some #'(lambda (x)
                                                       (transitive-subp slot-name initform x))
                                                   slotd-initform))
                                            (t (setq initform
                                                     (cons initform slotd-initform))
                                               (setq initfunc
                                                     (cons initfunc slotd-initfunc)))))
                                     ((transitive-subp slot-name initform slotd-initform))
                                     ((transitive-subp slot-name slotd-initform initform)
                                      (setq initform slotd-initform)
                                      (setq initfunc slotd-initfunc))
                                     (t (setq initform (list slotd-initform initform))
                                        (setq initfunc (list slotd-initfunc initfunc)))))
                              (slotd-initform ; null initform
                               (setq initform slotd-initform)
                               (setq initfunc slotd-initfunc))))
                       (t (cond ((and initform slotd-initform)
                                 (cond ((and (listp initform) (listp slotd-initform))
                                        (setq initform
                                              (union slotd-initform initform :test #'equal))
                                        (setq initfunc
                                              (union slotd-initfunc initfunc :test #'equal)))
                                       ((and (listp initform)
                                             (not (member slotd-initform initform
                                                          :test #'equal)))
                                        (setq initform (cons slotd-initform initform))
                                        (setq initfunc (cons slotd-initfunc initfunc)))
                                       ((and (listp slotd-initform)
                                             (not (member initform slotd-initform
                                                          :test #'equal)))
                                        (setq initform
                                              (append slotd-initform (list initform)))
                                        (setq initfunc
                                              (append slotd-initfunc (list initfunc))))
                                       ((not (equal initform slotd-initform))
                                        (setq initform (list slotd-initform initform))
                                        (setq initfunc (list slotd-initfunc initfunc)))))
                                (slotd-initform ; null initform
                                 (setq initform slotd-initform)
                                 (setq initfunc slotd-initfunc))))))
             finally (progn
                       (setf (getf initargs ':initform) initform)
                       (setf (getf initargs ':initfunction)
                         (cond ((consp initfunc)
                                (eval `(excl::named-function
                                        (mop:slot-definition-initfunction
                                         ,class-name ,(gentemp (string slot-name)))
                                        (lambda () (mapcar #'funcall ',initfunc)))))
                               (t initfunc)))
                       (setf (getf initargs ':maxcardinality) maxc)
                       (setf (getf initargs ':mincardinality) minc)
                       (assert (or (null maxc) (null minc) (<= minc maxc)) ()
                               "Unsatisfiability by cardinality for ~S ~S"
                               class-name slot-name)
                       ;(format t "~%Computing Effective Slots ... ~S ~S ~S" class-name slot-name direct-slotds)
                       ;(format t "~%                              ~S" initargs)
                       (return initargs))))
        ((member-if #'property-direct-slotd-p direct-slotds)
         ;; if a slotd is property slotd, add subject-type option.
         (setq initargs `(:subject-type ,class ,@initargs))
         (loop for slotd in direct-slotds
             with initform and initfunc
             as
               slotd-initfunc = (mop:slot-definition-initfunction slotd) and
               slotd-initform = (mop:slot-definition-initform slotd)
             do
               (when (and slotd-initfunc (property? slot-name) (find-class '|owl|:|TransitiveProperty| nil))
                 (cond ((cl:typep (symbol-value slot-name) (find-class '|owl|:|TransitiveProperty|))
                        (cond ((and initform slotd-initform)
                               (cond ((and (consp initform) (consp slotd-initform))
                                      (error "Not Yet!"))
                                     ((consp initform)
                                      (cond ((some
                                              #'(lambda (x)
                                                  (transitive-subp slot-name slotd-initform x))
                                              initform))
                                            (t (setq initform
                                                     (cons slotd-initform initform))
                                               (setq initfunc
                                                     (cons slotd-initfunc initfunc)))))
                                     ((consp slotd-initform)
                                      (cond ((some #'(lambda (x)
                                                       (transitive-subp slot-name initform x))
                                                   slotd-initform))
                                            (t (setq initform
                                                     (cons initform slotd-initform))
                                               (setq initfunc
                                                     (cons initfunc slotd-initfunc)))))
                                     ((transitive-subp slot-name initform slotd-initform))
                                     ((transitive-subp slot-name slotd-initform initform)
                                      (setq initform slotd-initform)
                                      (setq initfunc slotd-initfunc))
                                     (t (setq initform (list slotd-initform initform))
                                        (setq initfunc (list slotd-initfunc initfunc)))))
                              (slotd-initform ; null initform
                               (setq initform slotd-initform)
                               (setq initfunc slotd-initfunc))))
                       (t (cond ((and initform slotd-initform)
                                 (cond ((and (listp initform) (listp slotd-initform))
                                        (setq initform
                                              (union slotd-initform initform :test #'equal))
                                        (setq initfunc
                                              (union slotd-initfunc initfunc :test #'equal)))
                                       ((and (listp initform)
                                             (not (member slotd-initform initform
                                                          :test #'equal)))
                                        (setq initform (cons slotd-initform initform))
                                        (setq initfunc (cons slotd-initfunc initfunc)))
                                       ((and (listp slotd-initform)
                                             (not (member initform slotd-initform
                                                          :test #'equal)))
                                        (setq initform
                                              (append slotd-initform (list initform)))
                                        (setq initfunc
                                              (append slotd-initfunc (list initfunc))))
                                       ((not (equal initform slotd-initform))
                                        (setq initform (list slotd-initform initform))
                                        (setq initfunc (list slotd-initfunc initfunc)))))
                                (slotd-initform ; null initform
                                 (setq initform slotd-initform)
                                 (setq initfunc slotd-initfunc))))))
             finally (progn
                       (setf (getf initargs ':initform) initform)
                       (setf (getf initargs ':initfunction)
                         (cond ((consp initfunc)
                                (eval `(excl::named-function
                                        (mop:slot-definition-initfunction
                                         ,class-name ,(gentemp (string slot-name)))
                                        (lambda () (mapcar #'funcall ',initfunc)))))
                               (t initfunc)))
                       ;(format t "~%Computing Effective Slots ... ~S ~S ~S" class-name slot-name direct-slotds)
                       ;(format t "~%                              ~S" initargs)
                       (return initargs))))
        (t initargs)))
)
;;;
;;; If <initargs> in making an effective-slot-definition includes :maxcardinality or
;;; :mincardinality keyword, the slot-definition must be OwlProperty-direct-slot-definition.
(excl:without-redefinition-warnings
(defmethod mop:effective-slot-definition-class ((class |rdfs|:|Class|) &rest initargs)
  "This method calls next method if there is no :maxcardinality keyword in <initargs>."
  (cond ((member :maxcardinality initargs)
         (find-class 'OwlProperty-effective-slot-definition))
        ((member :subject-type initargs)
         (find-class 'gx::Property-effective-slot-definition))
        (t (call-next-method))))

(defun type-option-check-with-cardinality (instance filler slotd oldval)
  (unless slotd ; when slot is an ordinal slot.
    (return-from type-option-check-with-cardinality nil))
  (let ((type (mop:slot-definition-type slotd))
        (name (mop:slot-definition-name slotd))
        (maxc (and (slot-exists-p slotd 'maxcardinality)
                   (slot-value slotd 'maxcardinality)))
        (minc (and (slot-exists-p slotd 'mincardinality)
                   (slot-value slotd 'mincardinality))))
    (typecase type
      (null nil)
      (cons (case (op type)
              (and (mapc #'(lambda (type)
                             (satisfy-filler instance name filler type maxc minc oldval slotd))
                     (cdr type)))
              (or (error "Not Yet!"))
              (not (error "Not Yet!"))
              (otherwise ;; conjunction of type options
               (mapc #'(lambda (type)
                         (satisfy-filler instance name filler type maxc minc oldval slotd))
                 type))))
      (symbol (case type
                ((t) nil) ; nothing done
                (otherwise (cond ((object? type)
                                  (setf (slot-value slotd 'excl::type)
                                    (symbol-value type))
                                  (type-option-check-with-cardinality instance filler slotd oldval))
                                 (t (error "Cant happen!"))))))
      (t (satisfy-filler instance name filler type maxc minc oldval slotd)))))
)
(defun satisfy-filler (x R y type maxc minc oldval slotd)
  (declare (ignore minc))
  (let ((values (append (mklist y) (mklist oldval))))
    (flet ((closed-p () (and maxc (>= (length (mklist oldval)) maxc)))
           (closing-p () (and maxc (>= (length values) maxc)))
           (exacts () (and maxc (= (length values) maxc)))
           (range-satisfy (filler)
                          (cond ((typep filler type) t) ; nothing done
                                ((eq (class-of filler) (load-time-value (symbol-value '|rdfs:Resource|)))
                                 (change-class filler type))
                                ((subsumed-p type (class-of filler))
                                 (warn "Range entail of ~S: change class of ~S to ~S." R filler type)
                                 (change-class filler type))
                                ((disjoint-p (class-of filler) type)
                                 (error "~S of ~S is disjoint to ~S in range entailment"
                                   (type-of filler) filler type))
                                (t ;; shadowing
                                 (error "Not Yet!"))))
           (forall-satisfy (filler)
                           (cond ((typep filler (forall-filler type)) t) ; nothing done
                                 ((eq (class-of filler)
                                      (load-time-value (symbol-value '|rdfs:Resource|)))
                                  (change-class filler (forall-filler type)))
                                 ((subsumed-p (forall-filler type) (class-of filler))
                                  (warn "allValuesFrom entailment: change class ~S to ~S."
                                    filler (forall-filler type))
                                  (change-class filler (forall-filler type)))
                                 ((disjoint-p (class-of filler) (forall-filler type))
                                  (error "~S of ~S is disjoint to allValuesFrom ~S in ~S."
                                    (type-of filler) filler (forall-filler type)
                                    (mop:class-direct-subclasses (slot-subject-type type))))
                                 (t ;; shadowing
                                  (error "Not Yet!"))))
           (exists-satisfy (filler)
                           (cond ((typep filler (exists-filler type)) t) ; satisfied
                                 ((disjoint-p (class-of filler) (exists-filler type))
                                  (error "Disjoint class ~S of ~S to ~S in someValuesFrom."
                                    (type-of filler) filler (exists-filler type)))
                                 ((eq (class-of x) (slot-definition-subject-type slotd))
                                  ;; instance and this role filler is defined for this class
                                  ;; and someValuesFromRestriction defined to this class.
                                  ;; So, filler may be defined under this restriction, may be.
                                  (warn "someValuesFrom entailment: class of ~S is changed to ~S." filler (class-name (exists-filler type)))
                                  (change-class filler (exists-filler type)))
                                 (t (if (y-or-n-p "someValuesFrom entailment: you may or may not type ~S to ~S.~%Do it?"
                                                     filler (exists-filler type))
                                        (change-class filler (exists-filler type))
                                      nil)))) ; then others may satisfy
           (fills-satisfy (filler)
                              (cond ((owl-same-p filler (fills-filler type)) t)  ; nothing done
                                    ((owl-different-p filler (fills-filler type))
                                     (error "hasValue clash: ~S different from ~S."
                                       filler (fills-filler type)))
                                    (t (if (y-or-n-p "hasValue entailment: you may or may not define ~S same as ~S.~%Do it?"
                                                     filler (fills-filler type))
                                           ;; push filler into same group
                                           (shared-initialize-after-for-sameAs
                                            filler (list (fills-filler type)))
                                         nil)))))
      (cond ((and maxc (minusp maxc))
             (error "Clash with max cardinality for ~S ~S." x R))
            ((and maxc (zerop maxc) x)
             (error "Clash with max cardinality for ~S ~S ~S." x R y))
            ((closed-p)
             (error "Clash with max cardinality for ~S ~S ~S ~S." x R y maxc))
            ((and (closing-p) (not (exacts)))
             (cond ((atom y)
                    (cond ((some #'(lambda (old) (owl-same-p y old)) (mklist oldval))
                           (warn "~S sames in ~S: nothing done." y oldval))
                          (t (error "Max cardinality constraint violation ~S ~S ~S into ~S"
                               x R y oldval))))
                   ((null oldval)
                    (cond ((= maxc 1)
                           (warn "Cardinality entailment: All of ~S must be same!" y)
                           (mapc #'(lambda (s) (when (owl-thing-p s)
                                                 (setf (slot-value s 'same-as) y)))
                             y))
                          ((> maxc 1)
                           (error "Cardinality entailment: ~S is given for value of ~S of ~S.~%   but max cardinality ~S~%   Some of them must be same!"
                             y R x maxc))))
                   (t (error "Not Yet!"))))
            (t (typecase type
                 (null nil)
                 (|rdfs|:|Class| (cond ((atom y) (range-satisfy y))
                                   (t (mapc #'(lambda (fil) (range-satisfy fil)) y))))
                 (forall (cond ((atom y) (forall-satisfy y))
                               (t (mapc #'(lambda (fil) (forall-satisfy fil)) y))))
                 (exists (cond ((atom y) (exists-satisfy y))
                               ((some #'(lambda (fil) (exists-satisfy fil)) y))
                               (t )))
                 (fills (cond ((atom y) (fills-satisfy y))
                                  (t (mapc #'(lambda (fil) (fills-satisfy fil)) y))))
                 (t (error "Not Yet!"))))))))

;;;
;;;; Functional Property
;;;

;;; FunctionalProperty may *NOT* be classified to |owl|:|Class|.
;;; However, an instance of |owl|:|FunctionalProperty| must be an instance of |owl|:|ObjectProperty|,
;;; then the range of instance of |owl|:|FunctionalProperty| turns out |owl|:|Thing|.
;;; This requirement is not forced by the OWL specification or the entailment of SWCLOS.
;;; An ontologist must define an instance of |owl|:|FunctionalProperty| as instance of
;;; |owl|:|ObjectProperty| like followings.
;;; ----------------------------------------------------------------------------------
;;;  (defProperty vin::hasMaker
;;;    (|rdf|:|type| |owl|:|FunctionalProperty| |owl|:|ObjectProperty|))
;;; ----------------------------------------------------------------------------------

(defConcept |owl|:|FunctionalProperty| (|rdf|:|type| |rdfs|:|Class|))

(defun functional-property? (name)
  "returns true if <name> is an owl functional property name"
  (declare (inline))
  (and (boundp name) (functional-property-p (symbol-value name))))

(defun functional-property-p (obj)
  "Is this <obj> an instance of |owl|:|FunctionalProperty|?"
  ;;this is same as '(cl:typep <obj> |owl|:|FunctionalProperty|)'
  (and (excl::standard-instance-p obj)
       (let ((class (class-of obj)))
         (cond ((eq class (load-time-value |owl|:|FunctionalProperty|)))
               ((mop:class-finalized-p class)
                (and (member (load-time-value |owl|:|FunctionalProperty|)
                                (mop:class-precedence-list class)
                                :test #'eq)
                     t))
               ((labels ((walk-partial-cpl (c)
                           (let ((supers (mop:class-direct-superclasses c)))
                             (when (member (load-time-value |owl|:|FunctionalProperty|)
                                              supers
                                              :test #'eq)
                               (return-from functional-property-p t))
                             (mapc #'walk-partial-cpl supers))))
                  (declare (dynamic-extent #'walk-partial-cpl))
                  (walk-partial-cpl class)
                  nil))))))

(defun %get-functional-property-sames (x)
  (when (and (owl-thing-p x) (slot-boundp x 'funprop-inverse))
    (loop for (prop subj) in (slot-value x 'funprop-inverse)
        as obj = (slot-value subj prop)
        unless (eql x obj)
        append (remove x (mklist obj)))))

;;;
;;;; Inverse Functional Property
;;;
;;; An instance of |owl|:|InversefunctionalProperty| may not be an instance of |owl|:|ObjectProperty|.
;;; Then, the range value may not be |owl|:|Thing| and may be |rdfs|:|Literal|.

(defConcept |owl|:|InverseFunctionalProperty| (|rdf|:|type| |rdfs|:|Class|))

(defProperty |owl|:|disjointWith|)

(defun inverse-functional-property-p (obj)
  "Is this <obj> an instance of |owl|:|InverseFunctionalProperty|?"
  ;;this is same as '(cl:typep <obj> |owl|:|InverseFunctionalProperty|)'
  (and (excl::standard-instance-p obj)
       (let ((class (class-of obj)))
         (cond ((eq class (load-time-value |owl|:|InverseFunctionalProperty|)))
               ((not (mop:class-finalized-p class))
                (labels ((walk-partial-cpl (c)
                           (let ((supers (mop:class-direct-superclasses c)))
                             (when (member
                                    (load-time-value |owl|:|InverseFunctionalProperty|)
                                    supers
                                    :test #'eq)
                               (return-from inverse-functional-property-p t))
                             (mapc #'walk-partial-cpl supers))))
                  (declare (dynamic-extent #'walk-partial-cpl))
                  (walk-partial-cpl class)
                  nil))
               (t (and (member (load-time-value |owl|:|InverseFunctionalProperty|)
                                  (mop:class-precedence-list class)
                                  :test #'eq)
                       t))))))

(defun %get-inverse-functional-property-sames (x)
  (loop for inv-funprop in (collect-owl-role-name-if #'inverse-functional-property-p x)
      as inv-funprop-val = (and (slot-boundp x inv-funprop) (slot-value x inv-funprop))
      when inv-funprop-val
      append (loop for (subj val) in (collect-all-extensions-of inv-funprop)
                 when (or (equal inv-funprop-val val)
                          (member inv-funprop-val (%same-as-of val) :test #'%owl-same-p))
                 collect subj)))

(defProperty |owl|:|complementOf|)
(defConcept |owl|:|Ontology|)
(defProperty |owl|:|unionOf|)
;;; ==================================================================================
;;;; Here OWL rdf file is read.
;;;

(defProperty owl::imports)          ; just for suppression of entailment warning
(defProperty owl::versionInfo)      ; just for suppression of entailment warning
(defProperty owl::priorVersion)     ; just for suppression of entailment warning

(eval-when (:load-toplevel)
  (let ((*default-pathname-defaults* *load-pathname*))
    (read-rdf-file #'addRdfXml (merge-pathnames "OWL.rdf" *load-pathname*))))
;;; ==================================================================================

;;;
;;;; We add some new axioms for OWL.
;;;

;;; For OWL Full, an owl class also inherit |owl|:|Thing|
(addClass `(,(class-of |owl|:|Class|)) '|owl|:|Class| `(,|owl|:|Thing|) ())
#|
(apply #'mop:ensure-class-using-class (find-class 'shadow-class) 'shadow-class
       :direct-superclasses `(,|owl|:|Class|)
       :metaclass (class-of (find-class 'shadow-class))
       ())
|#
(reinitialize-instance
 (symbol-value '|owl|:|allValuesFrom|)
 '|rdfs|:|domain| (load-time-value (symbol-value '|owl|:|allValuesFromRestriction|)))
(reinitialize-instance
 (symbol-value '|owl|:|someValuesFrom|)
 '|rdfs|:|domain| (load-time-value (symbol-value '|owl|:|someValuesFromRestriction|)))
(reinitialize-instance
 (symbol-value '|owl|:|hasValue|)
 '|rdfs|:|domain| (load-time-value (symbol-value '|owl|:|hasValueRestriction|)))
(reinitialize-instance
 (symbol-value '|owl|:|minCardinality|)
 '|rdfs|:|domain| (load-time-value (symbol-value '|owl|:|cardinalityRestriction|)))
(reinitialize-instance
 (symbol-value '|owl|:|maxCardinality|)
 '|rdfs|:|domain| (load-time-value (symbol-value '|owl|:|cardinalityRestriction|)))
(reinitialize-instance
 (symbol-value '|owl|:|cardinality|)
 '|rdfs|:|domain| (load-time-value (symbol-value '|owl|:|cardinalityRestriction|)))
(eval-when (:execute :load-toplevel)
  (let ((slots
         (remove '|owl|:|allValuesFrom|
                 (remove '|owl|:|hasValue|
                         (remove '|owl|:|someValuesFrom|
                                 (remove '|owl|:|minCardinality|
                                         (remove '|owl|:|maxCardinality|
                                                 (remove '|owl|:|cardinality|
                                                         (mop:class-direct-slots |owl|:|Restriction|)
                                                         :key #'cg:name)
                                                 :key #'cg:name)
                                         :key #'cg:name)
                                 :key #'cg:name)
                         :key #'cg:name)
                 :key #'cg:name)))
    (slot-makunbound |owl|:|Restriction| 'excl::direct-slots)
    (setf (slot-value |owl|:|Restriction| 'excl::direct-slots) slots)
    )
  )

(reinitialize-instance (load-time-value (symbol-value '|owl|:|Thing|))
                       :complement-class (load-time-value (symbol-value '|owl|:|Nothing|))
                       :different-from (list (load-time-value (symbol-value '|owl|:|Nothing|)))
                       :disjoint-classes (list (load-time-value (symbol-value '|owl|:|Nothing|))))

(reinitialize-instance (load-time-value (symbol-value '|owl|:|Nothing|))
                       :complement-class (load-time-value (symbol-value '|owl|:|Thing|))
                       :different-from (list (load-time-value (symbol-value '|owl|:|Thing|)))
                       :disjoint-classes (list (load-time-value (symbol-value '|owl|:|Thing|))))

;;;
;;;; %addForm for OWL
;;; Calling sequence: %addForm (<type> <slots> <role>)
;;; When <type> is an undefined symbol as resource,
;;; # If <type> is a symbol |rdf|:|Description| and <role> is a symbol |owl|:|intersectionOf|,
;;;   |owl|:|unionOf|, then |owl|:|Class| is used for <type>. See, rule2a and rule2b.
;;; # If <role> is a symbol |owl|:|distinctMembers| or |owl|:|oneOf|, then |owl|:|Thing| is used for <type>.
;;;   See. rule3.

(excl:without-redefinition-warnings
(defun get-range-constraint-from (role)
  "This is same as one in RDFS module except |owl|:|intersectionOf|, |owl|:|unionOf| returns |owl|:|Class|,
   and |owl|:|oneOf| returns |owl|:|Thing| instead of |rdf|:|List|."
  (case role
    ((nil) nil)
    ((t) nil)
    ((|owl|:|intersectionOf| |owl|:|unionOf|)
     ;; The range of these properties is |rdf|:|List|, but the range constraint should be |owl|:|Class|
     ;; for list elements.
     |owl|:|Class|)
    ((|owl|:|oneOf|) |rdfs|:|Resource|)
    ((|owl|:|distinctMembers|) |owl|:|Thing|)
    (otherwise
     (when (boundp role)
       (let ((range (get-range (symbol-value role))))
         (if (eql range |rdf|:|List|) |rdfs|:|Resource| range))))))
)

;;;
;;;; Default Super Class in OWL
;;;
;;; If <v> is typed to |owl|:|Class|, then <v> is subtyped to |owl|:|Thing|.

;; rule1a and rule1b by Seiji
(defmethod excl::default-direct-superclasses ((class |owl|:|Class|))
  (list (load-time-value (find-class '|owl|:|Thing|))))

(defmethod make-instance :around ((class (eql |owl|:|Class|)) &rest initargs)
  (cond ((notany #'(lambda (cls)
                     (cl:subtypep cls (load-time-value (symbol-value '|owl|:|Thing|))))
                 (getf initargs :direct-superclasses))
         (setf (getf initargs :direct-superclasses)
           (append (getf initargs :direct-superclasses)
                   (list (load-time-value (symbol-value '|owl|:|Thing|)))))
         (apply #'call-next-method class initargs))
        (t (call-next-method)))
  )

(defmethod change-class :around ((from |rdfs|:|Class|) (to |owl|:|Thing|) &rest initargs)
  (declare (ignore initargs))
  "This is happen when <from> is class in RDF and it is changed into OWL."
  ;(format t "~%Changing(|rdfs|:|Class| |owl|:|Thing|) ~S to ~S" from to)
  (change-class from (find-class '|owl|:|Class|))
  ;; then from is typed to |owl|:|Thing| through |owl|:|Class|.
  )

;;
;; Magic change-class, I don't know why, but this is needed absolutely.
;;

(defmethod change-class :before ((from cl:class) (to cl:class) &rest initargs) ;bug3166
  (declare (ignore initargs))		;bug3166
  (unless (mop:class-finalized-p to)	;bug3254
    (mop:finalize-inheritance to))
  (format nil "~S" (mop:class-prototype to))    ; this is magic code.
  (unless (excl::validate-metaclass-change from (mop:class-prototype to))
    (excl::.program-error "validate-metaclass-change forbids changing the class of ~s to ~s"
                          from to)))

(defmethod change-class :after ((class |rdfs|:|Class|) (new-class (eql |owl|:|Class|))  &rest initargs)
  (declare (ignore initargs))
  (unless (cl:subtypep class (load-time-value (symbol-value '|owl|:|Thing|)))
    (reinitialize-instance class :direct-superclasses `(,(load-time-value (symbol-value '|owl|:|Thing|))))))

(defmethod change-class :after ((class |rdfs|:|Class|) (new-class |owl|:|Class|)  &rest initargs)
  (declare (ignore initargs))
  (unless (cl:subtypep class (load-time-value (symbol-value '|owl|:|Thing|)))
    (reinitialize-instance class :direct-superclasses `(,(load-time-value (symbol-value '|owl|:|Thing|))))))

;;;
;;;; Equivalency as Class
;;;
;;; |owl|:|equivalentOf|, |owl|:|intersectionOf|, |owl|:|unionOf|, |owl|:|complementOf|, and |owl|:|oneOf| are
;;; complete relation. Therefore, at these relation the equivalency as class at righthand
;;; side implies the equivalency as class at lefthand side.

;;;
;;;; equivalentClass
;;;

(defun shared-initialize-after-for-equivalentClass (class equivalentclasses)
  "|owl|:|equivalentClass| is a subproperty of |rdfs|:|subClassOf|, so
  the objects are automatically captured as subclasses in nature.
  However, this characteristics is very tricky and bad for construction of stable ontology.
  Therefore, the class is not asserted as subclass."
  (let ((oldequivs (slot-value class 'equivalent-classes))
        (newequivs equivalentclasses))
    (let ((equivs (adjoin class (union oldequivs newequivs)))
          (disjoints ()))
      (cond ((setq disjoints (intersection equivs equivs :test #'disjoint-p))
             (error 'equivalentclass-condition-unsatiafiable
               :format-control "between ~S and ~S."
               :format-arguments `(,class ,disjoints)))
            (t (mapc #'(lambda (c)
                         (when (owl-class-p c)
                           (setf (slot-value c 'equivalent-classes) equivs)))
                 equivs))))))

(defun %intersection-equivalent (x y)
  "If both <x> and <y> have an intersection of concepts and two sets of intersection are
equivalent as class, then <x> and <y> is equivalent as class. If either or neither has an
instersection, then returns false."
  (when (and (slot-exists-p x '|owl|:|intersectionOf|)    ;added for Hotz's project
             (slot-exists-p y '|owl|:|intersectionOf|)    ;added for Hotz's project
             (slot-boundp x '|owl|:|intersectionOf|)
             (slot-boundp y '|owl|:|intersectionOf|))
    (let ((interx (slot-value x '|owl|:|intersectionOf|))
          (intery (slot-value y '|owl|:|intersectionOf|)))
      (let ((xsupers (remove-if #'owl-restriction-p interx))
            (ysupers (remove-if #'owl-restriction-p intery))
            (xrestrictions (remove-if-not #'owl-restriction-p interx))
            (yrestrictions (remove-if-not #'owl-restriction-p intery)))
        (and (subsetp xsupers ysupers :test #'owl-equivalent-p)
             (subsetp ysupers xsupers :test #'owl-equivalent-p)
             (subsetp xrestrictions yrestrictions :test #'%owl-restriction-equal)
             (subsetp yrestrictions xrestrictions :test #'%owl-restriction-equal))))))

(defun %union-equivalent (x y)
  "If both <x> and <y> have an union of concepts and two sets of union are equivalent as class,
   then <x> and <y> is equivalent as class."
  (when (and (slot-boundp x '|owl|:|unionOf|)
             (slot-boundp y '|owl|:|unionOf|))
    (let ((unionx (slot-value x '|owl|:|unionOf|))
          (uniony (slot-value y '|owl|:|unionOf|)))
      (and (subsetp unionx uniony :test #'owl-equivalent-p)
           (subsetp uniony unionx :test #'owl-equivalent-p)))))

(defun %complemently-equal (x y)
  ;; if both complements are set equal, then equal.
  (cond ((and (slot-boundp x '|owl|:|complementOf|)
              (slot-boundp y '|owl|:|complementOf|))
         (let ((xcomplement (slot-value x '|owl|:|complementOf|))
               (ycomplement (slot-value y '|owl|:|complementOf|)))
           (%owl-equivalent-p-without-complements xcomplement ycomplement)))
        ((and (slot-boundp x 'complement-class)
              (slot-boundp y 'complement-class))
         (let ((xcomplement (slot-value x 'complement-class))
               (ycomplement (slot-value y 'complement-class)))
           (%owl-equivalent-p-without-complements xcomplement ycomplement)))))

(defun %oneof-equal (x-ones y-ones)
  "If two OneOf sets are set-equal, then both are equal as class.
  sets are test by owl-same-p."
  (and (subsetp x-ones y-ones :test #'%owl-same-p)
       (subsetp y-ones x-ones :test #'%owl-same-p)))

(defun %owl-restriction-equal (c1 c2)
  (flet ((get-slot-value (r p) (and (slot-boundp r p) (slot-value r p))))
    (flet ((value-eql (c1 c2)
             (let ((r1-restriction-name (class-name (class-of c1)))
                   (r2-restriction-name (class-name (class-of c2))))
               (and (eq r1-restriction-name r2-restriction-name)
                    (cond ((eq r1-restriction-name '|owl|:|allValuesFromRestriction|)
                           (let ((v1 (mklist (slot-value c1 '|owl|:|allValuesFrom|)))
                                 (v2 (mklist (slot-value c2 '|owl|:|allValuesFrom|))))
                             (and (subsetp v1 v2 :test #'owl-equivalent-p)
                                  (subsetp v2 v1 :test #'owl-equivalent-p))))
                          ((eq r1-restriction-name '|owl|:|someValuesFromRestriction|)
                           ;; someValuesFrom satisfies even if other values exist
                           (some #'(lambda (v1)
                                     (some #'(lambda (v2) (owl-equivalent-p v1 v2))
                                           (mklist (slot-value c2 '|owl|:|someValuesFrom|))))
                                 (mklist (slot-value c1 '|owl|:|someValuesFrom|))))
                          ((eq r1-restriction-name '|owl|:|hasValueRestriction|)
                           ;; hasValue satisfies even if other values exist
                           (some #'(lambda (v1)
                                     (some #'(lambda (v2) (%owl-same-p v1 v2))
                                           (mklist (slot-value c2 '|owl|:|hasValue|))))
                                 (mklist (slot-value c1 '|owl|:|hasValue|))))
                          ((eq r1-restriction-name '|owl|:|cardinalityRestriction|)
                           (let ((c1max (or (get-slot-value c1 '|owl|:|cardinality|)
                                            (get-slot-value c1 '|owl|:|maxCardinality|)
                                            most-positive-fixnum))
                                 (c1min (or (get-slot-value c1 '|owl|:|cardinality|)
                                            (get-slot-value c1 '|owl|:|minCardinality|)
                                            most-negative-fixnum))
                                 (c2max (or (get-slot-value c2 '|owl|:|cardinality|)
                                            (get-slot-value c2 '|owl|:|maxCardinality|)
                                            most-positive-fixnum))
                                 (c2min (or (get-slot-value c2 '|owl|:|cardinality|)
                                            (get-slot-value c2 '|owl|:|minCardinality|)
                                            most-negative-fixnum)))
                             ;; equality at cardinalities
                             (when (cl:typep c1min |rdf|:|XMLLiteral|)
                               (setq c1min (value-of c1min)))
                             (when (cl:typep c2min |rdf|:|XMLLiteral|)
                               (setq c2min (value-of c2min)))
                             (when (cl:typep c1max |rdf|:|XMLLiteral|)
                               (setq c1max (value-of c1max)))
                             (when (cl:typep c2max |rdf|:|XMLLiteral|)
                               (setq c2max (value-of c2max)))
                             (and (= c1min c2min) (= c1max c2max)))))))))
      (and (eq (name (slot-value c1 '|owl|:|onProperty|)) (name (slot-value c2 '|owl|:|onProperty|)))
           (value-eql c1 c2)             ;if empty t
           ))))

(defun %owl-equivalent-p-without-type-equivalents (x y)
  (cond ((intersection (%equivalent-classes-of x) (list y) ; seiji 2009.01.11
                       :test #'%owl-equivalent-p-without-equivalents)
         t)
        ;; rdfp1 by ter Horst
        ((functional-property-equal-p x y :test #'%owl-equivalent-p))
        ;; rdfp2 by ter Horst
        ((inverse-functional-property-equal-p x y :test #'%owl-equivalent-p))
        ))

(excl:without-redefinition-warnings
(defun owl-equivalent-p (x y)
  "returns true if <x> and <y> are equivalent classes."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((equal x y))         ; symbols, objects, and gx:uri. If both are equal, then equal.
        ((and (owl-class-p x) (owl-class-p y))
         (cond ((and (name x) (name y) (equal (name x) (name y))) t) ; if names are equal.
               ;;name may be a cons
               ((and (owl-restriction-p x) (owl-restriction-p y))
                (%owl-restriction-equal x y))
               ((and (owl-oneof-p x) (owl-oneof-p y))
                (%oneof-equal (slot-value x '|owl|:|oneOf|) (slot-value y '|owl|:|oneOf|)))
               ((%intersection-equivalent x y)) ; if intersection slot is equal, then equivalent
               ((%union-equivalent x y))        ; if unionOf slot is equal, then equivalent
               ((%owl-equivalent-p x y))
               ((%complemently-equal x y))
               ;; sameAs? See rdfp6, rdfp7
               ((member x (same-as-of y) :test #'(lambda (a b) (%owl-same-p a b))) t)
               ;; differentFrom?
               ((member x (different-from-of y) :test #'(lambda (a b) (%owl-same-p a b))) nil)
               ;; functional and inversefunctional
               ((and (owl-thing-p x) (owl-thing-p y)
                     (or (functional-property-equal-p x y)             ; rdfp1 by ter Horst
                         (inverse-functional-property-equal-p x y))))  ; rdfp2 by ter Horst
               ((and (not *nonUNA*) (name x) (name y)) nil) ; if UNA and different names
               (t   ; if nonUNA, check subtree, even though different names or anonymous
                (multiple-value-bind (result graph) (rdf-graph-equalp x y)
                  (declare (ignore graph))
                  result))))
        ((and (symbolp x) (object? x) (symbolp y) (object? y))
         (owl-equivalent-p (symbol-value x) (symbol-value y)))
        ((and (symbolp x) (object? x)) (owl-equivalent-p (symbol-value x) y))
        ((and (symbolp y) (object? y)) (owl-equivalent-p x (symbol-value y)))
        ((and (net.uri:uri-p x) (net.uri:uri-p y) (uri= x y))) ; or go through next
        ((and (uri-p x) (uri-p y))                             ; uri-string different but
         (cond ((and *nonUNA* (uri-boundp x) (uri-boundp y))   ; if nonUNA and has value
                (owl-equivalent-p (uri-value x) (uri-value y)))      ; then check values
               (t nil)))                            ; else different uri means different
        ((and (uri-p x) (uri-boundp x)) (owl-equivalent-p (uri-value x) y))
        ((and (uri-p y) (uri-boundp y)) (owl-equivalent-p x (uri-value y)))
        ((and (consp x) (consp y))
         (and (owl-equivalent-p (car x) (car y))
              (owl-equivalent-p (cdr x) (cdr y))))))
)

#|
(owl-equivalent-p food:Wine vin:Wine)
(owl-equivalent-p vin:TableWine vin:DryWine)
|#

(defun %owl-equivalent-p (x y)
  "checks |owl|:|equivalentOf|."
  (cond ((intersection (%equivalent-classes-of x) (%equivalent-classes-of y) ; seiji 2009.01.11
                       :test #'%owl-equivalent-p-without-equivalents)
         t)
        ;; rdfp1 by ter Horst
        ((functional-property-equal-p x y :test #'%owl-equivalent-p))
        ;; rdfp2 by ter Horst
        ((inverse-functional-property-equal-p x y :test #'%owl-equivalent-p))
        ))

(defun %owl-equivalent-p-without-equivalents (x y)
  "This subsubfunction prevents recursive call for %owl-equivalent-p."
  (declare (optimize (speed 3) (safety 0)))
  ;(error "Check it")
  (cond ((equal x y))            ; symbols, objects, and gx:uri. If both are equal, then equal.
        ((and (owl-class-p x) (owl-class-p y))
         (cond ((and (name x) (name y) (equal (name x) (name y))) t) ; if names are equal.
               ;; name may be a cons
               ((and (owl-restriction-p x) (owl-restriction-p y))
                (%owl-restriction-equal x y))
               ((and (owl-oneof-p x) (owl-oneof-p y))
                (%oneof-equal (slot-value x '|owl|:|oneOf|) (slot-value y '|owl|:|oneOf|)))
               ((%intersection-equivalent x y)) ; if intersection slot is equal, then equivalent
               ((%union-equivalent x y))        ; if unionOf slot is equal, then equivalent
  ;;;;;;;;;;;  ((%owl-equivalent-p x y))
               ((%complemently-equal x y))
               ;; sameAs? See rdfp6, rdfp7
               ((member x (same-as-of y) :test #'(lambda (a b) (%owl-same-p a b))) t)
               ;; differentFrom?
               ((member x (different-from-of y) :test #'(lambda (a b) (%owl-same-p a b))) nil)
               ;; functional and inversefunctional
               ((and (owl-thing-p x) (owl-thing-p y)
                     (or (functional-property-equal-p x y)             ; rdfp1 by ter Horst
                         (inverse-functional-property-equal-p x y))))  ; rdfp2 by ter Horst
               ((and (not *nonUNA*) (name x) (name y)) nil) ; if UNA and different names
               (t     ; if nonUNA, check subtree, even though different names or anonymous
                (multiple-value-bind (result graph) (rdf-graph-equalp x y)
                  (declare (ignore graph))
                  result))))
        ((and (consp x) (consp y))
         (and (%owl-equivalent-p-without-equivalents (car x) (car y))
              (%owl-equivalent-p-without-equivalents (cdr x) (cdr y))))))

(defun %owl-equivalent-p-without-complements (x y)
  "returns true if <x> and <y> are equivalent classes or equal literals, symbols,
   and objects. At first, <x> and <y> should be a class."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((equal x y))      ; symbols, objects, and gx:uri. If both are equal, then equal.
        ((and (owl-class-p x) (owl-class-p y))
         (cond ((and (name x) (name y) (equal (name x) (name y))) t) ; if names are equal.
               ;; name may be a cons
               ((and (owl-restriction-p x) (owl-restriction-p y))
                (%owl-restriction-equal x y))
               ((and (owl-oneof-p x) (owl-oneof-p y))
                (%oneof-equal (slot-value x '|owl|:|oneOf|) (slot-value y '|owl|:|oneOf|)))
               ((%intersection-equivalent x y)) ; if intersection slot is equal, then equivalent
               ((%union-equivalent x y))        ; if unionOf slot is equal, then equivalent
               ((%owl-equivalent-p x y))
  ;;;;;;;;;;;  ((%complemently-equal x y))
               ;; sameAs? See rdfp6, rdfp7
               ((member x (same-as-of y) :test #'(lambda (a b) (%owl-same-p a b))) t)
               ;; differentFrom?
               ((member x (different-from-of y) :test #'(lambda (a b) (%owl-same-p a b))) nil)
               ;; functional and inversefunctional
               ((and (owl-thing-p x) (owl-thing-p y)
                     (or (functional-property-equal-p x y)             ; rdfp1 by ter Horst
                         (inverse-functional-property-equal-p x y))))  ; rdfp2 by ter Horst
               ((and (not *nonUNA*) (name x) (name y)) nil) ; if UNA and different names
               (t     ; if nonUNA, check subtree, even though different names or anonymous
                (multiple-value-bind (result graph) (rdf-graph-equalp x y)
                  (declare (ignore graph))
                  result))))
        ((and (consp x) (consp y))
         (and (%owl-equivalent-p-without-complements (car x) (car y))
              (%owl-equivalent-p-without-complements (cdr x) (cdr y))))))

(excl:without-redefinition-warnings
(defun owl-same-p (x y &optional pairs)
  "Are <x> and <y> same as individual in the semantics of OWL?
   This predicate does not test the internal structures of <x> and <y>."
  (cond ((and pairs
              (or (member (cons x y) pairs :test #'equal)
                  (member (cons y x) pairs :test #'equal)))
         nil)    ;; occurence check
        ((and (owl-thing-p x) (owl-thing-p y)
              (member x (same-as-of y)
                         :test #'(lambda (a b) (%owl-same-p a b (cons (cons x y) pairs)))))
         t)
        ((rdf-equalp x y))     ; literals, symbols, objects, uris. See rdfp5a, rdfp5b.
        ; See rdfp6, rdfp7
        ;; rdfp1 by ter Horst
        ((functional-property-equal-p
          x y :test #'(lambda (a b) (%owl-same-p a b (cons (cons x y) pairs)))))
        ;; rdfp2 by ter Horst
        ((inverse-functional-property-equal-p
          x y :test #'(lambda (a b) (%owl-same-p a b (cons (cons x y) pairs)))))
        ((and (symbolp x) (object? x) (symbolp y) (object? y))
         (%owl-same-p (symbol-value x) (symbol-value y) pairs))
        ((and (symbolp x) (object? x)) (%owl-same-p (symbol-value x) y pairs))
        ((and (symbolp y) (object? y)) (%owl-same-p x (symbol-value y) pairs))
        ((and (uri-p x) (uri-p y)) (uri= x y))    ; uris
        ((uri-p x) (%owl-same-p (uri-value x) y pairs))
        ((uri-p y) (%owl-same-p x (uri-value y) pairs))
        ((and (cl:typep x '|rdf|:|inLang|) (cl:typep y '|rdf|:|inLang|))
         (and (eq (lang x) (lang y)) (equal (content x) (content y))))
        ((and (cl:typep x '|rdf|:|XMLLiteral|) (cl:typep y '|rdf|:|XMLLiteral|))
         (and (eq (class-of x) (class-of y)) (equal (value-of x) (value-of y))))))

(defmethod %owl-same-p ((x |rdfs|:|Resource|) (y |rdfs|:|Resource|) &optional pairs)
  "Non-resolution version. This is used in <owl-equalp> and <owl-equivalent-p>."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((and (name x) (name y) (equal (name x) (name y))))
        ;; occurence check
        ((and pairs
              (or (member (cons x y) pairs :test #'equal)
                  (member (cons y x) pairs :test #'equal)))
         nil)
        ((and (owl-thing-p x) (owl-thing-p y)
              (member x (same-as-of y)
                         :test #'(lambda (a b) (%owl-same-p a b (cons (cons x y) pairs))))))
        ; See rdfp6, rdfp7
        ((and (rsc-object-p x) (rsc-object-p y)
              (or
               ;; rdfp1 by ter Horst
               (functional-property-equal-p
                x y :test #'(lambda (a b) (%owl-same-p a b (cons (cons x y) pairs))))
               ;; rdfp2 by ter Horst
               (inverse-functional-property-equal-p
                x y :test #'(lambda (a b) (%owl-same-p a b (cons (cons x y) pairs)))))))
        ((and (owl-thing-p x) (owl-thing-p y)
              (member x (slot-value y 'different-from))) ; stated different
         nil)
        ((rdf-equalp x y))
        ))
)

(defmethod %owl-same-p (x y &optional pairs)
  (declare (ignore pairs))
  (equalp x y))

(defun functional-property-equal-p  (x y &key (test #'%owl-same-p))
  ;; rdfp1 ter Horst
  (not (not (intersection (slot-value x 'funprop-inverse) (slot-value y 'funprop-inverse)
                          :test #'(lambda (xx yy)
                                    (and (eq (car xx) (car yy))                    ; funprop
                                         (funcall test (cadr xx) (cadr yy))))))))  ; object

(defun inverse-functional-property-equal-p (x y &key (test #'%owl-same-p))
  ;; rdfp2 cf. ter Horst
  (not (not
        (some #'(lambda (role)
                  (let ((fil1 (slot-value x role))
                        (fil2 (slot-value y role)))
                    (cond ((equal fil1 fil2) t)
                          ; if fillers are string and equal, then equal (OWL-Full specs)
                          ((null fil1) nil)
                          ((null fil2) nil)
                          ((and (consp fil1) (consp fil2)) (intersection fil1 fil2 :test test))
                          ((consp fil1) (member fil2 fil1 :test test))
                          ((consp fil2) (member fil1 fil2 :test test))
                          ((funcall test fil1 fil2)))))
              (intersection (collect-owl-role-name-if #'inverse-functional-property-p x)
                            (collect-owl-role-name-if #'inverse-functional-property-p y))))))

;;; <owl-equalp> checks the equality of objects in OWL, namely it returns t, if
;;; # owl-equivalent-p holds, if args are property.
;;; # if args are restrictions, then if both of |owl|:|onProperty| values are equal and values are
;;;   equal as |owl|:|allValuesFrom| or |owl|:|someValuesFrom| or |owl|:|hasValue|,
;;;   and either both or neither cardinality restriction are also equal.
;;; # owl-same-p holds.
;;; # otherwise equal in the sense of rdf-graph equality.

(excl:without-redefinition-warnings
(defun owl-equalp (x y)
  "returns true if <x> and <y> is equal in semantics of RDF(S) superimposed by OWL.
   This predicate checks <x> and <y> in classes as individuals."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((equal x y)) ; cl:strings, symbols, objects, and gx:uri. If both are equal, then equal.
        ((and (numberp x) (numberp y))     ; in case of cl:number,
         (= x y))                          ; = tests in value space, e.g., 1 and 1.0 is equal
        ((and (datatype-p (class-of x)) (datatype-p (class-of y)))
         (and (eq (class-of x) (class-of y))
              (rdf-equalp (value-of x) (value-of y))))
        ((and (rsc-object-p x) (rsc-object-p y))
         (cond ((and (name x) (name y) (equal (name x) (name y))) t) ; if names are equal.
               ;; name may be a cons

               ;; owl part
               ((and (owl-restriction-p x) (owl-restriction-p y))
                (%owl-restriction-equal x y))
               ((and (owl-oneof-p x) (owl-oneof-p y))
                (%oneof-equal (slot-value x '|owl|:|oneOf|) (slot-value y '|owl|:|oneOf|)))
               ;; sameAs? See rdfp6, rdfp7
               ((member x (same-as-of y) :test #'(lambda (a b) (%owl-same-p a b))) t)
               ;; differentFrom?
               ((member x (different-from-of y) :test #'(lambda (a b) (%owl-same-p a b))) nil)
               ;; functional and inversefunctional
               ((and (owl-thing-p x) (owl-thing-p y)
                     (or (functional-property-equal-p x y)             ; rdfp1 by ter Horst
                         (inverse-functional-property-equal-p x y))))  ; rdfp2 by ter Horst
               ;((and (property-p x) (property-p y))
               ; (not (not (member x (equivalent-property-of y)))))
               ;; even though intersection and union, another slots must be checked for equality
               ;; therefore, graph-equality-checking must be done.
               ;; end of owl part

               ((and (not *nonUNA*) (name x) (name y)) nil) ; if UNA and different names
               (t  ; if nonUNA, check subtree, even though different names or anonymous
                (multiple-value-bind (result graph) (rdf-graph-equalp x y)
                  (declare (ignore graph))
                  result))))
        ((and (symbolp x) (object? x) (symbolp y) (object? y))
         (owl-equalp (symbol-value x) (symbol-value y)))
        ((and (symbolp x) (object? x)) (owl-equalp (symbol-value x) y))
        ((and (symbolp y) (object? y)) (owl-equalp x (symbol-value y)))
        ((and (net.uri:uri-p x) (net.uri:uri-p y) (uri= x y))) ; or go through next
        ((and (uri-p x) (uri-p y))                             ; uri-string different but
         (cond ((and *nonUNA* (uri-boundp x) (uri-boundp y))   ; if nonUNA and has value
                (rdf-equalp (uri-value x) (uri-value y)))      ; then check values
               (t nil)))                                    ; else different uri means different
        ((and (uri-p x) (uri-boundp x)) (owl-equalp (uri-value x) y))
        ((and (uri-p y) (uri-boundp y)) (owl-equalp x (uri-value y)))
        ((and (cl:typep x '|rdf|:|inLang|) (cl:typep y '|rdf|:|inLang|))
         ;; see, http://www.w3.org/TR/2004/REC-rdf-concepts-20040210/#section-Graph-Literal
         (and (equalp (string (lang x)) (string (lang y))) (string= (content x) (content y))))
        ((and (consp x) (consp y))
         (and (owl-equalp (car x) (car y))
              (owl-equalp (cdr x) (cdr y))))))
)

#|
(owl-equalp vin:TableWine vin:DryWine) -> nil
|#

(defun owl-equalp-for-refining (x y)
  "<x> and <y> must be clos objects but may be anonymous."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((eql x y))
        ((and (datatype-p (class-of x)) (datatype-p (class-of y)))
         (and (eq (class-of x) (class-of y))
              (equalp (value-of x) (value-of y))))
        ((and (rsc-object-p x) (rsc-object-p y))
         (cond ((and (name x) (name y))
                (cond ((equal (name x) (name y)) t)        ; if names are equal, then equal.
                      ;; name may be cons
                      (*nonUNA*
                       (multiple-value-bind (result graph) (rdf-graph-equalp x y)
                         (declare (ignore graph))
                         result))  ; else if nonUNA, check the graph
                      (t nil)))                                      ; else returns nil
               (t (not (not (rdf-graph-equalp x y)))))) ; if anonymous, then check the graph
        ))

;;;
;;;; Disjointness
;;;
;;; When equivalent classes are given for arguments as disjoint classes, the unsatisfiable error
;;; happens. Note that two concepts in disjointness cannot make intersection, and multiple
;;; classing.

(defun shared-initialize-after-for-disjointWith (class disjoints)
  (let ((equiv (find class disjoints :test #'owl-equivalent-p)))
    (cond (equiv (error 'disjointwith-condition-unsatiafiable
                   :format-control "~S is equivalent to ~S."
                   :format-arguments `(,class ,equiv)))
          (t (loop for disjoint in disjoints with result
                 do (cond ((setq result (check-instance-sharing class disjoint))
                           (error 'disjointwith-condition-unsatiafiable
                             :format-control "~S has super-sub relation ~S."
                             :format-arguments `(,(class-name class) ,(class-name result))))
                          (t (pushnew disjoint (slot-value class 'disjoint-classes))
                             (pushnew class (slot-value disjoint 'disjoint-classes)))))))))

;;;
;;; Not only from the disjoint statement, but also |owl|:|intersectionOf| values and |owl|:|oneOf|
;;; values decide the disjointness. Namely, if two values for such complete relational properties
;;; are disjoint, then the arguments are disjoint.

(defun %owl-disjoint-p (c1 c2)
  "special rules of disjointness in OWL.
   This function is used internally in routines of default reasoning for disjointness.
   Namely, it returns <nil nil> if the disjointness is explicitly not stated."
  (cond ((equal c1 c2) (values nil t))
        ((cl:subtypep c1 c2) (values nil t))
        ((cl:subtypep c2 c1) (values nil t))
        ((check-instance-sharing c1 c2) (values nil t))
        ((and (slot-exists-p c1 'disjoint-classes) (slot-boundp c1 'disjoint-classes)
              (member c2 (slot-value c1 'disjoint-classes) :test #'owl-equivalent-p))
         (values t t))
        ((and (slot-exists-p c2 'disjoint-classes) (slot-boundp c2 'disjoint-classes)
              (member c1 (slot-value c2 'disjoint-classes) :test #'owl-equivalent-p))
         (values t t))
        ((and (owl-restriction-p c1) (owl-restriction-p c2))
         (multiple-value-bind (val1 val2) (%restriction-disjoint-p c1 c2)
           (when val2 (return-from %owl-disjoint-p (values val1 val2))))
         (values nil nil))
        ((owl-restriction-p c1) (values nil t))
        ((owl-restriction-p c2) (values nil t))
        ((and (intersection-of c1) (intersection-of c2)
              (multiple-value-bind (val1 val2)
                  (%intersection-disjoint-p (intersection-of c1) (intersection-of c2))
                (when val2 (return-from %owl-disjoint-p (values val1 val2))))))
        ((and (owl-oneof-p c1) (owl-oneof-p c2))
         (let ((c1-ones (slot-value c1 '|owl|:|oneOf|))
               (c2-ones (slot-value c2 '|owl|:|oneOf|)))
           (cond ((intersection c1-ones c2-ones :test #'%owl-same-p)
                  (values nil t))
                 (t (values t t)))))
        (t (let ((supers1 (remove-if #'owl-restriction-p (mop:class-direct-superclasses c1)))
                 (supers2 (remove-if #'owl-restriction-p (mop:class-direct-superclasses c2))))
             ;; this recursion stops at |rdfs|:|Resource| or |owl|:|Class|
             ;; rule4
             (cond ((some #'(lambda (s) (%owl-disjoint-p s c2)) supers1)
                    (values t t))
                   ((some #'(lambda (s) (%owl-disjoint-p c1 s)) supers2)
                    (values t t))
                   (t (values nil nil)))))))

(defun %intersection-disjoint-p (xinter yinter)
  "returns true if <xinter> and <yinter> are disjoint.
   This function returns true if either of args has intersection,
   and returns unknown if neither of args has intersection."
  (cond ((and (null xinter) (null yinter)) (values nil nil))
        ((or (null xinter) (null yinter)) (values t t))
        (t (let ((xsupers (remove-if #'owl-restriction-p xinter))
                 (ysupers (remove-if #'owl-restriction-p yinter))
                 (xrestrictions (remove-if-not #'owl-restriction-p xinter))
                 (yrestrictions (remove-if-not #'owl-restriction-p yinter)))
             (let ((intersects (intersection xsupers ysupers))
                   (val2 t))
               (loop for xsuper in (set-difference xsupers intersects)
                   do (loop for ysuper in (set-difference ysupers intersects)
                          do (multiple-value-bind (v1 v2) (%owl-disjoint-p xsuper ysuper)
                               (when v1 (return-from %intersection-disjoint-p (values t t)))
                               (setq val2 (and v2 val2)))))
               (loop for xrestriction in xrestrictions
                   do (loop for yrestriction in yrestrictions
                          do (multiple-value-bind (v1 v2)
                                 (%owl-disjoint-p xrestriction yrestriction)
                               (when v1 (return-from %intersection-disjoint-p (values t t)))
                               (setq val2 (and v2 val2)))))
               (values nil val2))))))

(defun %restriction-disjoint-p (c1 c2)
  (let ((role1 (name (slot-value c1 '|owl|:|onProperty|)))
        (role2 (name (slot-value c2 '|owl|:|onProperty|))))
    (unless (equivalent-property-p role1 role2)
      (return-from %restriction-disjoint-p (values nil t)))
    ;; compare only if role1 and role2 is equivalent.
    (cond ((cl:typep c1 |owl|:|hasValueRestriction|)
           (cond ((cl:typep c2 |owl|:|someValuesFromRestriction|) (values t t))
                 ((cl:typep c2 |owl|:|allValuesFromRestriction|) (values t t))
                 ((cl:typep c2 |owl|:|hasValueRestriction|)
                  (cond (nil ; *autoepistemic-local-closed-world*
                         (some #'(lambda (fil1)
                                   (some #'(lambda (fil2)
                                             (cond ((multiple-value-bind (val1 val2)
                                                        (owl-different-p fil1 fil2)
                                                      (when val2
                                                        (return-from %restriction-disjoint-p
                                                          (values val1 val2)))))
                                                   ;; else nothing done
                                                   ))
                                         (mklist (slot-value c2 '|owl|:|hasValue|))))
                               (mklist (slot-value c1 '|owl|:|hasValue|)))
                         (values nil nil))
                        (t (values nil nil))))
                 ((error "Cant happen"))))
          ((cl:typep c1 |owl|:|someValuesFromRestriction|)
           (cond ((cl:typep c2 |owl|:|hasValueRestriction|) (values t t))
                 ((cl:typep c2 |owl|:|allValuesFromRestriction|) (values t t))
                 ((cl:typep c2 |owl|:|someValuesFromRestriction|)
                  (cond (nil ; *autoepistemic-local-closed-world*
                         (some #'(lambda (fil1)
                                   (some #'(lambda (fil2)
                                             (cond ((owl-equivalent-p fil1 fil2)
                                                    (return-from %restriction-disjoint-p
                                                      (values nil t)))
                                                   ((multiple-value-bind (val1 val2)
                                                        (%owl-disjoint-p fil1 fil2)
                                                      (when val2
                                                        (return-from %restriction-disjoint-p
                                                          (values val1 val2)))))))
                                         ;; else nothing done
                                         (mklist (slot-value c2 '|owl|:|someValuesFrom|))))
                               (mklist (slot-value c1 '|owl|:|someValuesFrom|)))
                         (values nil nil))
                        (t (values nil nil))))
                 ((error "Cant happen"))))
          ((cl:typep c1 |owl|:|allValuesFromRestriction|)
           (cond ((cl:typep c2 |owl|:|hasValueRestriction|) (values t t))
                 ((cl:typep c2 |owl|:|someValuesFromRestriction|) (values t t))
                 ((cl:typep c2 |owl|:|allValuesFromRestriction|)
                  (if (every #'(lambda (fil1)
                                 (every #'(lambda (fil2)
                                            (when (%owl-disjoint-p fil1 fil2)
                                              (return-from %restriction-disjoint-p
                                                (values t t)))
                                            (owl-equivalent-p fil1 fil2))
                                        (mklist (slot-value c2 '|owl|:|allValuesFrom|))))
                             (mklist (slot-value c1 '|owl|:|allValuesFrom|)))
                      (values nil t)
                    (values nil nil)))
                 ((error "Cant happen"))))
          ((error "Cant happen")))))

#|
(%owl-disjoint-p vin:Zinfandel vin:Vintage)                                -> false
(%owl-disjoint-p vin:RedWine vin:WhiteWine)                                -> true
(%owl-disjoint-p vin:CaliforniaWine vin:ItalianWine)                       -> true
;; because names are different.
(let ((*nonUNA* t)) (%owl-disjoint-p vin:CaliforniaWine vin:ItalianWine))  -> true,
;; because vin:CaliforniaRegion is graph-different from vin:ItalianRegion.
|#

;;;
;;; disjointness in OWL universe is ????

(excl:without-redefinition-warnings
(defun disjoint-p (c d)
  "returns true if <c> and <d> are disjoint in OWL."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((eq c d) (values nil t))
        ((eq c |rdfs|:|Resource|) (values nil t)) ; in RDF universe
        ((eq d |rdfs|:|Resource|) (values nil t))
        ((eq d |owl|:|Nothing|) (values t t))     ; |owl|:|Nothing| shares no instances with any class.
        ((eq c |owl|:|Nothing|) (values t t))
        ((and (eq c |owl|:|Thing|) (owl-class-p d)) (values nil t)) ; |owl|:|Thing| subsumes owl classes
        ((and (owl-class-p c) (eq d |owl|:|Thing|)) (values nil t))
        ((and (consp c) (consp d))
         (case (op c)
           (and (case (op d)
                  (and (some #'(lambda (cc) (some #'(lambda (dd) (disjoint-p cc dd))
                                                  (args d))) (args c)))
                  (or (some #'(lambda (cc) (every #'(lambda (dd) (disjoint-p cc dd))
                                                  (args d))) (args c)))
                  (not (not (every #'(lambda (cc) (disjoint-p cc (arg1 d))) (args c))))
                  (satisfies (error "Not Yet!"))
                  (forall (error "Not Yet!"))
                  (exists (error "Not Yet!"))
                  (fills (error "Not Yet!"))
                  (otherwise (error "Not Yet!"))))
           (or (case (op d)
                 (and (every #'(lambda (cc) (some #'(lambda (dd) (disjoint-p cc dd))
                                                  (args d))) (args c)))
                 (or (every #'(lambda (cc) (every #'(lambda (dd) (disjoint-p cc dd))
                                                  (args d))) (args c)))
                 (not (not (some #'(lambda (cc) (disjoint-p cc (arg1 d))) (args c))))
                 (satisfies (error "Not Yet!"))
                 (forall (error "Not Yet!"))
                 (exists (error "Not Yet!"))
                 (fills (error "Not Yet!"))
                 (otherwise (error "Not Yet!"))))
           (not (case (op d)
                  (and (error "Not Yet!"))
                  (or (error "Not Yet!"))
                  (not (error "Not Yet!"))
                  (satisfies (error "Not Yet!"))
                  (forall (error "Not Yet!"))
                  (exists (error "Not Yet!"))
                  (fills (error "Not Yet!"))
                  (otherwise (error "Not Yet!"))))
           (satisfies (case (op d)
                        (and (error "Not Yet!"))
                        (or (error "Not Yet!"))
                        (not (error "Not Yet!"))
                        (satisfies (error "Not Yet!"))
                        (forall (error "Not Yet!"))
                        (exists (error "Not Yet!"))
                        (fills (error "Not Yet!"))
                        (otherwise (error "Not Yet!"))))
           (forall (case (op d)
                     (and (error "Not Yet!"))
                     (or (error "Not Yet!"))
                     (not (error "Not Yet!"))
                     (satisfies (error "Not Yet!"))
                     (forall (error "Not Yet!"))
                     (exists (error "Not Yet!"))
                     (fills (error "Not Yet!"))
                     (otherwise (error "Not Yet!"))))
           (exists (case (op d)
                     (and (error "Not Yet!"))
                     (or (error "Not Yet!"))
                     (not (error "Not Yet!"))
                     (satisfies (error "Not Yet!"))
                     (forall (error "Not Yet!"))
                     (exists (error "Not Yet!"))
                     (fills (error "Not Yet!"))
                     (otherwise (error "Not Yet!"))))
           (fills (case (op d)
                        (and (error "Not Yet!"))
                        (or (error "Not Yet!"))
                        (not (error "Not Yet!"))
                        (satisfies (error "Not Yet!"))
                        (forall (error "Not Yet!"))
                        (exists (error "Not Yet!"))
                        (fills (if *autoepistemic-local-closed-world*
                                       (when (equivalent-property-p (arg1 c) (arg1 d))
                                         (disjoint-p (arg2 c) (arg2 d)))
                                     (values nil nil)))
                        (otherwise (error "Not Yet!"))))
           (otherwise (case (op d)
                        (and (error "Not Yet!"))
                        (or (error "Not Yet!"))
                        (not (error "Not Yet!"))
                        (satisfies (error "Not Yet!"))
                        (forall (error "Not Yet!"))
                        (exists (error "Not Yet!"))
                        (fills (error "Not Yet!"))
                        (otherwise (error "Not Yet!"))))))
        ((consp c)
         (case (op c)
           (and (let ((cf nil))
                  (some #'(lambda (cc)
                            (multiple-value-bind (val1 val2) (disjoint-p cc d)
                              (when val1 (return-from disjoint-p (values t t)))
                              (setq cf (or cf val2))))
                        (args c))
                  (values nil cf)))
           (or  (let ((cf t))
                  (if (every #'(lambda (cc)
                                 (multiple-value-bind (val1 val2) (disjoint-p cc d)
                                   (setq cf (and cf val2))))
                             (args c))
                      (values t t)
                    (values nil cf))))
           (not (error "Not Yet!"))
           (satisfies (error "Not Yet!"))
           (forall (error "Not Yet!"))
           (exists (error "Not Yet!"))
           (fills (error "Not Yet!"))))
        ((consp d)
         (case (op d)
           (and (let ((cf nil))
                  (some #'(lambda (dd)
                            (multiple-value-bind (val1 val2) (disjoint-p c dd)
                              (when val1 (return-from disjoint-p (values t t)))
                              (setq cf (or cf val2))))
                        (args d))
                  (values nil cf)))
           (or  (let ((cf t))
                  (if (every #'(lambda (dd)
                                 (multiple-value-bind (val1 val2) (disjoint-p c dd)
                                   (setq cf (and cf val2))))
                             (args d))
                      (values t t)
                    (values nil cf))))
           (not (error "Not Yet!"))
           (satisfies (error "Not Yet!"))
           (forall (error "Not Yet!"))
           (exists (error "Not Yet!"))
           (fills (error "Not Yet!"))
           (otherwise (some #'(lambda (dd) (disjoint-p c dd)) d))))
        (t (%disjoint-p c d))))

(defun %disjoint-p (c d)
  (cond ((and (rdf-class-p c) (rdf-class-p d))
         (cond ((or (%clos-subtype-p c d) (%clos-subtype-p d c))
                (values nil t))  ; xsd datatype is also reasoned.
               ((check-instance-sharing c d)
                (values nil t))
               ;; rdf disjoint part
               ((or (and (eq c |xsd|:|nonPositiveInteger|) (eq d |xsd|:|nonNegativeInteger|))
                    (and (eq d |xsd|:|nonPositiveInteger|) (eq c |xsd|:|nonNegativeInteger|)))
                (values nil t))
               ((or (and (%clos-subtype-p c (symbol-value '|xsd|:|integer|))
                         (%clos-subtype-p d (symbol-value '|xsd|:|nonPositiveInteger|)))
                    (and (%clos-subtype-p d (symbol-value '|xsd|:|integer|))
                         (%clos-subtype-p c (symbol-value '|xsd|:|nonPositiveInteger|)))
                    (and (%clos-subtype-p c (symbol-value '|xsd|:|integer|))
                         (%clos-subtype-p d (symbol-value '|xsd|:|nonNegativeInteger|)))
                    (and (%clos-subtype-p d (symbol-value '|xsd|:|integer|))
                         (%clos-subtype-p c (symbol-value '|xsd|:|nonNegativeInteger|))))
                (values nil t))
               ((or (and (%clos-subtype-p c (symbol-value '|xsd|:|nonPositiveInteger|))
                         (%clos-subtype-p d (symbol-value '|xsd|:|positiveInteger|)))
                    (and (%clos-subtype-p d (symbol-value '|xsd|:|nonPositiveInteger|))
                         (%clos-subtype-p c (symbol-value '|xsd|:|positiveInteger|))))
                (values t t))
               ((or (and (%clos-subtype-p c (symbol-value '|xsd|:|nonPositiveInteger|))
                         (%clos-subtype-p d (symbol-value '|xsd|:|unsignedLong|)))
                    (and (%clos-subtype-p d (symbol-value '|xsd|:|nonPositiveInteger|))
                         (%clos-subtype-p c (symbol-value '|xsd|:|unsignedLong|))))
                (values t t))
               ((or (and (%clos-subtype-p c (symbol-value '|xsd|:|nonNegativeInteger|))
                         (%clos-subtype-p d (symbol-value '|xsd|:|negativeInteger|)))
                    (and (%clos-subtype-p d (symbol-value '|xsd|:|nonNegativeInteger|))
                         (%clos-subtype-p c (symbol-value '|xsd|:|negativeInteger|))))
                (values t t))
               ;; implicit disjointness of datatype
               ((and (%clos-subtype-p c |xsd|:|anySimpleType|)
                     (%clos-subtype-p d |xsd|:|anySimpleType|))
                (loop for csub in (cons c (collect-all-subs c))
                    with dsubs = (cons d (collect-all-subs d))
                    do (loop for dsub in dsubs
                           when (or (%clos-subtype-p csub dsub) (%clos-subtype-p dsub csub))
                           do (return-from %disjoint-p (values nil t))))
                (values t t))
               ;; end of rdf disjoint part
               (t (let ((val2 nil))
                    (loop for cc in (equivalent-classes-of c)
                        do (loop for dd in (equivalent-classes-of d)
                               do (multiple-value-bind (v1 v2) (%owl-disjoint-p cc dd)
                                    (cond (v1 (return-from %disjoint-p (values t t)))
                                          (v2 (setq val2 t))))))
                    (values nil val2)))))
        ((and (rdf-instance-p c) (rdf-instance-p d))
         ;; for transitive property, instances have subsumption
         (if gx::*autoepistemic-local-closed-world*
             (cond ((or (subsumed-p c d) (subsumed-p d c))
                    (values nil t))
                   (t (values t t)))
           (values nil nil)))
        (t (typecase c
             (forall (typecase d
                       (forall (cond ((%disjoint-p (forall-filler c) (forall-filler d))
                                      (values t t))
                                     (t (values nil nil))))
                       (exists (cond ((%disjoint-p (forall-filler c) (exists-filler d))
                                      (values t t))
                                     (t (values nil nil))))
                       (fills (cond ((typep (fills-filler d) (forall-filler c))
                                         (values nil t))
                                        (t (values nil nil))))
                       (|rdfs|:|Class| (disjoint-p (forall-filler c) d))
                       (|rdfs|:|Resource| (error "Not Yet!"))
                       (t (values nil nil))))
             (exists (typecase d
                       (exists (cond ((%owl-disjoint-p (exists-filler c) (exists-filler d))
                                      (values t t))
                                     (t (values nil nil))))
                       (forall (cond ((%disjoint-p (exists-filler c) (forall-filler d))
                                      (values t t))
                                     (t (values nil nil))))
                       (fills (cond ((typep (fills-filler d) (exists-filler c))
                                         (values nil t))
                                        (t (values nil nil))))
                       (|rdfs|:|Class| (cond ((disjoint-p (exists-filler c) d)
                                          ;(format t "~%disjoint ~S ~S" c d)
                                          (values t t))
                                         (t (values nil nil))))
                       (|rdfs|:|Resource| (error "Not Yet!"))
                       (t (values nil nil))))
             (fills (typecase d
                          (fills (disjoint-p (fills-filler c) (fills-filler d)))
                          (forall (cond ((typep (fills-filler c) (forall-filler d))
                                         (values nil t))
                                        (t (values nil nil))))
                          (exists (cond ((typep (fills-filler c) (exists-filler d))
                                         (values nil t))
                                        (t (values nil nil))))
                          (|rdfs|:|Class| (cond ((typep (fills-filler c) d)
                                             (values nil t))
                                            (t (values nil nil))))
                          (|rdfs|:|Resource| (values nil nil))
                          (t (values nil nil))))
             (|rdfs|:|Class| (typecase d
                           (forall (disjoint-p c (forall-filler d)))
                           (exists (disjoint-p c (exists-filler d)))
                           (fills (cond ((typep (fills-filler d) c)
                                             (values nil t))
                                            (t (values nil nil))))
                           (|rdfs|:|Class| (error "Cant happen!"))
                           (t (values nil nil))))
             (|rdfs|:|Resource| (typecase d
                              (forall (values nil nil))
                              (exists (values nil nil))
                              (fills (values nil nil))
                              (|rdfs|:|Resource| (error "Cant happen!"))
                              (t (values nil nil))))
             (t (values nil nil))
             ))))
)

;;;
;;;; Subsumption
;;;

;;;
;;; (subsumed-p vin:DryWhiteWine vin:WhiteNonSweetWine) -> t
;;;

;;;
;;; (subsumed-p food:Fruit food:EdibleThing)
;;; (subsumed-p food:Bananas food:Fruit)
;;;

(defun %union-subsumed-p (c d)
  (let (cdisjuncts ddisjuncts)
    (or (and (setq cdisjuncts (union-of c))
             (every #'(lambda (csub) (subsumed-p-without-equivalency csub d nil nil)) cdisjuncts))
        (and (setq ddisjuncts (union-of d))
             (some #'(lambda (dsub) (subsumed-p-without-equivalency c dsub nil nil)) ddisjuncts)))))


(defun %intersection1-subsumed-p (conjuncts d)
  "This functio ndirectly computes conjuncts instead of C's intersection. This is useful to compute
   conjunction (and) without creating an intersection class."
  (let ((dintersections (intersection-of d)))
    (cond (dintersections                ; if C is subsumed by D's intersections
           (%intersection12-subsumed-p conjuncts dintersections))
          (t (error "Not Yet!")))))
(defun %intersection12-subsumed-p (conjuncts dintersections)
  (loop for c in conjuncts
      unless (mop:class-finalized-p c)
      do (mop:finalize-inheritance c))
  (let ((cpl (remove-duplicates (loop for c in conjuncts append (clos:class-precedence-list c)))))
    (and (every #'(lambda (ds)
                    (some #'(lambda (cs) (subsumed-p cs ds))
                          (remove-if #'owl-restriction-p cpl)))
                (remove-if #'owl-restriction-p dintersections))
         (let ((drestrs (remove-if-not #'owl-restriction-p dintersections)))
           (unless drestrs (return-from %intersection12-subsumed-p t))
           (let ((props (remove-duplicates
                         (mapcar #'(lambda (dr) (name (onproperty-of dr))) drestrs))))
             (loop for prop in props
                 always
                   (let* ((drs (remove-if-not  ; y's restriction
                                #'(lambda (dr) (eq prop (name (onproperty-of dr))))
                                drestrs))
                          (cslot (remove-duplicates
                                  (loop for c in conjuncts
                                      when (find prop (mop:class-slots c)
                                                 :key #'mop:slot-definition-name)
                                      collect it)))
                          (cmax (slot-definition-maxcardinality cslot))
                          (cmin (slot-definition-mincardinality cslot))
                          (crs (mklist (mop:slot-definition-type cslot))))
                     ;(format t "~%crs: ~S cmax: ~S cmin: ~S~%drs: ~S" crs cmax cmin drs)
                     ;; crs includes every user defined restriction and range constraint.
                     ;; x: predecessor, y: successor
                     (let ((alls (loop for cn in crs
                                     when (cl:typep cn 'forall)
                                     collect (forall-filler cn)))
                           (exists (loop for cn in crs
                                       when (cl:typep cn 'exists)
                                       collect (exists-filler cn)))
                           (fillers (loop for cn in crs
                                        when (cl:typep cn 'fills)
                                        collect (fills-filler cn))))
                       ;(format t "~%alls:~S exists:~S fillers:~S" alls exists fillers)
                       (let ((models* (generate-models cmax alls exists fillers nil)))
                         ;(format t "~%models*:~S" models*)
                         ;; now we obtained C's models, then all of them satisfy D's constraint?
                         (loop for models in models*
                             thereis (loop for model in models
                                         always (satisfy-model nil model drs))))))))))))

(defun what-if-subsumed-p-with-cardinality (crs drs)
  (let ((others (remove-if #'owl-cardinality-p crs))
        (cards (remove-if-not #'owl-cardinality-p crs)))
    (assert (length=1 cards))
    (let* ((obj (car cards))
           ;(R (slot-value obj '|owl|:|onProperty|))
           (maxc (value-of (or (and (slot-boundp obj '|owl|:|cardinality|)
                                    (slot-value obj '|owl|:|cardinality|))
                               (and (slot-boundp obj '|owl|:|maxCardinality|)
                                    (slot-value obj '|owl|:|maxCardinality|))))))
      (setq maxc (value-of maxc))
      (cond ((= maxc 1) ; all y in R(x,y) must be identical, so it is easy to infer.
             ;; all constraints are for one successor
             (let ((hasconsts (loop for cn in others when (cl:typep cn |owl|:|hasValueRestriction|) collect cn))
                   (someconsts (loop for cn in others when (cl:typep cn |owl|:|someValuesFromRestriction|) collect cn))
                   (allconsts (loop for cn in others when (cl:typep cn |owl|:|allValuesFromRestriction|) collect cn)))
               (let ((alltypes (mapcar #'(lambda (x) (slot-value x '|owl|:|allValuesFrom|)) allconsts))
                     (sometypes (mapcar #'(lambda (x) (slot-value x '|owl|:|someValuesFrom|)) someconsts))
                     (hasvals (mapcar #'(lambda (x) (slot-value x '|owl|:|hasValue|)) hasconsts)))
                 ;(format t "~%~S ~S ~S" alltypes sometypes hasvals)
                 (every #'(lambda (dr)
                            ;(format t "~%  ~S" dr)
                            (etypecase dr
                              (|owl|:|allValuesFromRestriction|
                               (let ((drtype (slot-value dr '|owl|:|allValuesFrom|)))
                                 (and (every #'(lambda (ty) (subsumed-p ty drtype)) alltypes)
                                      (or (null sometypes)
                                          (some #'(lambda (ty) (subsumed-p ty drtype)) sometypes))
                                      (or (null hasvals)
                                          (some #'(lambda (v) (typep v drtype)) hasvals)))))
                              (|owl|:|someValuesFromRestriction|
                               (let ((drtype (slot-value dr '|owl|:|someValuesFrom|))
                                     (crtype (when (or alltypes sometypes)
                                               `(and ,@(append alltypes sometypes)))))
                                 (cond ((and crtype hasvals) (error "Not Yet!"))
                                       (crtype (subsumed-p crtype drtype))
                                       (hasvals (some #'(lambda (v) (typep v drtype)) hasvals))
                                       (t nil))))
                              (|owl|:|hasValueRestriction|
                               (let ((dval (slot-value dr '|owl|:|hasValue|)))
                                 (some #'(lambda (v) (owl-same-p v dval)) hasvals)))))
                        drs))))
            (t (error "Not Yet!"))))))

(defun what-if-hasvalue-subsumed-p-with-cardinality (crs drs)
  (let ((others (remove-if #'owl-cardinality-p crs))
        (cards (remove-if-not #'owl-cardinality-p crs)))
    (assert (length=1 cards))
    (let* ((obj (car cards))
           ;(R (slot-value obj '|owl|:|onProperty|))
           (maxc (value-of (or (and (slot-boundp obj '|owl|:|cardinality|)
                                    (slot-value obj '|owl|:|cardinality|))
                               (and (slot-boundp obj '|owl|:|maxCardinality|)
                                    (slot-value obj '|owl|:|maxCardinality|))))))
      (setq maxc (value-of maxc))
      (cond ((= maxc 1) ; all y in R(x,y) must be identical, so it is easy to infer.
             (every #'(lambda (dr) (subsumed-p `(and ,@others) dr)) drs))
            (t (error "Not Yet!"))))))

;; (subsumed-p vin:DryWine vin:TableWine)
;; (subsumed-p vin:DryWhiteWine vin:WhiteNonSweetWine)
#|
(defConcept Doctor (|rdfs|:|subClassOf| Person))
(defConcept Employee (|rdfs|:|subClassOf| Person))
(defConcept Employer (|rdfs|:|subClassOf| Person)
  (|owl|:|disjointWith| Employee))
(defConcept DoctorAsEmployee
    (|owl|:|intersectionOf| Employee Doctor))
(defConcept DoctorAsEmployer
    (|owl|:|intersectionOf| Employer Doctor))
(defConcept DoctorSelfEmployed
    (|owl|:|intersectionOf| Doctor
                        (|owl|:|Class| (|owl|:|complementOf| Employee))
                        (|owl|:|Class| (|owl|:|complementOf| Employer))))

(subsumed-p DoctorSelfEmployed Doctor)
(disjoint-p DoctorSelfEmployed DoctorAsEmployee)
(disjoint-p DoctorSelfEmployed DoctorAsEmployer)
(disjoint-p DoctorAsEmployee DoctorAsEmployer)

(defConcept Parent
    (|owl|:|intersectionOf| Person
                        (|owl|:|Restriction| (|owl|:|onProperty| hasChild)
                                         (|owl|:|someValuesFrom| Person))))
(defConcept ParentOfEmployee
    (|owl|:|intersectionOf| Person
                        (|owl|:|Restriction| (|owl|:|onProperty| hasChild)
                                         (|owl|:|someValuesFrom| Employee))))
(defConcept ParentOfEmployer
    (|owl|:|intersectionOf| Person
                        (|owl|:|Restriction| (|owl|:|onProperty| hasChild)
                                         (|owl|:|someValuesFrom| Employer))))
(defConcept ParentOfDoctor
    (|owl|:|intersectionOf| Person
                        (|owl|:|Restriction| (|owl|:|onProperty| hasChild)
                                         (|owl|:|someValuesFrom| Doctor))))
(subsumed-p ParentOfEmployee Parent)
(subsumed-p ParentOfEmployer Parent)
(subsumed-p ParentOfDoctor Parent)
(subsumed-p ParentOfEmployer ParentOfDoctor)
(disjoint-p ParentOfEmployee ParentOfDoctor)

(defConcept ParentOfAllDoctors
    (|owl|:|intersectionOf| Person
                        (|owl|:|Restriction| (|owl|:|onProperty| hasChild)
                                         (|owl|:|allValuesFrom| Doctor))))

(subsumed-p ParentOfAllDoctors ParentOfDoctor)
(subsumed-p ParentOfAllDoctors ParentOfEmployer)
(subsumed-p ParentOfEmployer ParentOfAllDoctors)

(defConcept ParentOfDoctorAsEmployee
    (|owl|:|intersectionOf| Person
                        (|owl|:|Restriction| (|owl|:|onProperty| hasChild)
                                         (|owl|:|someValuesFrom| DoctorAsEmployee))))
(defConcept ParentOfDoctorAsEmployer
    (|owl|:|intersectionOf| Person
                        (|owl|:|Restriction| (|owl|:|onProperty| hasChild)
                                         (|owl|:|someValuesFrom| DoctorAsEmployer))))
(defConcept ParentOfDoctorSelfEmployed
    (|owl|:|intersectionOf| Person
                        (|owl|:|Restriction| (|owl|:|onProperty| hasChild)
                                         (|owl|:|someValuesFrom| DoctorSelfEmployed))))

(subsumed-p ParentOfDoctorAsEmployee ParentOfDoctor)
(subsumed-p ParentOfDoctorAsEmployee ParentOfEmployee)
(subsumed-p ParentOfDoctorAsEmployee ParentOfEmployer)

(defConcept IntersectOfParentsOfEmployeeAndParentsOfDoctor
    (|owl|:|intersectionOf| ParentOfEmployee ParentOfDoctor))

(subsumed-p IntersectOfParentsOfEmployeeAndParentsOfDoctor ParentOfDoctorAsEmployee) -> nil

(defConcept ParentOfOneChildDoctorAndEmployee
    (|owl|:|intersectionOf| ParentOfEmployee ParentOfDoctor
                        (|owl|:|Restriction| (|owl|:|onProperty| hasChild)
                                         (|owl|:|cardinality| 1))))

(subsumed-p ParentOfOneChildDoctorAndEmployee ParentOfDoctorAsEmployee)

(defConcept ParentOfTwoChildDoctorAndEmployee
    (|owl|:|intersectionOf| ParentOfEmployee ParentOfDoctor
                        (|owl|:|Restriction| (|owl|:|onProperty| hasChild)
                                         (|owl|:|cardinality| 2))))

(subsumed-p ParentOfTwoChildDoctorAndEmployee ParentOfDoctorAsEmployee)
(subsumed-p ParentOfTwoChildDoctorAndEmployee IntersectOfParentsOfEmployeeAndParentsOfDoctor)
(subsumed-p ParentOfTwoChildDoctorAndEmployee ParentOfDoctor)
(subsumed-p ParentOfTwoChildDoctorAndEmployee ParentOfEmployee)

|#

(defun allvalues-subsumed-p (cr dr)
  (let ((drval (slot-value dr '|owl|:|allValuesFrom|)))
    (etypecase cr
      (|owl|:|allValuesFromRestriction|
       (subsumed-p (slot-value cr '|owl|:|allValuesFrom|) drval))
      (|owl|:|hasValueRestriction| (typep (slot-value cr '|owl|:|hasValue|) drval))
      (|owl|:|someValuesFromRestriction|
       ;; #<Å‡•üwn30schema:containsWordSense.wn30schema:AdjectiveWordSense> vs.
       ;; #<Å‡§Øwn30schema:containsWordSense.wn30schema:AdjectiveWordSense>
       (subsumed-p (slot-value cr '|owl|:|someValuesFrom|) drval))
      (|owl|:|cardinalityRestriction| nil))))

(defun somevalues-subsumed-p (cr dr)
  (let ((drval (slot-value dr '|owl|:|someValuesFrom|)))
    (etypecase cr
      (|owl|:|hasValueRestriction|
       (typep (slot-value cr '|owl|:|hasValue|) drval))
      (|owl|:|allValuesFromRestriction|
       ;; #<Å‡§Øwn30schema:containsWordSense.wn30schema:AdjectiveWordSense> vs.
       ;; #<Å‡•üwn30schema:containsWordSense.wn30schema:AdjectiveWordSense>
       ;; #<Å‡§ØhasChild.Doctor> vs.
       ;; #<Å‡•ühasChild.Doctor>
       (subsumed-p (slot-value cr '|owl|:|allValuesFrom|) drval))
      (|owl|:|someValuesFromRestriction|
       (subsumed-p (slot-value cr '|owl|:|someValuesFrom|) drval))
      (|owl|:|cardinalityRestriction| nil))))

(defun hasvalue-subsumed-p (cr dr)
  (etypecase cr
    (|owl|:|hasValueRestriction|
     (subsumed-p (slot-value cr '|owl|:|hasValue|) (slot-value dr '|owl|:|hasValue|)))
    (|owl|:|allValuesFromRestriction| nil)
    (|owl|:|someValuesFromRestriction| nil)
    (|owl|:|cardinalityRestriction| nil)))

(defun cardinality-subsumed-p (cr dr)
  (etypecase cr
    (|owl|:|hasValueRestriction| nil)
    (|owl|:|allValuesFromRestriction| nil)
    (|owl|:|someValuesFromRestriction| nil)
    (|owl|:|cardinalityRestriction|
     (let ((maxcr
            (cond ((slot-boundp cr '|owl|:|cardinality|)
                   (slot-value cr '|owl|:|cardinality|))
                  ((slot-boundp cr '|owl|:|maxCardinality|)
                   (slot-value cr '|owl|:|maxCardinality|))))
           (maxdr
            (cond ((slot-boundp dr '|owl|:|cardinality|)
                   (slot-value dr '|owl|:|cardinality|))
                  ((slot-boundp dr '|owl|:|maxCardinality|)
                   (slot-value dr '|owl|:|maxCardinality|))))
           (mincr
            (cond ((slot-boundp cr '|owl|:|cardinality|)
                   (slot-value cr '|owl|:|cardinality|))
                  ((slot-boundp cr '|owl|:|minCardinality|)
                   (slot-value cr '|owl|:|minCardinality|))))
           (mindr
            (cond ((slot-boundp dr '|owl|:|cardinality|)
                   (slot-value dr '|owl|:|cardinality|))
                  ((slot-boundp dr '|owl|:|minCardinality|)
                   (slot-value dr '|owl|:|minCardinality|)))))
       ;(format t "~%[~S ~S] < [~S ~S]"
       ;  (or mincr :INF) (or maxcr :INF) (or mindr :INF) (or maxdr :INF))
       (and (or (null mindr) (null mincr)
                (>= (value-of mincr) (value-of mindr)))
            (or (null maxdr) (null maxcr)
                (<= (value-of maxcr) (value-of maxdr))))))))

(defun restriction-subsumed-p (cr dr)
  "This predicate checks restrictions lexically without ABox."
  (and (equivalent-property-p
        (onproperty-of dr) (onproperty-of cr))
       (etypecase dr
         (|owl|:|hasValueRestriction| (hasvalue-subsumed-p cr dr))
         (|owl|:|allValuesFromRestriction| (allvalues-subsumed-p cr dr))
         (|owl|:|someValuesFromRestriction| (somevalues-subsumed-p cr dr))
         (|owl|:|cardinalityRestriction| (cardinality-subsumed-p cr dr)))))

(defun %oneof1-subsumed-p (c d)
  ;; recursively loop while c is an oneof.
  (cond ((not (owl-oneof-p c)) (subsumed-p c d))
        ((%clos-subtype-p c d))
        ((every #'(lambda (ci) (typep ci d)) (slot-value c '|owl|:|oneOf|)))
        (t (some #'(lambda (csuper) (%oneof1-subsumed-p csuper d))
                 (mop:class-direct-superclasses c)))))

(defun %oneof2-subsumed-p (c d)
  ;; c is not an oneof but d is an oneof, seldom happen.
  (declare (ignore c d))
  nil)

(defun oneof-subsumed-p (c d)
  (if (subsetp (slot-value c '|owl|:|oneOf|) (slot-value d '|owl|:|oneOf|) :test #'%owl-same-p)
      (values t t)
    (values nil t)))



(defun most-specific-concepts-for-refining (classes)
  "This function is used for class refining for an instance. So, we treat only rigid classes.
   <classes> must be clos objects."
  (let ((l (remove-duplicates classes
                              :test #'(lambda (x y)
                                        (cond ((eql x y))
                                              ((and (not (class-name x)) (not (class-name y)))
                                               (owl-equalp-for-refining x y)))))))
    (set-difference l l :test #'clos-strict-supertype-p)))
;; (typep vin:StGenevieveTexasWhite vin:WhiteNonSweetWine) -> tt
;; (typep vin:StGenevieveTexasWhite vin:DryWhiteWine)      -> tt
;; However,
;; (clos-strict-supertype-p vin:DryWhiteWine vin:WhiteNonSweetWine) -> nil

(defun collect-most-specific-concepts (class instance)
  "This function is used for shared-initialize."
  (let ((subs (mop:class-direct-subclasses class)))
    (setq subs     ; same as |owl|:|Thing|
          (remove-if #'(lambda (sub) (eql (complement-of sub) |owl|:|Nothing|)) subs))
    (setq subs (remove |owl|:|Nothing| subs))
    (setq subs (remove-if #'owl-oneof-p subs))
    (setq subs (remove-if-not #'(lambda (sub) (%typep-for-MSCs instance sub)) subs))
    (cond ((null subs) (list class))
          (t (most-specific-concepts-for-refining
              (loop for sub in subs
                  append (collect-most-specific-concepts sub instance)))))))

;;.............................................................................................
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;

(defun get-intersect-siblings (class intersections)
  "returns a set of subclasses of all member of <intersections>, removing duplicates and
   ones that has same intersection as <class>."
  ;; %intersection-equivalent removes myself and the structural equal objects,
  ;; e.g., DryWine vs. TableWine
  (remove-if #'(lambda (sib) (%intersection-equivalent sib class))
             (remove-duplicates
              (mappend #'mop:class-direct-subclasses intersections))))

(defun get-union-spouses (class unions)
  "returns a set of superclases of all member of <unions>, removing duplicates and
  ones that has same union as <class>."
  (remove-if #'(lambda (sp) (%union-equivalent sp class))
             ;; %union-equivalent remove myself and the structuralequal objects on union members
             (remove-duplicates
              ;; remove-if-not is necessary for (|owl|:|unionOf| |owl|:|Nothing| (|owl|:|Class| (|owl|:|complementOf| |owl|:|Nothing|)))
              (remove-if-not #'owl-class-p
                             (mappend #'mop:class-direct-superclasses unions)))))

(defun get-superclasses-siblings (class superclasses)
  (remove-if #'(lambda (sib) (and (owl-class-p sib)
                                  (owl-class-p class)
                                  (%intersection-equivalent sib class)))
             (remove class
                     (remove-duplicates
                      (mappend #'mop:class-direct-subclasses superclasses)))))

(defun get-subclasses-siblings (class subclasses)
  (remove-if #'(lambda (sib) (and (owl-class-p sib)
                                  (owl-class-p class)
                                  (%union-equivalent sib class)))
             (remove class
                     (remove-duplicates
                      (mappend #'mop:class-direct-superclasses subclasses)))))

;; refine-sibling-from-intersection causes DryWine subtype TableWine and
;; TableWine subtype DryWine.
;; the calculation does not terminate.
(defun subtypep-for-refining (c1 c2)
  (or (%clos-subtype-p c1 c2)
      (%intersection-refining-p c1 c2)
      (%union-refining-p c1 c2)
      (%oneof-refining-p c1 c2)
      (%complement-refining-p c1 c2)))

(defun %intersection-refining-p (c1 c2)
  (let ((intersections nil))
    (when (and (owl-class-p c2) (setq intersections (intersection-of c2)))
      (let ((cpl (remove c1
                         (cond ((mop:class-finalized-p c1)
                                (clos:class-precedence-list c1))
                               (t (mop:compute-class-precedence-list c1)))))
            (c2supers (remove-if #'owl-restriction-p intersections))
            (c2restrictions (remove-if-not #'owl-restriction-p intersections)))
        (unless (and c2supers
                     (every #'(lambda (c2super)
                                (some #'(lambda (c1super)
                                          (subtypep-for-refining c1super c2super))
                                      (remove-if #'owl-restriction-p cpl)))
                            c2supers))
          (return-from %intersection-refining-p nil))
        ;; when c2supers=null or subtyed for c2supers
        (cond ((null c2restrictions)
               (return-from %intersection-refining-p t))
              ((every #'(lambda (c2restriction)
                          (some #'(lambda (c1restriction)
                                    (restriction-subtypep c1restriction c2restriction))
                                (remove-if-not #'owl-restriction-p cpl)))
                      c2restrictions)))))))

(defun %union-refining-p (c1 c2)
  (let ((unions nil))
    (when (and (owl-class-p c2) (setq unions (union-of c2)))
      (cond ((some #'(lambda (u) (cl:subtypep c1 u))
                   unions)
             (error "Not Yet!"))
            (t nil))
      )))

(defun %oneof-refining-p (c1 c2)
  (when (and (owl-oneof-p c1) (owl-oneof-p c2))
    (subsetp (slot-value c1 '|owl|:|oneOf|) (slot-value c2 '|owl|:|oneOf|) :test #'%owl-same-p)))

(defun %complement-refining-p (c1 c2)
  (let ((complement1 ())
        (complement2 ()))
    (when (and (owl-class-p c1) (setq complement1 (complement-of c1))
               (owl-class-p c2) (setq complement2 (complement-of c2)))
      (subtypep-for-refining complement2 complement1))))

;;;
;;;; |owl|:|complementOf|
;;;

(defun shared-initialize-after-for-complementOf (class complements)
  (loop for complement in complements with result
      unless (eql class |owl|:|Nothing|)  ; |owl|:|Nothing| is complement to any object.
      do (cond ((setq result (check-instance-sharing class complement))
                (error 'complementof-condition-unsatiafiable
                  :format-control "~S has super-sub relation ~S."
                  :format-arguments `(,(class-name class) ,(class-name result))))
               (t (cond ((slot-boundp class 'complement-class)
                         (cond ((subtypep complement (slot-value class 'complement-class))
                                (setf (slot-value class 'complement-class) complement))
                               ((error "Monotonicity Violation for ~S:complementOf=~S <- ~S"
                                  class (slot-value class 'complement-class) complement))))
                        (t (setf (slot-value class 'complement-class) complement)))  ; rule5
                  (cond ((slot-boundp complement 'complement-class)
                         (cond ((subtypep class (slot-value complement 'complement-class))
                                (setf (slot-value complement 'complement-class) class))
                               ((error "Monotonicity Violation for ~S:complementOf=~S <- ~S"
                                  complement (slot-value complement 'complement-class) class))))
                        (t (setf (slot-value complement 'complement-class) class)))  ; rule5
                  (pushnew complement (slot-value class 'disjoint-classes))          ; rule6
                  (pushnew class (slot-value complement 'disjoint-classes))))))      ; rule6

;;;
;;;; intersectionOf
;;;

;; ensure for intersections to be OWL classes. Most of work is done at ensure-meta-absts-using-class
(defun shared-initialize-after-for-intersectionOf (class intersections)
  (loop for super in (remove-if #'owl-restriction-p intersections)
      unless (owl-class-p super)
      do (warn "Change class by intersectionOf:~S |rdf|:|type| |owl|:|Class|" (name super))
        (change-class super (load-time-value |owl|:|Class|)))
  (let ((siblings (get-intersect-siblings class intersections)))
    ;; class and its structural equivalents are removed from siblings
    (loop for sib in siblings
        do (cond ((subsetp intersections (mop:class-direct-superclasses sib))
                  (let ((new-supers (refine-concept-by-intersection
                                     (most-specific-concepts-by-superclasses
                                      (cons class (mop:class-direct-superclasses sib))))))
                    ;(format t "~%Old1:~S" (mop:class-direct-superclasses sib))
                    ;(format t "~%New1:~S" new-supers)
                    (warn "~S is refined to a subclass of ~S by defining ~S's intersection."
                      sib class class)
                    (reinitialize-instance sib :direct-superclasses new-supers)))
                 ((every #'(lambda (sup) (cl:subtypep sib sup)) intersections)
                  (let ((new-supers (refine-concept-by-intersection
                                     (most-specific-concepts-by-superclasses
                                      (cons sib (mop:class-direct-superclasses class))))))
                    ;(format t "~%Old2:~S" (mop:class-direct-superclasses sib))
                    ;(format t "~%New2:~S" new-supers)
                    (warn "~S is refined to a subclass of ~S by defining ~S's intersection."
                      class sib class)
                    (reinitialize-instance class :direct-superclasses new-supers)))))))

(defun intersection-of (class)
  (and (slot-exists-p class '|owl|:|intersectionOf|)
       (slot-boundp class '|owl|:|intersectionOf|)
       (slot-value class '|owl|:|intersectionOf|)))

;; rule14
(defun check-intersection-refining-for-subclasses (class superclasses)
  (let ((old-supers (mop:class-direct-superclasses class))
        (siblings (get-superclasses-siblings class superclasses)))
    (loop for sib in siblings
        unless (member sib old-supers)
        do (when (subtypep-for-refining class sib)
             (let ((new-supers
                    (refine-concept-by-intersection
                     (most-specific-concepts-by-superclasses (cons sib old-supers)))))
               (unless (set-equalp old-supers new-supers)
                 (warn "~S is refined to subclass of ~S." class sib)
                 (reinitialize-instance class :direct-superclasses new-supers)))))))

(defun refine-concept-by-intersection (classes)
  "returns most specific concepts that include subs of <classes> as intersection's sub.
   Note that return values include initial values and newly added values."
  (cond ((null (cdr classes)) classes)
        (t (mapc #'(lambda (class)
                     (mapc #'(lambda (sub)
                               (cond ((and (cl:typep sub |owl|:|Class|)
                                           (intersection-of sub)
                                           (every #'(lambda (inte) (member inte classes))
                                                  (intersection-of sub)))
                                      ;; then refining
                                      (return-from refine-concept-by-intersection
                                        (refine-concept-by-intersection
                                         (most-specific-concepts-by-superclasses
                                          (cons sub classes)))))
                                     )) ; otherwise finally returns classes for mapc
                       (mop:class-direct-subclasses class)))
             classes))))

;;;
;;;; unionOf Shared Initialize After Action
;;; After defining a class with |owl|:|unionOf| slot,
;;; # If the defined class <class> is a subclass of a superclass <sib> of a union member,
;;;   <class> is inserted between the superclass <sib> and the union member.
;;; # If the defined class <class> is not a subclass of a superclass <sib> of a union member,
;;;   <class> is a sibling of <sib>.

(defun shared-initialize-after-for-unionOf (class unions)
  ;(format t "~%SHARED-INITIALIZE-AFTER-FOR-UNIONOF(~S ~S)" class unions)
  (loop for sub in (remove-if #'owl-restriction-p unions)
      unless (owl-class-p sub)
      do (warn "Change class by unionOf:~S |rdf|:|type| |owl|:|Class|" (name sub))
        (change-class sub (load-time-value (find-class '|owl|:|Class|))))
  ;; reinitialize supers of unions
  (loop for sub in (remove-if #'owl-restriction-p unions)
      unless (cl:subtypep sub class)
      do (reinitialize-instance
          sub
          :direct-superclasses (most-specific-concepts-by-clos-supers
                                (cons class (mop:class-direct-superclasses sub)))))
  (let ((spouses (remove-if #'owl-restriction-p (get-union-spouses class unions))))
    ;; class and its rdf equivalents are removed from spouses
    (when spouses
      (let ((old-supers (mop:class-direct-superclasses class))
            (add-supers (remove-if-not #'(lambda (sp) (%union-subsumed-p class sp)) spouses)))
        (when add-supers
          ;(format t "~%Adding-supers-for-union:~S" add-supers)
          (let ((class-supers
                 (most-specific-concepts-by-clos-supers (append add-supers old-supers))))
            (unless (set-equalp class-supers old-supers)
              ;(format t "~%Supers of union class ~S: ~S -> ~S" class old-supers class-supers)
              (reinitialize-instance class :direct-superclasses class-supers))))))))

(defun check-union-refining-for-subclasses (class subclasses)
  (let ((old-supers (mop:class-direct-superclasses class))
        (siblings (get-subclasses-siblings class subclasses)))
    (loop for sib in siblings
        unless (member sib old-supers)
        do (when (subtypep-for-refining class sib)
             (let ((new-supers (most-specific-concepts (cons sib old-supers))))
               (unless (set-equalp old-supers new-supers)
                 (warn "~S is refined to subclass of ~S." class sib)
                 (reinitialize-instance class :direct-superclasses new-supers)))))))

(defmethod change-class :before ((instance |owl|:|Thing|) (new-class (eql |owl|:|Class|))
                                 &rest initargs)
  (declare (ignore initargs))
  (when (disjoint-p (class-of instance) new-class)
    (error 'disjointwith-condition-unsatiafiable
      :format-control "between ~S and ~S."
      :format-arguments `(,instance ,(class-name new-class)))))

(defmethod change-class :before ((instance |owl|:|Thing|) (new-class |owl|:|Class|)
                                 &rest initargs)
  (declare (ignore initargs))
  (when (disjoint-p (class-of instance) new-class)
    (error 'disjointwith-condition-unsatiafiable
      :format-control "between ~S and ~S."
      :format-arguments `(,instance ,(class-name new-class)))))

;;;
;;;; Shared-initialize Before for |owl|:|Class|
;;;

(defmethod shared-initialize :before ((class |owl|:|Class|) slot-names &rest initargs)
  (when (and (null slot-names) (not (null initargs))) ; reinitialize
    (loop for (role newval) on initargs by #'cddr with oldval
        when (and (or (eq role '|owl|:|intersectionOf|) (eq role '|owl|:|unionOf|))
                  (slot-exists-p class role)
                  (slot-boundp class role)
                  (setq oldval (slot-value class role))
                  ;; not set equal
                  (not (and (subsetp oldval newval :test #'owl-same-p)
                            (subsetp newval oldval :test #'owl-same-p))))
        do (warn "~S value ~S of ~S is doubly specified to ~S." role oldval class newval))))

;;;
;;;; Shared-initialize After for |owl|:|Class|
;;;

;;; <update-instance-for-different-class> is called in <change-class>.
;;; Usually <change-class> is invoked by <ensure-class> without <initargs>.
;;; So, direct-superclasses is set by <reinitialize-instance> in <ensure-class>
;;; after <change-class>. The following routine is prepared only for direct
;;; invocation of <change-class> by users.
(defmethod update-instance-for-different-class ((previous |rdfs|:|Class|)
                                                (current |owl|:|Class|)
                                                &rest initargs)
  "ensures |owl|:|Thing| as superclasses in OWL universe. This routine is effective
   only if a user designates :direct-superclasses but no inheritance of |owl|:|Thing|."
  (cond ((and (not (cl:typep previous |owl|:|Class|))     ; _rdfsClass to |owl|:|Class|
              (getf initargs :direct-superclasses)    ; if supers but no |owl|:|Thing|
              (some #'(lambda (obj) (cl:subtypep obj |owl|:|Thing|))
                    (getf initargs :direct-superclasses)))
         (setf (getf initargs :direct-superclasses)
           (cons |owl|:|Thing| (getf initargs :direct-superclasses)))
         (apply #'call-next-method previous current initargs))
        (t (call-next-method))))
#|
(defmethod update-instance-for-redefined-class ((instance |owl|:|Class|)
                                                added-slots
                                                discarded-slots
                                                property-list
                                                &rest initargs)
  ;(format t "~%UPDATE-INSTANCE-FOR-REDEFINED-CLASS ~S ~S ~S ~S ~S"
  ;  instance added-slots discarded-slots property-list initargs)
  (call-next-method)
  )
|#
(defmethod shared-initialize :after ((class |owl|:|Class|) slot-names &rest initargs)
  (cond ((and (null slot-names) (null initargs))  ; when metaclass changed
         )
        ((and (consp slot-names) (null initargs)) ; when metaclass redefined, propagated
         )
        (t ;; first or redefinition
         (with-slots (|rdfs|:|subClassOf| |owl|:|intersectionOf| |owl|:|unionOf| |owl|:|equivalentClass|
                                      |owl|:|disjointWith| |owl|:|complementOf|)
             class
           ;; see also ensure-meta-absts-using-class class for subclasses and intersections
           (when (and (slot-boundp class '|rdfs|:|subClassOf|) |rdfs|:|subClassOf|)
             (check-intersection-refining-for-subclasses class (mklist |rdfs|:|subClassOf|))
             (when (mop:class-direct-subclasses class)
               (check-union-refining-for-subclasses class (mop:class-direct-subclasses class))))
           (when (and (slot-boundp class '|owl|:|intersectionOf|) |owl|:|intersectionOf|)
             (shared-initialize-after-for-intersectionOf class |owl|:|intersectionOf|))
           (when (and (slot-boundp class '|owl|:|unionOf|) |owl|:|unionOf|)
             (unless (eq class |owl|:|Thing|)
               (shared-initialize-after-for-unionOf class |owl|:|unionOf|)))
           (when (and (slot-boundp class '|owl|:|equivalentClass|) |owl|:|equivalentClass|)
             (shared-initialize-after-for-equivalentClass class (mklist |owl|:|equivalentClass|)))
           (when (and (slot-boundp class '|owl|:|disjointWith|) |owl|:|disjointWith|)
             (shared-initialize-after-for-disjointWith class (mklist |owl|:|disjointWith|)))
           (when (and (slot-boundp class '|owl|:|complementOf|) |owl|:|complementOf|)
             (shared-initialize-after-for-complementOf class (mklist |owl|:|complementOf|))))
         (unless (owl-thing-p class)
           (reinitialize-instance
            class
            :direct-superclasses (list |owl|:|Thing| (mop:class-direct-superclasses class))))
         )))

;;;
;;;; sameAs
;;;

;; slot same-as includes same individuals including itself against instance.
(defun shared-initialize-after-for-sameAs (instance sameindividuals)
  (let ((oldsames (slot-value instance 'same-as))
        (newsames sameindividuals))
    (let ((sames (adjoin instance (union oldsames newsames)))
          (diffs ()))
      (cond ((setq diffs (intersection sames sames :test #'owl-different-p))
             (error 'sameas-condition-unsatiafiable
               :format-control "Different pairs are found in ~S."
               :format-arguments `(,diffs)))
            (t (mapc #'(lambda (s) (when (owl-thing-p s)
                                     (setf (slot-value s 'same-as) sames)))
                 sames))))))

(defun sames-p (instance sames)
  (some #'(lambda (same) (owl-same-p instance same)) sames))

(defun get-sames (x)
  (append (same-as-of x)
          (%get-functional-property-sames x)
          (%get-inverse-functional-property-sames x)))

;;;
;;;; differentFrom
;;;

;; slot different-from includes different individuals against instance.
(defun shared-initialize-after-for-differentFrom (instance differents)
  (let ((same (find instance differents :test #'%owl-same-p)))
    (cond (same (error 'differentfrom-condition-unsatiafiable
                  :format-control "~S is same as ~S."
                  :format-arguments `(,instance ,same)))
          (t (setf (slot-value instance 'different-from)
               (union (slot-value instance 'different-from) differents))
             (loop for dif in differents
                 do (pushnew instance (slot-value dif 'different-from))))))) ; rule13

;;;
;;;; owl-different-p
;;;

(defun different-from-of (y)
  (and (slot-exists-p y 'different-from)
       (slot-boundp y 'different-from)
       (slot-value y 'different-from)))

(defun %owl-different-p (x y)
  (cond ((equal x y) (values nil t))
        ((and (numberp x) (numberp y))
         (if (= x y) (values nil t) (values t t)))
        ((and (datatype-p (class-of x)) (datatype-p (class-of y)))
         (if (eq (class-of x) (class-of y))
             (if (equalp (value-of x) (value-of y))    ; equalp treats 1 and 1.0
                 (values nil t)
               (values t t))
           (values t t)))
        ((and (owl-thing-p x) (owl-thing-p y))
         (cond ((member x (slot-value y 'different-from))
                (values t t))
               ((and (name x) (name y))
                (cond (*nonUNA*
                       (if (eql (name x) (name y)) (values nil t) (values t t)))
                      (t (multiple-value-bind (result graph) (rdf-graph-equalp x y)
                           (if graph (values (not result) graph) (values nil nil))))))
               (t (multiple-value-bind (result graph) (rdf-graph-equalp x y)
                    (if graph (values (not result) graph) (values nil nil))))))
        ((and (typep x |rdfs|:|Literal|) (typep y |rdfs|:|Literal|))
         ;; fall here when not equal
         (values t t))
        (t (values nil nil))))

(defun owl-different-p (x y)
  "Is <x> different from <y> as individual in semantics of OWL?"
  ;; not same does not imply difference in open world.
  (declare (optimize (speed 3) (safety 0)))
  (cond ((equal x y) (values nil t))
        ((and (numberp x) (numberp y))
         (if (= x y) (values nil t) (values t t)))
        ((and (datatype-p (class-of x)) (datatype-p (class-of y)))
         (if (eq (class-of x) (class-of y))
             (if (equalp (value-of x) (value-of y))    ; equalp treats 1 and 1.0
                 (values nil t)
               (values t t))
           (values t t)))
        ((and (property-p x) (property-p y) (member x (equivalent-property-of y)))
         (values nil t))
        ((and (owl-restriction-p x) (owl-restriction-p y))
         (if (%owl-restriction-equal x y)
             (values nil t)
           (values t t)))
        ((and (owl-oneof-p x) (owl-oneof-p y))
         (if (%oneof-equal (slot-value x '|owl|:|oneOf|) (slot-value y '|owl|:|oneOf|))
             (values nil t) (values nil nil)))
        ((and (owl-class-p x) (owl-class-p y)
              (or (%owl-equivalent-p x y)
                  (%intersection-equivalent x y)
                  (%union-equivalent x y)
                  (%complemently-equal x y)))
         (values nil t))
        ((and (owl-thing-p x) (owl-thing-p y))
         (cond ((member x (slot-value y 'different-from))
                (values t t))
               ((member x (append (mklist (and (slot-boundp y '|owl|:|sameAs|)
                                               (slot-value y '|owl|:|sameAs|)))
                                     (%same-as-of y)) :test #'owl-equalp)
                (values nil t))
               ((member y (append (mklist (and (slot-boundp x '|owl|:|sameAs|)
                                               (slot-value x '|owl|:|sameAs|)))
                                     (%same-as-of x)) :test #'owl-equalp)
                (values nil t))
               ((and (name x) (name y))
                (cond (*nonUNA* (rdf-graph-different-p x y))
                      (t (if (eql (name x) (name y)) (values nil t) (values t t)))))
               (t (rdf-graph-different-p x y))))
        ;;
        ((and (typep x |rdfs|:|Literal|) (typep y |rdfs|:|Literal|))
         (values t t))
        ((and (rsc-object-p x) (rsc-object-p y))
         (cond ((and (name x) (name y))
                (cond (*nonUNA* (rdf-graph-different-p x y))
                      (t (if (eql (name x) (name y)) (values nil t) (values t t)))))
               (t (rdf-graph-different-p x y))))
        ((and (symbolp x) (symbolp y))
         (cond ((and (object? x) (object? y))
                (%owl-different-p (symbol-value x) (symbol-value y)))
               (t (values nil nil))))
        ((and (symbolp y) (object? y))
         (%owl-different-p x (symbol-value y)))
        ((and (symbolp x) (object? x))
         (%owl-different-p (symbol-value x) y))
        ((and (uri-p x) (uri-p y))
         (cond ((uri= x y) (values nil t))
               (t (%owl-different-p (uri-value x) (uri-value y)))))
        ((uri-p y)
         (owl-different-p x (uri-value y)))
        ((uri-p x)
         (owl-different-p (uri-value x) y))
        (t (values nil nil))))

;;;
;;;; Symmetric Property
;;;
(excl:without-redefinition-warnings
(defun symmetric-property-p (obj)
  "Is this <obj> an instance of |owl|:|SymmetricProperty|?"
  ;;this is same as '(cl:typep <obj> |owl|:|SymmetricProperty|)'
  (and (excl::standard-instance-p obj)
       (let ((class (class-of obj)))
         (cond ((eq class (load-time-value |owl|:|SymmetricProperty|)))
               ((mop:class-finalized-p class)
                (and (member (load-time-value |owl|:|SymmetricProperty|)
                                (mop:class-precedence-list class)
                                :test #'eq)
                     t))
               ((labels ((walk-partial-cpl (c)
                                           (let ((supers (mop:class-direct-superclasses c)))
                                             (when (member
                                                    (load-time-value |owl|:|SymmetricProperty|)
                                                    supers :test #'eq)
                                               (return-from symmetric-property-p t))
                                             (mapc #'walk-partial-cpl supers))))
                  (declare (dynamic-extent #'walk-partial-cpl))
                  (walk-partial-cpl class)
                  nil))))))
)

(excl:without-redefinition-warnings
(defmethod shared-initialize :after ((instance |rdfs|:|Resource|) slot-names &rest initargs)
  "book-keeping for reification"
  (let ((args (copy-list initargs)))
    (let ((changed (remf args :direct-slots)))
      (setq changed (or (remf args :direct-superclasses) changed))
      (cond ((and (null slot-names) (null args))  ; when change-class
             )
            ((and (consp slot-names) (null args)) ; when propagated
             )
            (t                                        ; first or redefinition
             (typecase instance
               (|rdfs|:|Literal| nil)
               (|rdfs|:|Datatype| nil)
               (|rdf|:|Statement| nil)
               (|rdf|:|List| nil)
               (t
                (apply #'book-keeping-for-reification instance slot-names args)
                (let ((name (getf args :name)))
                  (when name
                    (when (nodeID? name)
                      (setf (slot-value instance 'excl::name) nil))
                    (export-as-QName name)
                    (setf (symbol-value name) instance)))

                ;; functional property registers its inverse.
                ;; Then, vin:Delicate has ((vin:hasFlavor vin:WhitehallLanePrimavera) ... )
                (loop for funprop in (collect-owl-role-name-if #'functional-property-p instance)
                    as funprop-val = (and (slot-boundp instance funprop) (slot-value instance funprop))
                    when funprop-val
                    do (loop for val in (mklist funprop-val)
                           ;when (owl-thing-p val)
                           do (pushnew `(,funprop ,instance) (slot-value val 'funprop-inverse)
                                       :test #'equal)))
                ;; rdfp1 ter Horst is deduced by reasoning.

                ;; inverse functional property also registers its inverse.
                (loop for inv-funprop in (collect-owl-role-name-if #'inverse-functional-property-p instance)
                    as inv-funprop-val = (and (slot-boundp instance inv-funprop) (slot-value instance inv-funprop))
                    when inv-funprop-val
                    do (loop for val in (mklist inv-funprop-val)
                           ;when (owl-thing-p val)
                           do (pushnew `(,inv-funprop ,instance) (slot-value val 'inverse-funprop-inverse)
                                       :test #'equal)))
                ;; symmetric property adds the subject to the object's slot. See rdfp3.
                (loop for symprop in (collect-owl-role-name-if #'symmetric-property-p instance) with symprop-val
                    when (setq symprop-val
                               (and (slot-boundp instance symprop) (slot-value instance symprop)))
                    do
                      (loop for val in (mklist symprop-val)
                           do (setf (slot-value val symprop) instance)))

                ;; transitive property registers its inverse. See rdfp4.
                (loop for trans-prop in (collect-owl-role-name-if #'transitive-property-p instance) with inv-transitive
                    when (setq inv-transitive
                               (and (slot-boundp instance trans-prop) (slot-value instance trans-prop)))
                    do (loop for invt in (mklist inv-transitive)
                           do (reinitialize-instance invt :inverse-transitive `(,trans-prop ,instance))))
                )))))))
)

;;;
;;;; Shared-initialize After for |owl|:|Thing|
;;;

(defmethod shared-initialize :after ((instance |owl|:|Thing|) slot-names &rest initargs)
  (cond ((and (null slot-names) (null initargs))  ; when class changed
         )
        ((and (consp slot-names) (null initargs)) ; when class redefined, propagated
         )
        (t                                        ; first or redefinition
         (cond ((eq slot-names t)
                ;(format t "~%SHARED-INITIALIZE:AFTER(|owl|:|Thing|) first definition ~S~%  with ~S)" instance initargs)
                )
               (t ;(format t "~%SHARED-INITIALIZE:AFTER(|owl|:|Thing|) redefining ~S~%  slots ~S~%  with ~S)" instance slot-names initargs)
                ))
         (with-slots (|owl|:|sameAs| |owl|:|differentFrom|) instance
           ;; |owl|:|sameAs| makes sameAs groups among individuals.
           (when (slot-boundp instance '|owl|:|sameAs|)
             (shared-initialize-after-for-sameAs instance (mklist |owl|:|sameAs|)))

           ;; |owl|:|differentFrom| makes pairwise different groups among individuals.
           (when (slot-boundp instance '|owl|:|differentFrom|)
             (shared-initialize-after-for-differentFrom instance (mklist |owl|:|differentFrom|))))

         ;; functional property is moved to shared-initialize:after(|rdfs|:|Resource|)
         ;; inverse functional property is moved to shared-initialize:after(|rdfs|:|Resource|)
         ;; symmetric property is moved to shared-initialize:after(|rdfs|:|Resource|)
         ;; transitive property is moved to shared-initialize:after(|rdfs|:|Resource|)

         ;; satisfiability check for oneOf individual
         (let ((oneof (find-if #'owl-oneof-p  (mop:class-precedence-list (class-of instance)))))
           (when oneof
             ;(format t "~%SHARED-INITIALIZE :AFTER ~S ~S ~S" instance slot-names initargs)
             ;(format t "~%CLASS = ~S" (class-of instance))
             ;(format t "~%MCLASSES = ~S" (mclasses instance))
             (let ((ones (slot-value oneof '|owl|:|oneOf|)))
               (cond ((member instance ones :test #'%owl-same-p) nil) ; nothing done
                     (t (let ((others (remove instance ones :test #'owl-different-p)))
                          (cond ((null others) (error 'oneof-condition-unsatiafiable
                                                 :format-control "~S for ~S of ~S"
                                                 :format-arguments `(,instance ,ones ,oneof)))
                                (t (warn "~S should be one of ~S." instance others)))))))
             ))

         ;; if oneof exists in classes, check its type.
         #|
         (let ((oneofclasses (remove-if-not #'(lambda (x) (cl:typep x 'OneOf)) (mclasses instance)))
               (notoneofclasses (remove-if #'(lambda (x) (cl:typep x 'OneOf)) (mclasses instance))))
           (cond ((and oneofclasses (null notoneofclasses))
                  (let ((newMSCs (set-difference oneofclasses oneofclasses
                                                 :test #'(lambda (subsumer subsumee)
                                                           (and (not (eql subsumer subsumee))
                                                                (subsumed-p subsumee subsumer))))))
                    (when newMSCs
                      ;(format t "~%newMSCs:~S" newMSCs)
                      (unless (set-equalp newMSCs oneofclasses)
                        (let ((notMSCs (set-difference oneofclasses newMSCs)))
                          (loop for new in newMSCs
                              do (reinitialize-instance new :direct-superclasses notMSCs)))
                        (cond ((null (cdr newMSCs))
                               ;(format t "~%Unshadowing for ~S to ~S" instance (car newMSCs))
                               (change-class instance (car newMSCs)))
                              ((error "Not Yet!"))) ))))
                 ((null (cdr notoneofclasses))
                  (loop for oneofclass in oneofclasses with super = (car notoneofclasses)
                      when (every #'(lambda (one) (cl:typep one super))
                                  (slot-value oneofclass '|owl|:|oneOf|))
                      do (unless (cl:subtypep oneofclass super)
                           (reinitialize-instance oneofclass :direct-superclasses `(,super)))))))
         |#
         ;; refine instance in OWL
         (typecase instance
           (|owl|:|Restriction| nil)
           (|owl|:|Class|
            (when (not (eql (class-of instance) |owl|:|Class|))
              (let ((*autoepistemic-local-closed-world* nil))
                (let ((MSCs (collect-most-specific-concepts (class-of instance) instance)))
                  (cond ((null MSCs) (error "Cant happen!"))
                        ((length=1 MSCs)
                         (cond ((eq (car MSCs) (class-of instance)))
                               ((no-twin-p (car MSCs) instance)
                                (assert (not (eq (car MSCs) |owl|:|Nothing|)))
                                (warn "Entailed in refining: ~S to ~S." instance (class-name (car MSCs)))
                                (apply #'change-class instance (car MSCs) initargs))
                               (t (warn "Twin found for ~S at ~S" instance (name (car MSCs)))
                                  ;(destroy instance)
                                  )))
                        (t (warn "~S might be refine with changing class to ~S" instance MSCs)
                           ))))))
           (|owl|:|Thing|
            (when (not (eql (class-of instance) |owl|:|Thing|))
              (let ((*autoepistemic-local-closed-world* nil))
                (let ((MSCs (collect-most-specific-concepts (class-of instance) instance)))
                  (cond ((null MSCs) (error "Cant happen!"))
                        ((length=1 MSCs)
                         (cond ((eq (car MSCs) (class-of instance)))
                               ((no-twin-p (car MSCs) instance)
                                (assert (not (eq (car MSCs) |owl|:|Nothing|)))
                                (warn "Entailed in refining: ~S to ~S." instance (class-name (car MSCs)))
                                (apply #'change-class instance (car MSCs) initargs))
                               (t (warn "Twin found for ~S at ~S" instance (name (car MSCs)))
                                  ;(destroy instance)
                                  )))
                        (t (warn "~S might be refine with changing class to ~S" instance MSCs)
                           ))))))))
        ))

;;; Warning: Entailed in refining: #<vin:WhiteWine vin:StGenevieveTexasWhite>
;;; to vin:WhiteNonSweetWine.

;;; |owl|:|equivalentClass| is a subproperty of |rdfs|:|subClassOf|, so
;;; the objects are automatically captured as subclasses in nature.
;;; However, this characteristics is very tricky and bad for construction of stable ontology.
;;; Because |rdfs|:|subClassOf| is substantial relation, but equivalentClass is temporal one.
;;; We treat equivalentClass in inference instead of subproperty of |rdfs|:|subClassOf|
;;; or superclass-subclass relation.
(excl:without-redefinition-warnings
(defmethod ensure-meta-absts-using-class ((class |rdfs|:|Class|) slot-forms domains)
  "picks up metaclasses and superclasses from <slot-forms>. If |rdf|:|type| slot is included in <slot-forms>,
   the filler supplies meta classes. If |rdfs|:|subClassOf| slot is included, it supplies superclasses (absts).
   In addition, if |owl|:|intersectionOf| slot is included, the filler is added to superclasses (absts)."
  (loop for (role . fillers) in slot-forms
      with metas and absts
      do (cond ((eq role '|rdf|:|type|)
                (setq metas (append fillers metas)))
               ((eql role '|rdfs|:|subClassOf|)
                (setq absts (append fillers absts)))
               ((eql role '|owl|:|intersectionOf|)
                (setq absts (append fillers absts)))
               ((eql role '|owl|:|unionOf|)
                ;; union fillers are not reflected into absts
                )
               ;((eql role '|owl|:|oneOf|)
               ; (cond ((eq class |owl|:|Class|) (pushnew owl::OneOf metas))
               ;       (t (pushnew OneOf metas))))
               )
      finally (return-from ensure-meta-absts-using-class
                (values (refine-concept-by-intersection
                         (most-specific-concepts-by-superclasses
                          (cons class (append metas domains))))
                        absts))))
) ; end of excl:without-redefinition-warnings
;; See rdfskernel
(defmethod make-this-supers ((class |owl|:|Class|) old-supers new-supers)
  "returns an append list of MSCs of restrictions and MSCs of non restrictions in <old-users> and <new-supers>."
  (let ((restrictions (append (remove-if-not #'owl-restriction-p old-supers)
                              (remove-if-not #'owl-restriction-p new-supers)))
        (supers (append (remove-if #'owl-restriction-p old-supers)
                        (remove-if #'owl-restriction-p new-supers))))
    (when restrictions
      (setq restrictions (most-specific-concepts-for-restrictions
                          ;; if same, the old-supers order remains
                          restrictions)))
    (when supers
      (when (and (not (eq class |owl|:|Thing|))
                 (not (member |owl|:|Thing| old-supers)))
        (setq supers (cons |owl|:|Thing| supers)))       ; add default top
      (setq supers (most-specific-concepts-for-refining supers)))
    (append supers restrictions)))

(defun most-specific-concepts-for-restrictions (classes)
  (declare (optimize (speed 3) (safety 0)))
  (when classes
    (flet ((most-specific-concepts-1 (classes)
                                     (let ((l (remove-duplicates classes :test #'%owl-restriction-equal)))
                                       (set-difference l l :test #'strict-abstp-for-restrictions))))
      (let ((answer (most-specific-concepts-1 classes)))
        (assert answer)
        ;(format t "~%most-specific-concepts-1(~S)~% -> ~S" classes answer)
        answer))))

(defun strict-abstp-for-restrictions (abst spec)
  "Is <abst> strictly (not equal to) superclass of <spec>?"
  (cond ((%owl-restriction-equal abst spec) nil)
        ((restriction-subsumed-p spec abst) t)))

;;;
;;;; Transitivity
;;;

(defun transitive-p (obj)
  (cond ((slot-value obj 'inverse-transitive) t)
        ((some #'(lambda (prop) (and (slot-boundp obj prop) (slot-value obj prop)))
               (collect-owl-role-name-if #'transitive-property-p obj)))))

(defun transitive-subp (property sub super &optional visited)
  (or (eql sub super)
      (if (member sub visited) nil
        (strict-transitive-subp property sub super visited))))
(defun strict-transitive-subp (property sub super visited)
  (let ((supers (and (slot-exists-p sub property)
                     (slot-boundp sub property)
                     (slot-value sub property))))
    (unless (listp supers) (setq supers (list supers)))
    (loop for subsSuper in supers
        thereis (transitive-subp property
                                 subsSuper
                                 super
                                 (cons sub visited)))))

(defun strict-transitive-superp (property super sub)
  (and (not (eql super sub)) (strict-transitive-subp property sub super nil)))
(defun strict-include-p (property super sub)
  (and (not (eql super sub)) (strict-transitive-subp property sub super nil)))

(defun most-specific-transitives (property transitives)
  (let ((l (remove-duplicates transitives)))    ; eql should be assured
    (set-difference l l :test #'(lambda (x y) (strict-transitive-superp property x y)))))

;;;
;;; Here after Bookkeeping Facilities
;;;

;;;
;;;; |owl|:|AllDifferent|
;;;

(defmethod shared-initialize :after ((instance |owl|:|AllDifferent|) slot-names &rest initargs)
  (declare (ignore slot-names))
  (let ((distincts (getf initargs '|owl|:|distinctMembers|)))
    (unless (listp distincts) (setq distincts (list distincts)))
    (loop for distinct in distincts
        do (cond ((symbolp distinct)
                  (cond ((object? distinct)
                         (setq distinct (symbol-value distinct))
                         (unless (owl-thing-p distinct)
                           (warn "Change class by distinctMembers: ~S type |owl|:|Thing|." distinct)
                           (change-class distinct '|owl|:|Thing|)))
                        (t (warn "Entail by distinctMembers: ~S type |owl|:|Thing|." distinct)
                           (make-instance '|owl|:|Thing| `(:name ,distinct)))))
                 ((owl-thing-p distinct))
                 (t (warn "Change class by distinctMembers: ~S type |owl|:|Thing|." distinct)
                    (change-class distinct '|owl|:|Thing|)
                    )))
    (loop for distinct1 in distincts
        do (loop for distinct2 in distincts
               unless (eq distinct1 distinct2)
               do (pushnew distinct1 (slot-value distinct2 'different-from))
                 (pushnew distinct2 (slot-value distinct1 'different-from))))))

;;
;;
;;

(defun no-twin-p (class instance)
  (or (anonymous-p instance)
      (every #'(lambda (ins)
                 (or (eq ins instance)
                     (anonymous-p ins)
                     (not (eql (name ins) (name instance)))))
             (class-direct-instances class))))

(defun %symbols2values (lst)
  (mapcar #'(lambda (x) (if (and (symbolp x) (boundp x)) (symbol-value x) x))
    lst))

;;;
;;;; |owl|:|Restriction| again
;;;
(excl:without-redefinition-warnings
(defun owl-restriction-p (obj)
  "Is this <obj> an instance of |owl|:|Restriction|?"
  ;;this is same as '(cl:typep <obj> |owl|:|Restriction|)'
  (let ((class (class-of obj)))
    (cond ((eq class (load-time-value |owl|:|Restriction|)))
          ((not (mop:class-finalized-p class))
           (labels ((walk-partial-cpl (c)
                                      (let ((supers (mop:class-direct-superclasses c)))
                                        (when (member (load-time-value |owl|:|Restriction|)
                                                         supers
                                                         :test #'eq)
                                          (return-from owl-restriction-p t))
                                        (mapc #'walk-partial-cpl supers))))
             (declare (dynamic-extent #'walk-partial-cpl))
             (walk-partial-cpl class)
             nil))
          (t (and (member (load-time-value |owl|:|Restriction|)
                             (mop:class-precedence-list class)
                             :test #'eq)
                  t)))))
)
(defun restriction-subtypep (restriction1 restriction2)
  (and (eq (name (slot-value restriction1 '|owl|:|onProperty|))
           (name (slot-value restriction2 '|owl|:|onProperty|)))
       (etypecase restriction2
         (|owl|:|hasValueRestriction|
          (etypecase restriction1
            (|owl|:|hasValueRestriction|
             (eql (slot-value restriction1 '|owl|:|hasValue|)
                  (slot-value restriction2 '|owl|:|hasValue|)))
            (|owl|:|allValuesFromRestriction| nil)
            (|owl|:|someValuesFromRestriction| nil)
            (|owl|:|cardinalityRestriction| nil)))
         (|owl|:|allValuesFromRestriction| nil)
         (|owl|:|someValuesFromRestriction|
          (etypecase restriction1
            (|owl|:|hasValueRestriction| nil)
            (|owl|:|allValuesFromRestriction| nil)
            (|owl|:|someValuesFromRestriction|
             (subtypep-for-refining
              (slot-value restriction1 '|owl|:|someValuesFrom|)
              (slot-value restriction2 '|owl|:|someValuesFrom|)))
            (|owl|:|cardinalityRestriction| nil)))
         (|owl|:|cardinalityRestriction| nil))))

;;;
;;;; |owl|:|intersectionOf| is translated to super/subtype relation.
;;;

(defmethod collect-all-subsumed-types ((class |owl|:|Class|))
  (cond ((null (intersection-of class)) (collect-all-subtypes class))
        (t (remove-duplicates
            (loop for super in (remove-if #'owl-restriction-p (intersection-of class))
                append (remove-if-not #'(lambda (sub) (subsumed-p sub class))
                                      (collect-all-subtypes super)))))))

(defmethod collect-all-instances-of ((class |owl|:|Class|))
  (let ((*autoepistemic-local-closed-world* nil))
    (cond ((null (intersection-of class)) (call-next-method))
          (t (labels ((%all-instances-from (sub)
                                           (append
                                            (remove-if-not #'(lambda (ins) (%typep ins class))
                                                           (class-direct-instances sub))
                                            (mappend #'%all-instances-from
                                                     (mop:class-direct-subclasses sub)))))
               (remove-duplicates
                (loop for super in (remove-if #'owl-restriction-p (intersection-of class))
                    append
                      (loop for sib in (mop:class-direct-subclasses super)
                          append (%all-instances-from sib)))))))))

(defmethod all-instances-generator ((class |owl|:|Class|))
  (cond ((null (intersection-of class)) (call-next-method))
        (t (let ((pending-classes (intersection-of class))
                 (pending-instances nil))
             (flet ((generator ()
                      (loop
                        (when pending-instances
                          (return-from generator (pop pending-instances)))
                        (when (null pending-classes) (return-from generator nil))
                        (let ((next-class (pop pending-classes)))
                          (when (not (owl-restriction-p next-class)) ; next loop
                            (let ((instances
                                   (remove-if-not #'(lambda (ins) (typep ins class))
                                                  (class-direct-instances next-class))))
                              (when instances (setf pending-instances instances))
                              (setf pending-classes
                                (append (mop:class-direct-subclasses next-class)
                                        pending-classes))))))))
               #'generator)))))

;;
;; (collect-all-instances-of vin:TexasWine)
;;

(defun owl-complement-p (c d)
  (cond ((eq c |rdfs|:|Resource|) (values nil nil))
        ((eq d |rdfs|:|Resource|) (values nil nil))
        ((not (slot-exists-p c 'equivalent-classes)) (values nil nil)) ; not OWL, return
        ((not (slot-exists-p d 'equivalent-classes)) (values nil nil)) ; not OWL, return
        ((values (some #'(lambda (cc)
                           (some #'(lambda (dd) (%owl-complement-p cc dd))
                                 (equivalent-classes-of d)))
                       (equivalent-classes-of c))
                 t))))

(defun %owl-complement-p (cc dd)
  (and (slot-exists-p dd 'complement-class)
       (slot-boundp dd 'complement-class)
       (equal cc (slot-value dd 'complement-class))))

;;
;;
;;

(defmethod mop:ensure-class-using-class ((class |owl|:|Class|) name &rest args)
  ;(format t "~%ENSURE-CLASS-USING-CLASS ~S ~S ~S" class name args)
  (assert (not (eq (car (mop:class-direct-superclasses |owl|:|Thing|)) |owl|:|Thing|)))
  (cond ((eq class |owl|:|Thing|) class)                              ; nothing done
        ((getf args :direct-superclasses) (call-next-method))     ; next
        (t (let ((initargs (copy-list args)))
             (setf (getf initargs :direct-superclasses) '(|owl|:|Thing|))
             (apply #'call-next-method class name initargs)))))

;;;
;;;;  hasValue Restriction sets up an initial value of the slot.
;;;
;; The following methods are called after |owl|:|Class|
(defmethod shared-initialize :after ((class |owl|:|hasValueRestriction|) slot-names &rest initargs)
  (when (member-if #'(lambda (x) (owl-oneof-p x)) (getf initargs :direct-superclasses))
    (error "Debug It!"))
  (cond ((and (null slot-names) (null initargs))  ; when metaclass changed
         )
        ((and (consp slot-names) (null initargs)) ; when metaclass redefined, propagated
         )
        (t ;; first or redefinition
         (let ((property (slot-value class '|owl|:|onProperty|))
               (hasvalues (slot-value class '|owl|:|hasValue|)))
           (let ((range (get-range property)))
             (when range
               (slot-value-range-check '|owl|:|hasValue| hasvalues range)))
           (let* ((name (name property))
                  (slotd (find name (slot-value class 'excl::direct-slots) :key #'cg:name))
               #|   (initfun (cond ((symbolp hasvalues)
                                  (eval (excl::compute-initfunction-function
                                         hasvalues 'mop:slot-definition-initfunction
                                         (class-name class) name :boot)))
                                 ((rsc-object-p hasvalues)
                                  (eval (excl::compute-initfunction-function
                                         hasvalues 'mop:slot-definition-initfunction
                                         (class-name class) name)))
                                 ((stringp hasvalues)
                                  (eval (excl::compute-initfunction-function
                                         hasvalues 'mop:slot-definition-initfunction
                                         (class-name class) name)))
                                 ((numberp hasvalues)
                                  (eval (excl::compute-initfunction-function
                                         hasvalues 'mop:slot-definition-initfunction
                                         (class-name class) name)))
                                 (t (error "Not Yet for ~S" hasvalues)))) |#
                  )
             ;; see update-onPropertyConstraints-for
             (cond (slotd (reinitialize-instance slotd
                                                 :name name
                                                 :type (make-instance 'fills
                                                         :role name
                                                         :filler hasvalues
                                                         :subject-type class)
                                                 ;:initform hasvalues :initfunction initfun
                                                 ))
                   (t (case name
                        ((|rdfs|:|range|)
                         (push (make-instance 'gx::Property-direct-slot-definition
                                 :name name :initargs `(,name)
                                 :type (make-instance 'fills
                                         :role name
                                         :filler hasvalues
                                         :subject-type class)
                                 :subject-type class
                                 ;:initform hasvalues :initfunction initfun
                                 )
                               (slot-value class 'excl::direct-slots)))
                        (otherwise
                         (push (make-instance 'OwlProperty-direct-slot-definition
                                 :documentation (format nil
                                                    "From hasValueRestriction ~S" class)
                                 :name name :initargs `(,name)
                                 :type (make-instance 'fills
                                         :role name
                                         :filler hasvalues
                                         :subject-type class)
                                 :subject-type class
                                 ;:initform hasvalues :initfunction initfun
                                 )
                               (slot-value class 'excl::direct-slots)))))))))))

(defmethod shared-initialize :after
  ((class |owl|:|someValuesFromRestriction|) slot-names &rest initargs)
  (cond ((and (null slot-names) (null initargs))  ; when metaclass changed
         )
        ((and (consp slot-names) (null initargs)) ; when metaclass redefined, propagated
         )
        (t ;; first or redefinition
         (let ((property (slot-value class '|owl|:|onProperty|))
               (somevalues (slot-value class '|owl|:|someValuesFrom|)))
           (let* ((name (name property))
                  (slotd (find name (slot-value class 'excl::direct-slots) :key #'cg:name)))
             (cond (slotd (reinitialize-instance slotd :name name
                                                 :type (make-instance 'exists
                                                         :role name
                                                         :filler somevalues
                                                         :subject-type class)))
                   (t (case name
                        ((|rdfs|:|range|)
                         (push (make-instance 'gx::Property-direct-slot-definition
                                 :name name :initargs `(,name)
                                 :type (make-instance 'exists
                                         :role name
                                         :filler somevalues
                                         :subject-type class)
                                 :documentation "From someValuesFromRestriction as range"
                                 :subject-type class)
                               (slot-value class 'excl::direct-slots)))
                        (otherwise
                         (push (make-instance 'OwlProperty-direct-slot-definition
                                 :name name :initargs `(,name)
                                 :type (make-instance 'exists
                                         :role name
                                         :filler somevalues
                                         :subject-type class)
                                 :documentation "From someValuesFromRestriction"
                                 :subject-type class)
                               (slot-value class 'excl::direct-slots)))))))))))

(defmethod shared-initialize :after
  ((class |owl|:|allValuesFromRestriction|) slot-names &rest initargs)
  (cond ((and (null slot-names) (null initargs))  ; when metaclass changed
         )
        ((and (consp slot-names) (null initargs)) ; when metaclass redefined, propagated
         )
        (t ;; first or redefinition
         (let ((property (slot-value class '|owl|:|onProperty|))
               (allvalues (slot-value class '|owl|:|allValuesFrom|)))
           (let ((name (name property))
                 ;(range (get-range property))
                 )
             #|
             (when range
               (cond ((owl-oneof-p allvalues)
                      (loop for one in (class-direct-instances allvalues)
                          unless (typep one range)
                          do (warn "oneOf + onProperty range entailment by ~S: ~S |rdf|:|type| ~S"
                               name one range)
                            (change-class one range)))
                     ((and (not (cl:typep allvalues |owl|:|Restriction|))
                           (not (subsumed-p allvalues range)))
                      (warn "onProperty range entailment by ~S: ~S |rdfs|:|subClassOf| ~S"
                        name allvalues range)
                      (reinitialize-instance
                       allvalues
                       :direct-superclasses
                       (most-specific-concepts
                        (append (mklist range) (mop:class-direct-superclasses allvalues))))))) |#

             (let ((slotd (find name (slot-value class 'excl::direct-slots) :key #'cg:name)))
               (cond (slotd (reinitialize-instance slotd
                                                   :name name
                                                   :type (make-instance 'forall
                                                           :role name
                                                           :filler allvalues
                                                           :subject-type class)))
                     (t (case name
                          ((|rdfs|:|range|)
                           (push (make-instance 'gx::Property-direct-slot-definition
                                   :name name :initargs `(,name)
                                   :type (make-instance 'forall
                                           :role name
                                           :filler allvalues
                                           :subject-type class)
                                   :documentation "From allValuesFromRestriction as range"
                                   :subject-type class)
                                 (slot-value class 'excl::direct-slots)))
                          (otherwise
                           (push (make-instance 'OwlProperty-direct-slot-definition
                                   :name name :initargs `(,name)
                                   :type (make-instance 'forall
                                           :role name
                                           :filler allvalues
                                           :subject-type class)
                                   :documentation "From allValuesFromRestriction"
                                   :subject-type class)
                                 (slot-value class 'excl::direct-slots))))))))))))

(defmethod shared-initialize :after
  ((class |owl|:|cardinalityRestriction|) slot-names &rest initargs)
  (cond ((and (null slot-names) (null initargs))  ; when metaclass changed
         )
        ((and (consp slot-names) (null initargs)) ; when metaclass redefined, propagated
         )
        (t ;; first or redefinition
         (let ((property (slot-value class '|owl|:|onProperty|)))
           (let* ((name (name property))
                  (maxcardinality (and (slot-boundp class '|owl|:|maxCardinality|)
                                       (slot-value class '|owl|:|maxCardinality|)))
                  (mincardinality (and (slot-boundp class '|owl|:|minCardinality|)
                                       (slot-value class '|owl|:|minCardinality|)))
                  (cardinality (and (slot-boundp class '|owl|:|cardinality|)
                                    (slot-value class '|owl|:|cardinality|)))
                  (slotd (find name (slot-value class 'excl::direct-slots) :key #'cg:name)))
             (when (datatype-p (class-of cardinality))
               (setq cardinality (value-of cardinality)))
             (assert (or (null cardinality) (integerp cardinality)))
             (cond (slotd
                    (reinitialize-instance slotd
                                           :name name
                                           :maxcardinality cardinality
                                           :mincardinality cardinality))
                   (t
                    (when (datatype-p (class-of maxcardinality))
                      (setq maxcardinality (value-of maxcardinality)))
                    (when (datatype-p (class-of mincardinality))
                      (setq mincardinality (value-of mincardinality)))
                    (push (make-instance 'OwlProperty-direct-slot-definition
                            :documentation (format nil
                                               "From cardinalityRestriction ~S" class)
                            :name name :initargs `(,name)
                            :maxcardinality (or maxcardinality cardinality)
                            :mincardinality (or mincardinality cardinality))
                          (slot-value class 'excl::direct-slots)))))))))

;;;
;;;; For |owl|:|equivalentProperty|
;;;
(excl:without-redefinition-warnings
(defun equivalentProperty-maintain (instance slot-names &rest initargs)
  (declare (ignore slot-names))
  (when (getf initargs '|owl|:|equivalentProperty|)
    (let ((equivs (mklist (slot-value instance '|owl|:|equivalentProperty|))))
      (let ((equivalent-props (equivalent-property-of instance)))
        (cond ((null equivalent-props) (setq equivalent-props (list instance)))
              (t (setq equivalent-props (most-specific-properties (append equivalent-props equivs)))
                 (mapc #'(lambda (p) (setf (slot-value p 'equivalent-property) equivalent-props))
                   equivalent-props)))))))

(defun equivalent-property-p (x y)
  "Are <x> and <y> equivalent property in semantics of OWL?"
  (cond ((equal x y))
        ((and (uri-p x) (uri-p y))
         (uri= x y))
        ((uri-p x)
         (equivalent-property-p (%uri2symbol x) y))
        ((uri-p y)
         (equivalent-property-p x (%uri2symbol y)))
        ((and (symbolp x) (object? x))
         (equivalent-property-p (symbol-value x) y))
        ((and (symbolp y) (object? y))
         (equivalent-property-p x (symbol-value y)))
        ((and (property-p x) (property-p y)
              (not (not (member x (equivalent-property-of y))))))
        ((and (owl-thing-p x) (owl-thing-p y)
              (or ;; rdfp1 ter Horst
                  (functional-property-equal-p x y)
                  ;; rdfp2 ter Horst
                  (inverse-functional-property-equal-p x y))))
        ))
)

(defun most-specific-properties (properties)
  (setq properties (remove-duplicates properties :test #'eql))
  (warn "Equivalent properties:~S" properties)
  properties)

;;;
;;;; |owl|:|inverseOf|
;;;
(excl:without-redefinition-warnings
(defun %get-inverse-prop (prop)              ; See rdfp8ax, rdfp8bx
  (when (cl:typep prop |owl|:|ObjectProperty|)
    (or (and (slot-boundp prop '|owl|:|inverseOf|) (slot-value prop '|owl|:|inverseOf|))
        (slot-value prop 'inverse-inverse-of))))
)
;;;
;;;; Property in OWL
;;;

(defmethod shared-initialize :after ((instance |owl|:|ObjectProperty|) slot-names &rest initargs)
  ; instance = ub:memberOf
  (declare (ignore slot-names))
  (cond ((getf initargs '|owl|:|inverseOf|)
         (let ((inv (slot-value instance '|owl|:|inverseOf|)))
           ; inv = ub:member
           (assert (cl:typep inv |owl|:|ObjectProperty|))
           (assert (or (null (slot-value inv 'inverse-inverse-of))
                       (eql instance (slot-value inv 'inverse-inverse-of))))
           ;(format t "~%REINITIALIZE ~S :inverse-inverse-of ~S" inv instance)
           (reinitialize-instance inv :inverse-inverse-of instance)
           (let ((inv-domain (and (slot-boundp inv '|rdfs|:|domain|) (slot-value inv '|rdfs|:|domain|)))
                 ; inv-domain = ub:Organization
                 (inv-range (and (slot-boundp inv '|rdfs|:|range|) (slot-value inv '|rdfs|:|range|))))
             ; inv-range = ub:Person
             (when (or inv-range inv-domain)
               ;(format t "~%REINITIALIZE ~S |rdfs|:|domain| ~S |rdfs|:|range| ~S" instance inv-range (or inv-domain t))
               (reinitialize-instance instance '|rdfs|:|domain| inv-range '|rdfs|:|range| (or inv-domain t))
               ; slot ub:memberOf for ub:Person
               (mop:finalize-inheritance inv-range)
               ))))))

;; rule8 by seiji
(defmethod shared-initialize :after ((instance |owl|:|SymmetricProperty|) slot-names &rest initargs)
  (declare (ignore slot-names))
  (when (or (getf initargs '|rdfs|:|domain|) (getf initargs '|rdfs|:|range|))
    (let ((domain (domain-value instance))
          (range (range-value instance)))
      (when (and domain range)
        (let ((equivs (union (mklist domain) (mklist range))))
          (loop for cls in equivs
              do (unless (owl-class-p cls)
                   (warn "~S |rdfs|:|type| |owl|:|Class| by |owl|:|SymmetricProperty| entailment." cls)
                   (change-class cls |owl|:|Class|))
                (setf (slot-value cls 'equivalent-classes) equivs)
                ))))))

(excl:without-redefinition-warnings
(defmethod domain-value ((property |rdf|:|Property|))
  (flet ((get-dom (p) (and (slot-boundp p '|rdfs|:|domain|) (slot-value p '|rdfs|:|domain|))))
    (mkatom (mappend #'(lambda (p) (mklist (get-dom p))) (equivalent-property-of property)))))
)

(defmethod domain-value ((property |owl|:|ObjectProperty|))
  ;; in change-class from (|rdf|:|Property| vin:locatedIn) to |owl|:|TransitiveProperty|
  ;; this function is called from update-instance-for-different-class/shared-initialize:around(|rdf|:|Property|)
  ;; but |owl|:|inverseOf| and inverse-inverse-of slot value is unbound before shared-initialization.
  ;; rule12 by seiji
  (flet ((%get-inv (p) (and (slot-boundp p '|owl|:|inverseOf|) (slot-value p '|owl|:|inverseOf|)))
         (%get-inv-inv (p) (and (slot-boundp p 'inverse-inverse-of) (slot-value property 'inverse-inverse-of)))
         (get-dom (p) (and (slot-boundp p '|rdfs|:|domain|) (slot-value p '|rdfs|:|domain|)))
         (get-ran (p) (and (slot-boundp p '|rdfs|:|range|) (slot-value p '|rdfs|:|range|))))
    (let* ((inv (or (%get-inv property) (%get-inv-inv property)))
           (inv-range (and inv
                           (mappend #'(lambda (p) (mklist (get-ran p)))
                                    (equivalent-property-of inv))))   ; rule12b
           (domain (mappend #'(lambda (p) (mklist (get-dom p)))
                            (equivalent-property-of property))))      ; rule11a
      (mkatom (append domain inv-range)))))

(excl:without-redefinition-warnings
(defmethod range-value ((property |rdf|:|Property|))
  (flet ((get-ran (p) (and (slot-boundp p '|rdfs|:|range|) (slot-value p '|rdfs|:|range|))))
    (mkatom (mappend #'(lambda (p) (mklist (get-ran p))) (equivalent-property-of property)))))
)

(defmethod range-value ((property |owl|:|ObjectProperty|))
  (flet ((%get-inv (p) (and (slot-boundp p '|owl|:|inverseOf|) (slot-value p '|owl|:|inverseOf|)))
         (%get-inv-inv (p) (and (slot-boundp p 'inverse-inverse-of) (slot-value property 'inverse-inverse-of)))
         (get-dom (p) (and (slot-boundp p '|rdfs|:|domain|) (slot-value p '|rdfs|:|domain|)))
         (get-ran (p) (and (slot-boundp p '|rdfs|:|range|) (slot-value p '|rdfs|:|range|))))
    (let* ((inv (or (%get-inv property) (%get-inv-inv property)))
           (inv-domain (and inv
                            (mappend #'(lambda (p) (mklist (get-dom p)))
                                     (equivalent-property-of inv))))  ; rule12a
           (range (mappend #'(lambda (p) (mklist (get-ran p)))
                           (equivalent-property-of property))))       ; rule11b
      (mkatom (most-specific-concepts (append range inv-domain))))))

;;
;; HasValue Restriction Violation
;;
#|
(defmethod (setf mop:slot-value-using-class) :before
  (value (class |owl|:|Class|) (object |owl|:|Thing|) (slotd gx::OwlProperty-effective-slot-definition))
  ;(format t "~%Setf Slot-value-using-class:before with ~S to ~S ~S" value object slotd)
  (when (slot-value slotd 'excl::initform)
    (when (not (owl-equalp value (slot-value slotd 'excl::initform)))
      (when (and (slot-value slotd 'maxcardinality) (<= (slot-value slotd 'maxcardinality) 1)
                 (not (functional-property? (name slotd))))
        (error "|owl|:|hasValue| cardinality violation: ~S.~S <- ~S" object (name slotd) value)))))
|#
;;;
;;; We construct every inference upon subsumption-basis.
;;; Subsumption is infered by structural subsumption algorithms, here.

(defun union-of (class)
  (and (slot-exists-p class '|owl|:|unionOf|)
       (slot-boundp class '|owl|:|unionOf|)
       (slot-value class '|owl|:|unionOf|)))

(defun complement-of (class)
  (and (slot-exists-p class '|owl|:|complementOf|)
       (slot-boundp class '|owl|:|complementOf|)
       (slot-value class '|owl|:|complementOf|)))

(defun onproperty-of (restriction)
  (and (slot-exists-p restriction '|owl|:|onProperty|)
       (slot-boundp restriction '|owl|:|onProperty|)
       (slot-value restriction '|owl|:|onProperty|)))

;;;
;;;; Membership in OWL
;;;
;;; The following is taken from http://www.w3.org/TR/owl-ref/#equivalentProperty-def.
;;;
;;; "Property equivalence is not the same as property equality. Equivalent properties have the same "values"
;;; (i.e., the same property extension), but may have different intensional meaning (i.e., denote different concepts).
;;; Property equality should be expressed with the |owl|:|sameAs| construct. As this requires that properties are
;;; treated as individuals, such axioms are only allowed in OWL Full."
;;;
;;; Since we consider the domain and range constraint and OWL onPorperty restrictions are intensional data of properties.
;;; we do not treat here the equivalent property.
;;; We do not implement sameAs functions for properties.

(excl:without-redefinition-warnings
(defun %%typep (object type)
  "<object> and <type> is an object in RDF universe."
  (declare (optimize (speed 3) (safety 0)))
  (let ((equivs (append
                 (and (slot-exists-p type 'equivalent-classes)
                      (slot-boundp type 'equivalent-classes)
                      (slot-value type 'equivalent-classes))
                 (and (slot-exists-p type 'same-as)
                      (slot-boundp type 'same-as)
                      (slot-value type 'same-as))))) ; OWL Full
    (if (null equivs) (%typep-without-type-equivalents object type) ; no equiv def
      (let ((value2 t))
        (loop for c in equivs                                ; equivs include type
            do (multiple-value-bind (val1 val2)
                   (%typep-without-type-equivalents object c)
                 (when val1 ; some equiv satisfies condition
                   (return-from %%typep (values t t)))
                 (setq value2 (and value2 val2))))
        (values nil value2)))))
)

(defun %typep-without-type-equivalents (object type)
  "This is sub-subfunction for <typep>. This is only invoked in <%%typep>.
   This function tests type relation for each member of |owl|:|sameAs| group."
  (if (owl-oneof-p type)
      (cond ((member object (slot-value type '|owl|:|oneOf|) :test #'%owl-same-p)
             (values t t))
            (t (values nil t)))
    (let ((sames (and (slot-exists-p object 'same-as)
                      (slot-boundp object 'same-as)
                      (slot-value object 'same-as))))
      (cond (sames
             (let ((value2 t))
               (loop for same in sames
                   do (multiple-value-bind (val1 val2)
                          (typep-without-sames-and-equivalents-in-owl same type)
                        (when val1
                          (return-from %typep-without-type-equivalents (values t t)))
                        (setq value2 (and value2 val2))))
               (values nil value2)))
            (t (typep-without-sames-and-equivalents-in-owl object type))))))

(defun typep-without-sames-and-equivalents-in-owl (object type)
  "<object> and <type> are in RDF universe and no direct class-instance relations.
   <type> may be a complex concept."
  (cond ((intersection-of type)
         (owl-intersection-type-p object (intersection-of type)))
        ((union-of type)
         (owl-union-type-p object (union-of type)))
        ((complement-of type)
         (owl-complement-type-p object (complement-of type)))
        (*autoepistemic-local-closed-world*
         (values nil t))
        (t (values nil nil))))

(defun owl-intersection-type-p (object intersections)
  "when every element in <intersections> satisfies a type of <object> then true.
   If any element does not satisfy obviously, then imediately returns with false.
   Otherwise, finally the result for all of <intersections> is true or unknown."
  (let ((supers (remove-if #'owl-restriction-p intersections))
        (restrictions (remove-if-not #'owl-restriction-p intersections))
        (known t))
    (loop for sup in supers
        do (multiple-value-bind (v1 v2) (%typep object sup)
             (when (not v1)
               (cond (v2             ; <nil, t> implies false then imediately returns
                      (return-from owl-intersection-type-p (values nil t)))
                     (t              ; <nil, nil> implies unknown
                      (setq known nil))))))
    ;; true or unknown here
    (loop for r in restrictions
        as prop = (name (onproperty-of r))
        as slotvalues = (and (slot-exists-p object prop)
                             (slot-boundp object prop)
                             (mklist (slot-value object prop)))
        do (etypecase r
             (|owl|:|allValuesFromRestriction|          ; rdfp16 by ter Horst
              (cond ((null slotvalues)              ; by Seiji, allValues restriction in intersection must exist definitely.
                     (return-from owl-intersection-type-p (values nil t)))
                    (t (let ((rst (slot-value r '|owl|:|allValuesFrom|)))
                         (loop for v in slotvalues
                             do (multiple-value-bind (v1 v2) (%typep v rst)
                                  (when (not v1)
                                    (cond (v2 (return-from owl-intersection-type-p (values nil t)))
                                          (t (setq known nil))))))))))
             (|owl|:|someValuesFromRestriction|          ; rdfp15 by ter Horst
              (cond ((null slotvalues)
                     (if *autoepistemic-local-closed-world*
                         (if (y-or-n-p "No value for ~S in ~S.~%Create it?" prop object)
                             (setf (slot-value object prop)
                               (addInstance (slot-value r '|owl|:|someValuesFrom|) nil))
                           (return-from owl-intersection-type-p (values nil t)))
                       (return-from owl-intersection-type-p (values nil nil))))
                    (t (let ((rst (slot-value r '|owl|:|someValuesFrom|)))
                         (cond ((some #'(lambda (v)
                                          (multiple-value-bind (v1 v2) (%typep v rst)
                                            (when (not v1)
                                              (setq known (and known v2)))
                                            v1))   ; then imediately exits this mapping fun
                                      slotvalues)) ; and nothing done
                               ; not satisfied
                               (*autoepistemic-local-closed-world*    ; local world
                                (if (y-or-n-p "No value for ~S in ~S.~%Create it?" prop object)
                                    (setf (slot-value object prop)
                                      (addInstance (slot-value r '|owl|:|someValuesFrom|) nil))
                                  (return-from owl-intersection-type-p (values nil t))))
                               (t (return-from owl-intersection-type-p (values nil nil))))))))
             (|owl|:|hasValueRestriction|              ; rdfp14a by ter Horst
              (cond ((null slotvalues)
                     (if *autoepistemic-local-closed-world*
                         (if (error "check it")
                             ;(y-or-n-p "No value for ~S in ~S.~%Add it?" prop object)
                             (setf (slot-value object prop) (slot-value r '|owl|:|hasValue|))
                           (return-from owl-intersection-type-p (values nil t)))
                       (return-from owl-intersection-type-p (values nil nil))))
                    (t (let ((rst (slot-value r '|owl|:|hasValue|)))
                         (cond ((some #'(lambda (v)
                                          (multiple-value-bind (v1 v2)
                                              ;; for regional subsumption
                                              (transitive-property-subsumed-p v rst)
                                            (when (not v1)
                                              (setq known (and known v2)))
                                            v1))   ; then imediately exits this mapping fun
                                      slotvalues)) ; and nothing done
                               ; not satisfied
                               (*autoepistemic-local-closed-world*    ; local world
                                (if (error "check it")
                                    ;(y-or-n-p "No value for ~S in ~S.~%Add it?" prop object)
                                    (setf (slot-value object prop) (slot-value r '|owl|:|hasValue|))
                                  (return-from owl-intersection-type-p (values nil t))))
                               (t (return-from owl-intersection-type-p (values nil nil))))))))
             (|owl|:|cardinalityRestriction|
              (let ((maxr (cond ((slot-boundp r '|owl|:|cardinality|)
                                 (slot-value r '|owl|:|cardinality|))
                                ((slot-boundp r '|owl|:|maxCardinality|)
                                 (slot-value r '|owl|:|maxCardinality|))))
                    (minr (cond ((slot-boundp r '|owl|:|cardinality|)
                                 (slot-value r '|owl|:|cardinality|))
                                ((slot-boundp r '|owl|:|minCardinality|)
                                 (slot-value r '|owl|:|minCardinality|)))))
                (when (and maxr (cl:typep maxr |rdf|:|XMLLiteral|))
                  (setq maxr (value-of maxr)))
                (when (and minr (cl:typep minr |rdf|:|XMLLiteral|))
                  (setq minr (value-of minr)))
                (unless (and (or (null minr) (>= (length slotvalues) minr))
                             (or (null maxr) (<= (length slotvalues) maxr)))
                  (return-from owl-intersection-type-p (values nil t))))))
        finally (return (cond (known (values t t))
                              (t (values nil nil)))))))
;;
;; (typep vin:StGenevieveTexasWhite vin:TexasWine)        -> tt
;; (typep vin:SaucelitoCanyonZinfandel1998 vin:TableWine) -> tt
;; (typep vin:MariettaOldVinesRed vin:DryWine)            -> tt
;;

(defun owl-union-type-p (object unions)
  (let ((subs (remove-if #'owl-restriction-p unions))
        (restrictions (remove-if-not #'owl-restriction-p unions))
        (known t))
    (assert (null restrictions))
    (loop for sub in subs
        do (multiple-value-bind (v1 v2) (%typep object sub)
             (cond (v1 (return-from owl-union-type-p (values t t)))
                   (t (setq known (and known v2)))))
        finally (return (values nil known)))))

(defun owl-complement-type-p (object complement)
  (multiple-value-bind (v1 v2) (%typep object complement)
    (cond (v1 (values nil t))
          (v2 (warn "Check ~S |rdf|:|type| (complement-of ~S)" object complement)
              (values t t))
          (t (values nil nil)))))

;;;
;;;; slot-value
;;;
;;; Cardinality check

(defmethod cardinality-ok-p (value (slotd OwlProperty-effective-slot-definition))
  (let ((slot-name (mop:slot-definition-name slotd))
        (maxcardinality (slot-definition-maxcardinality slotd))
        (length (if (listp value) (length value) 1)))
    (or (null maxcardinality)
        (< length maxcardinality)
        (= length maxcardinality)
        (cond ((functional-property? slot-name)
               (cond ((<= length 1))
                     ((rdf-instance-p (car value))
                      (cond ((intersection value value :test #'owl-different-p)
                             (error 'sameas-condition-unsatiafiable
                               :format-control "in cardinality check, ~S includes different individuals."
                               :format-arguments `(,value))
                             nil)
                            (t (warn "Entailing same individuals ~S by functional property ~S"
                                 value slot-name)
                               (loop for v in (cdr value)
                                   do (pushnew v (slot-value (car value) 'same-as))
                                     (pushnew (car value) (slot-value v 'same-as)))
                               nil)))
                     ((owl-class-p (car value))
                      (cond ((intersection value value :test #'disjoint-p)
                             (warn "Disjoint classes ~S are designated to functional property ~S"
                               value slot-name)
                             nil)
                            (t (warn "Entailing equivalent classes ~S by functional property ~S"
                                 value slot-name)
                               (loop for v in (cdr value)
                                   do (pushnew v (slot-value (car value) 'equivalent-with))
                                     (pushnew (car value) (slot-value v 'equivalent-with)))
                               nil)))
                     ((error "Cant happen!")))))
        )))
#|
;;;
;;;; Change-class
;;;

(defmethod update-instance-for-different-class :after
  ((previous |rdfs|:|Class|) (current |owl|:|Class|) &rest initargs)
  (unless (cl:typep previous |owl|:|Class|)
    ;; changes the default super from |rdfs|:|Resource| to |owl|:|Thing|
    (let* ((old-supers (mop:class-direct-superclasses previous))
           (new-supers (make-this-supers current old-supers nil)))
      (unless (set-equalp old-supers new-supers)
        ;(format t "~&Bingo! new supers of ~S is changed to ~S." current new-supers)
        (reinitialize-instance current :direct-superclasses new-supers)))))

(defmethod change-class :before ((instance |owl|:|Thing|) (new-class |rdfs|:|Class|) &rest initargs)
  (when (and (eq (class-of instance) |owl|:|Thing|)
             (eq (class-of new-class) |rdfs|:|Class|))
    (warn "COMPLEX PROACTIVE ENTAILMENT: ~S type |owl|:|Class| when change class ~S to ~S."
      (name new-class) (name instance) (name new-class))
    ))

;;
;; (typep ub:Department10.University0.FullProfessor5 ub:Chair) => t
;;
#|
(defmethod (setf mop:slot-value-using-class)
    ((value Property-direct-slot-definition) (class |rdfs|:|Class|) (object |owl|:|Restriction|) slotd)
  ;(format t "~%Setf Slot-value-using-class with ~S to ~S ~S" value object slotd)
  (error "Bingo")
  (let ((slot-name (mop:slot-definition-name slotd))
        (prop-name (mop:slot-definition-name value)))
    (assert (eq slot-name 'excl::direct-slots))
    (cond ((not (slot-boundp object slot-name))
           (funcall #'call-next-method (list value) class object slotd))
          (t (let ((direct-slotds (remove prop-name (slot-value object slot-name)
                                          :key #'mop:slot-definition-name)))
               (cond ((null direct-slotds)
                      (funcall #'call-next-method
                               (list value) class object slotd))
                     (t (funcall #'call-next-method
                                 (cons value direct-slotds) class object slotd))))))))
|#
;;;
;;;; OWL Untils
;;;

(eval-when (:execute :load-toplevel :compile-toplevel)
  (setq *system-properties* (list-all-properties t))
  )

(eval-when (:execute :load-toplevel)
  (setq *referenced-resources* nil)
)

(defmethod superclasses-of ((object |owl|:|Class|))
  (mappend #'equivalent-classes-of
           (mappend #'mop:class-direct-superclasses (equivalent-classes-of object))))
(defmethod subclasses-of ((object |owl|:|Class|))
  (mappend #'equivalent-classes-of
           (mappend #'mop:class-direct-subclasses (equivalent-classes-of object))))

;;
;; Additional Useful Axioms
;;
#+:slot-value-for-metaclass
(defConcept |owl|:|DatatypeProperty| (|rdf|:|type| |rdfs|:|Class|)
  (|rdfs|:|subClassOf| (|owl|:|Restriction| (|owl|:|onProperty| |rdfs|:|range|)
                                    (|owl|:|allValuesFrom| |rdfs|:|Datatype|))))
#+:slot-value-for-metaclass
(defConcept |owl|:|ObjectProperty| (|rdf|:|type| |rdfs|:|Class|)
  (|rdfs|:|subClassOf| (|owl|:|Restriction| (|owl|:|onProperty| |rdfs|:|range|)
                                    (|owl|:|allValuesFrom| |owl|:|Class|))))
|#

