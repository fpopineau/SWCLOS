;; -*- mode: common-lisp; syntax: common-lisp; package: gx-user; base: 10 -*-
;;;
;;; IT Program Project in Japan:
;;;          Building Operation-Support System for Large-scale System with IT
;;;
;;; Memory Organization Package on SWCLOS
;;; This program originates from Schank's MOP.
;;;
;;; This code was written by Seiji Koide for the Japanese MEXT IT Program,
;;; based on his experience on Human Media Project funded by NEDO-Japan.
;;; The name SWMop stands for Memory Organization Package on top of SWCLOS.
;;; Memory Organization Package(MOP) is a case-based memory system developed by Schank et.al.
;;; SWCLOS is a Semantic Web Processor on top of CLOS, programmed by Meta-Object Protocol(MOP).
;;; Meta-Object Protocol(MOP) is a user-customizable reflective object-oriented language on CLOS.
;;; The original code of Memory Organization Package was published in the books
;;; "Inside Case-Based Reasoning", C. K. Riesbeck and R. C. Schank, 1989, LEA, and
;;; "Inside Case-Based Explanation", R. C. Schank, A. Kass, and C. K. Riesbeck, 1994, LEA.
;;;
;;; Copyright (c) 2004, by Galaxy Express Corporation
;;;
;;; History
;;; -------
;;; 2004.10.24    File created

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage swmop
    (:shadowing-import-from :gx subtypep typep type-of subtypep typep type-of)
    (:use :common-lisp :gx)
    (:export calcFn role-filler Pattern get-filler list->sequence sequence->list Case)
    (:documentation "http://www.galaxy-express.co.jp/semweb/swmop#"))
  (require :owl)
  )

(in-package swmop)

;;;
;;; MFunction
;;;

(defResource MFunction (|rdf|:|type| |rdfs|:|Class|))

(defResource MFunctionClass (|rdf|:|type| |rdfs|:|Class|)
  (|rdfs|:|comment| "Meta class for MFunction")
  (|rdfs|:|subClassOf| |rdfs|:|Class| MFunction))

(defResource ConstraintFn (|rdf|:|type| |rdfs|:|Class|)
  (|rdfs|:|subClassOf| MFunction))

(defResource ConstraintFnClass (|rdf|:|type| |rdfs|:|Class|)
  (|rdfs|:|comment| "Meta class for ConstraintFn")
  (|rdfs|:|subClassOf| |rdfs|:|Class| ConstraintFn))

(defResource NotConstraint (|rdf|:|type| ConstraintFnClass)
  (|rdfs|:|subClassOf| ConstraintFnClass))

(defResource GetSibling (|rdf|:|type| MFunctionClass)
  (|rdfs|:|subClassOf| MFunctionClass))

;;;
;;; Pattern
;;;

(defResource Pattern (|rdf|:|type| |rdfs|:|Class|)
  (|rdfs|:|comment| "Pattern is from Schank's Memory Organization Package."))

(defProperty abstFn (|rdf|:|type| |rdf|:|Property|)
  (|rdfs|:|domain| Pattern)
  (|rdfs|:|range| ConstraintFn))

(defResource PatternClass (|rdf|:|type| |rdfs|:|Class|)
  (|rdfs|:|comment| "Meta class for Pattern")
  (|rdfs|:|subClassOf| |rdfs|:|Class| Pattern))

(defResource MNot (|rdf|:|type| PatternClass)
  (|rdfs|:|subClassOf| PatternClass)
  (|rdfs|:|subClassOf|
   (|owl|:|Restriction| (|owl|:|onProperty| abstFn)
                    (|owl|:|hasValue| NotConstraint))))

(defProperty calcFn (|rdf|:|type| |rdf|:|Property|)
  (|rdfs|:|domain| Pattern)
  (|rdfs|:|range| MFunction))

(defResource MSibling (|rdf|:|type| PatternClass)
  (|rdfs|:|subClassOf| PatternClass)
  (|rdfs|:|subClassOf|
   (|owl|:|Restriction| (|owl|:|onProperty| calcFn)
                    (|owl|:|hasValue| GetSibling))))

(defResource Case (|rdf|:|type| |rdfs|:|Class|)
  (|rdfs|:|subClassOf|
   (|owl|:|Restriction| (|owl|:|onProperty| old)
                    (|owl|:|hasValue| MSibling))))


#|
(defmethod shared-initialize :after ((class Pattern) slot-names &rest initargs)
  (declare (ignore slot-names))
  (let ((abst-fn (and (slot-boundp class 'abstFn) (slot-value class 'abstFn))))
    ;(format t "~%SHARED-INITIALIZE AFTER ~S ~S" class abst-fn)
    ))
|#

(defmethod gx::typep-using-class-in-owl (object (type Pattern))
  ;(format t "~%RESTRICTION TYPEP ~S ~S" object type)
  )

;;;
;;; Deamon
;;;

(defun get-filler (mop role)
  "get-filler <mop> <role>
   returns the filler for <role> in <mop>. <mop> may be a list of slots or
   a mop. The filler can be either an inherited instance mop, an inherited
   non-mop value, or a calculated value. If an inherited filler is an
   abstraction, it must be a pattern mop or a calculation function mop.
   Then, a filler is calculated and the slot for that role and filler is newly
   created and added to <mop>."
  (let ((filler (-> mop role)))
    (cond ((patternp filler)
           (let ((fn (-> filler 'calcFn)))
             (when fn
               (let ((new-filler (funcall (name fn) mop filler)))
                 (when new-filler
                   (addForm `(,mop (,role ,new-filler))))))))
          (t filler))))

;;;
;;; Instantiate
;;;

(defvar It "special variable for object under refinement")

;; We implement slots as simple lists to simplify handling fillers.
(defstruct (slot (:type list)) role filler)

;; Create-slot is newly defined in order to work around the name collision
;; with make-slot by slot defstruct.
(defun create-slot (role filler)
  "create-slot <role> <filler>
   returns a slot, or a list of <role> and <filler>."
  (make-slot :role role :filler filler))

(defstruct (mop (:print-object print-mop))
  name absts specs slots props)

(defun print-mop (case stream depth)
  (declare (ignore depth))
  (print-unreadable-object (case stream :type t :identity t)
     (princ "a case" stream)))

(defun mop-of (name)
  "mop-of <name>
   returns case object from <name>."
  (and name (boundp name) (symbol-value name)))

(defun instantiate (name absts slots &rest props)
  "instantiate <name> <absts> <slots> <props>
   adds a new instance to memory with the given <name>, <absts>, <slots>,
   and <props>.  If a case of that name already exists, it is redefined.
   . <name> can be any symbol including nil or QName but keyword and t.
   . <absts> should be either a class or a (possibly empty) list of classes
   . <slots> should be a list of the form ((role1 filler1) (role2 filler2) ...)
     A role should be a symbol or property, a filler can be anything.
   . <props> should be a list of the form (key1 value1 key2 value2 ...).
   The instantiated case is returned."
  (let ((old (mop-of name))
        (It (make-mop :name name :absts absts :slots slots :props props))
        (*reify-p* cl:nil))
    (refine-instance It)
    (remove-illegal-absts It)
    (unless (mop-absts It) ; failed for instantiation
      (%remove-mop It)
      (return-from instantiate))
    (let ((twin (get-twin It)))
      (when twin
        (%remove-mop It)
        (return-from instantiate twin)))
    (unless name
      (setq name (calc-name (car (mop-absts It)))))
    (cond ((null (cdr (mop-absts It)))
           (gx::addObject (car (mop-absts It)) (cons name (mop-slots It))))
          (t (let ((shadow (gx::make-shadow (mop-absts It) (gx::shadow-name (car (mop-absts It))))))
               (change-class It shadow))))))

(defun refine-instance (case)
  "refine-instance <case>
   takes classes of <case> and tries to replace it with one
   or more special classes of the classes. It repeats this process until all
   classes of <case> are as specialized as possible."
  ;(format t "~%Refining from ~S ..." (mop-absts case))
  (some
   #'(lambda (abst)
       (when (mops-abstp (mop:class-direct-subclasses abst) case)
         (unlink-abst case abst)
         (refine-instance case)))
   (mop-absts case)))

(defun unlink-abst (case abst)
  "unlink-abst <case> <abst>
   removes the type link from <case> to <abst> and returns <case>."
  (setf (mop-absts case) (remove abst (mop-absts case)))
  case)

(defun link-abst (case abst)
  "link-abst <case> <abst>
   makes <abst> a direct class of <case>. If <case> is already linked,
   nothing happens. Otherwise <abst> is put in the direct abstractions
   of <case>, and <case> is put in the direct specials of <abst>.
   <case> is returned."
  (pushnew abst (mop-absts case))
  case)

(defun mops-abstp (absts case)
  "mops-abstp <abst-list> <case>
   looks at each abst in <abst-list>.  If the abst can be abstract <case>,
   a link from <case> to the abst is made.  True is returned if at least
   one such abst is found."
  (not (null (loop for abst in absts
                 when (slots-abstp abst case)
                 collect (link-abst case abst)))))

(defun slots-abstp (class case)
  "slots-abstp <class> <case>
   returns true if every slot range in <class> is 'satisfied' by the corresponding
   slot in <case>. The slot range in <class> is treated as 'constraint'
   on the slot filler in <case>. <class> must have at least one slot to be
   satisfied. If not so, false is returned.
   See, satisfiedp."
  (loop for (role range) in (get-restrictions class)
      always (let ((filler (and (slot-boundp case role) (slot-value case role))))
               ;(when (instance-p case) (refine-instance filler))
               (satisfiedp range filler))))

(defun satisfiedp (constraint filler)
  "satisfiedp <constraint> <filler>
   returns true if <filler> satisfies the conditions specified by <constraint>.
   A constraint is satisfied if
   . <constraint> is null.
   . <constraint> is a pattern whose 'abstraction function' returns true
     when called with <constraint> and <filler>. An abstraction function is
     a callable CLOS method name stored in the abstFn role of the pattern.
   . <constraint> is a type of <filler> using predicate gx:typep.
   . <constraint> has at least one restriction, <filler> is not null, and all of
     <constraint>'s slots are satisfied by the slots of <filler>.
   Note that a constraint may be any type combination of (and ...), (or ...),
   and (eql ...)."
  (cond ((null constraint))
        ((null filler))
        ;((eq filler t))
        ((symbolp constraint)
         (cond ((resource? constraint)
                (satisfiedp (symbol-value constraint) filler))
               ((datatype? constraint)
                (gx:typep filler constraint))
               ((error "Cant happen"))))
        ((patternp constraint)
         (or (funcall (slot-value constraint 'abstFn) constraint filler)
             (return-from satisfiedp nil)))
        ((gx:typep filler constraint))
        ((and (gx::class-p constraint) (gx::instance-p filler))
         (slots-abstp constraint filler))
      ;  ((instance-p filler)
      ;   (refine-instance filler)
      ;   (typep filler constraint))
        ))

(defun remove-illegal-absts (case)
  (setf (mop-absts case)
    (remove-if #'illegal-abst-p (mop-absts case))))

(defun illegal-abst-p (class)
  "legal-abstp <class>
   returns true if <class> is a illegal place to put an instance, i.e.,
   <class> is slotless or have subclasses below it."
  (or (null (get-restrictions class))
      (mop:class-direct-subclasses class)))

(defun get-twin (case)
  "get-twin <case>
   returns the first mop sharing an abstraction in memory it can find
   that is 'equal' to <instance>. Here 'equal' means in the sense of mop."
  (some #'(lambda (abst)
           (some #'(lambda (sib) (mop-equalp-for-case sib case))
             (gx::class-direct-instances abst)))
    (mop-absts case)))

(defun mop-equalp-for-case (ins case)
  "mop-equalp <ins> <case>
   retunrs <ins> if <ins> and <case> are the same."
  (when (and (gx::set-equalp (gx::mclasses ins) (mop-absts case))
             (loop for (role filler2) in (mop-slots case)
                 always (equalp (slot-value ins role) filler2))
             (loop for (role filler1) in (gx::mop-slots ins)
                 always (equalp (slot-value case role) filler1)))
    ins))

(defun role-filler (source role)
  "role-filler <source> <role>
   returns the filler associated with <role> in the slots of <source>."
  (let ((slot (role-slot source role)))
    (when slot (slot-filler slot))))

(defun role-slot (source role)
  "role-slot <source> <role>
   returns a slot whose role is <role> in <source>.  <source>
   may be a slot list or a frame name or a frame object."
  (find role (slots-of source) :key #'slot-role))

(defun slots-of (source)
  "slots-of <source>
   returns slots of <source>, which may be the name of an object, or a list.
   If <source> is a non-NIL list, it is just returned. If <source> is not a
   list or an object or an object-name, NIL is returned."
  (if (consp source)
      source
    (if (symbolp source)
        (when (resource? source) (symbol-value source))
      (gx::mop-slots source))))
(defun patternp (x)
  "patternp <x>
   returns true if <x> is an instance of Pattern."
  (cl:typep x Pattern))

;;; --------------------------------------------------------------------
;;;
;;; Sequence
;;;
;;; You can't make instance directly under M-SEQUENCE,
;;; because an instance of sequence can't be made under any slotless abstract.
;;; You have to define a mediate special of sequence for creating instances.

;;; Sequence functions are moved here from NewMop.lisp
;;; Only sequencep is left at NewMop.lisp

(defun list->sequence (l)
  "LIST->SEQUENCE <list>
   returns a sequence mop with members from <list>. The first element of
   <list> fills the first role, the second fills the second role, and so on.
   If the list is empty, the instance mop EMPTY-SEQUENCE is returned."
  (if (null l) (frame-of 'EMPTY-SEQUENCE)
      (slots->mop
       (loop for x in l
             for i from 1 to (length l)
             collect (create-slot i x))
       (list (frame-of 'M-SEQUENCE))
       t)))

(defun sequence-size (x)
  "SEQUENCE-SIZE <sequence>
   returns the size of the sequence."
  (and (sequencep x) (length (mop-slots x))))

(defun sequence->list (sequence)
  "SEQUENCE->LIST <sequence>
   returns a list of the members of the sequence, or the filler of first
   role, of second role, and so on."
  (and sequence
       (progn (assert (sequencep sequence) () "SEQUENCE->LIST: illegal MOP.") t)
       (loop for index from 1 to (sequence-size sequence) with filler
             when (setq filler (role-filler sequence index))
             collect filler)))

(defun sequence-member (mop sequence)
  "SEQUENCE-MEMBER <mop> <sequence>
   returns true if <mop> is a member of <sequence>."
  (and (sequencep sequence)
       (loop for slot in (mop-slots sequence)
            thereis (eql (slot-filler slot) mop))))

(defun sequence-splice (new old sequence)
  "SEQUENCE-SPLICE <mop-list> <mop> <sequence>
   returns a new sequence mop with all the elements of <sequence>, except
   that <mop> is replaced with the elements of <mop-list>.  Note that a NIL
   <mop-list> returns a sequence with <mop> removed."
  (list->sequence
   (loop for mop in (sequence->list sequence)
        append (cond ((eql mop old) new)
                      (t (list mop))))))

(defun sequence-insert (mop sequence)
  "SEQUENCE-INSERT <mop> <sequence>
   returns a new sequence mop with all the elements of <sequence> plus <mop>,
   added at the end."
  (cond ((null mop) sequence)
        ((sequence-member mop sequence) sequence)
        (t (list->sequence (append (sequence->list sequence) (list mop))))))

(defun sequence-add (time data sequence)
  (push `(,time ,data) (mop-slots sequence)))

#|
(defun mop-includesp (mop1 mop2)
  "mop-includesp <mop1> <mop2>
   returns <mop1> if it includes <mop2>.  This is true if the two
   mops have the same type and every slot in <mop2> is also in <mop1>.
   <mop1> might have losts that are not in <mop2>."
  (and (eql (mop-type mop1) (mop-type mop2))
       (loop for (role filler2) in (gx::mop-slots mop2)
             always (let ((filler1 (get-filler mop1 role)))
                      (cond ((and (stringp filler1) (stringp filler2))
                             (string= filler2 filler1))
                            ((and (numberp filler1) (numberp filler2))
                             (= filler1 filler2))
                            ((and (consp filler1) (consp filler2))
                             (equalp filler1 filler2))
                            (t (eql filler2 filler1)))))
       mop1))

(defun get-instance (class)
  (car (gx::class-direct-instances class)))

(defun get-slot-from-class (class role)
  (unless (mop:class-finalized-p class)
    (mop:finalize-inheritance class))
  (let ((slotds (mop:class-slots class))
        (slotd cl:nil))
    (when (setq slotd (find role slotds :key #'mop:slot-definition-name))
      (assert (gx::resource? (mop:slot-definition-type slotd)))
      (symbol-value (mop:slot-definition-type slotd)))))

;;;
;;; Semantics in Mop
;;;

(defun get-restrictions (class)
  "get-restrictions <class>
   returns all effective ranges of semantic properties in the form '(role range)'."
  (unless (mop:class-finalized-p class) (mop:finalize-inheritance class))
  (loop for slotd in (mop:class-slots class)
      with range
      when (and (cl:typep slotd 'gx::Property-effective-slot-definition) ; includes OwlProperty-direct-slot-definition
                (setq range (mop:slot-definition-type slotd))
                (not (eq range t)))
      collect `(,(mop:slot-definition-name slotd) ,range)))

(defun mop-includesp (mop1 mop2)
  "mop-includesp <mop1> <mop2>
   returns <mop1> if it includes <mop2>. This is true if the two
   mops have the same type(instance or class or metaclass) and every slot
   in <mop2> is also in <mop1>. <mop1> might have slots that are not
   in <mop2>. Note that fewer slots includes more slots. Mop2 includes Mop1."
  (when (and (or (and (metaclass-p mop1) (metaclass-p mop2))
                 (and (strict-class-p mop1) (strict-class-p mop2))
                 (and (instance-p mop1) (instance-p mop2)))
             (loop for (role filler2) in (gx::mop-slots mop2)
                 always (equalp (slot-value mop1 role) filler2)))
    mop1))

;;;
;;; Task Instantiation
;;;

(defun relative-p (x y)
  (or (eql x y)
      (cl:subtypep x y)
      (cl:subtypep y x)))

(defun remove-mop (x)
  (let ((class (class-of x)))
    (setf (gx::class-direct-instances class)
      (remove x (gx::class-direct-instances class))))
  (when (gx::class-p x)
    (let ((subs (mop:class-direct-subclasses x))
          (sups (mop:class-direct-superclasses x)))
      (loop for sub in subs
          do (setf (mop:class-direct-superclasses sub)
               (remove x (mop:class-direct-superclasses sub))))
      (loop for sup in sups
          do (setf (mop:class-direct-subclasses sup)
               (remove x (mop:class-direct-subclasses sup))))))
  (setf (symbol-value (name x)) cl:nil)
  x)

|#

(defun %remove-mop (x)
  (setf (mop-name x) nil)
  (setf (mop-absts x) nil)
  (setf (mop-specs x) nil)
  (setf (mop-slots x) nil)
  (setf (mop-props x) nil))

(defun calc-name (type)
  "calc-name <type>
   constructs a symbol in agent's memory package of the form '<type>.nn'."
  (let ((sym (class-name type)))
    (let ((str (symbol-name sym))
          (pkg (find-package :gxmemory)))
      (gentemp (concatenate 'string str ".")
               pkg))))

#|
(defResource Actor (|rdf|:|type| |owl|:|Class|))
(defResource Economist (|rdf|:|type| |owl|:|Class|)
  (|rdfs|:|subClassOf| Actor))
(defIndividual Thurow (|rdf|:|type| Economist))
(defIndividual Friedman (|rdf|:|type| Economist))

(defResource MObject (|rdf|:|type| |owl|:|Class|))
(defResource Arg (|rdf|:|type| |owl|:|Class|)
  (|rdfs|:|subClassOf| MObject))
(defResource MObject/StateChange (|rdf|:|type| |owl|:|Class|)
  (|rdfs|:|subClassOf| MObject))
(defResource Arg/Econ (|rdf|:|type| |owl|:|Class|)
  (|rdfs|:|subClassOf| Arg))
(defResource Arg/Mon (|rdf|:|type| |owl|:|Class|)
  (|owl|:|intersectionOf| Arg/Econ MObject/StateChange))

(defResource MTrans (|rdf|:|type| |owl|:|Class|)
  (|owl|:|intersectionOf|
   (|owl|:|Restriction| (|owl|:|onProperty| actor)
                    (|owl|:|allValuesFrom| Actor))
   (|owl|:|Restriction| (|owl|:|onProperty| info)
                    (|owl|:|allValuesFrom| MObject))))
(defResource MTrans/StateChange (|rdf|:|type| |owl|:|Class|)
  (|owl|:|intersectionOf|
   MTrans
   (|owl|:|Restriction| (|owl|:|onProperty| info)
                    (|owl|:|allValuesFrom| MObject/StateChange))))
(defResource MTrans/Econ (|rdf|:|type| |owl|:|Class|)
  (|owl|:|intersectionOf|
   MTrans
   (|owl|:|Restriction| (|owl|:|onProperty| actor)
                    (|owl|:|allValuesFrom| Economist))
   (|owl|:|Restriction| (|owl|:|onProperty| info)
                    (|owl|:|allValuesFrom| Arg/Econ))))

(defIndividual Arg.23 (|rdf|:|type| Arg/Mon))
(addForm '(MTrans (actor Friedman) (info Arg.23)))

|#

;;;
;;;
;;;

(cl:provide :swmop)
