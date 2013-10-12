;; -*- mode: common-lisp; syntax: common-lisp; package: judge; base: 10 -*-
;;;
;;; IT Program Project in Japan:
;;;          Building Operation-Support System for Large-scale System with IT
;;;
;;; Micro JUDGE Semantic Web OWL language version
;;;
;;; This code is rewritten by Seiji Koide from "Inside Case-Based
;;; Reasoning" by Christopher K. Riesbeck and Roger C. Schank, 1989,
;;; LEA, in order to run with SWMOP, the new version of MOP for Semantic Webs,
;;; Judge is presented by "Inside Case-Based Explanation" by Roger C. Schank,
;;; Alex Kass, and Christopher K. Riesbeck, 1994, LEA.
;;;
;;; Copyright (c) 2004, by Galaxy Express Corporation
;;; Copyright (c) 1989, by Lawrence Erlbaum Associates,Inc.
;;;
;;; History
;;; -------
;;; 2004.10.24    File created
;;;

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage judge
    (:shadowing-import-from :gx subtypep typep type-of subtypep typep type-of)
    (:use :common-lisp gx swmop)
    (:documentation "http://www.galaxy-express.co.jp/semweb/judge#"))
  )

(in-package judge)

;(require :swmop)

;;; ====================================================================
;;;   Micro Judge module for Semantic Webs
;;; ====================================================================
;;; Modified by Seiji Koide

;;;
;;; JUDGE Basic MOPs
;;;

;;; Actors

(defResource Actor (|rdf|:|type| |owl|:|Class|))
(defIndividual Al (|rdf|:|type| Actor)    (|rdfs|:|comment| "Al is an Actor."))
(defIndividual Chuck (|rdf|:|type| Actor) (|rdfs|:|comment| "Chuck is an Actor."))
(defIndividual David (|rdf|:|type| Actor) (|rdfs|:|comment| "David is an Actor."))
(defIndividual Randy (|rdf|:|type| Actor) (|rdfs|:|comment| "Randy is an Actor."))
(defIndividual Ted (|rdf|:|type| Actor)   (|rdfs|:|comment| "Ted is an Actor."))
(defIndividual Tim (|rdf|:|type| Actor)   (|rdfs|:|comment| "Tim is an Actor."))

;;; Motives

(defResource Motive (|rdf|:|type| |owl|:|Class|)          (|rdfs|:|comment| "There is a Motive."))
(defResource Justified (|rdf|:|type| |owl|:|Class|)
  (|rdfs|:|subClassOf| Motive)                        (|rdfs|:|comment| "Justified is a kind of Motive."))
(defResource Unjustified (|rdf|:|type| |owl|:|Class|)
  (|rdfs|:|subClassOf| Motive)                        (|rdfs|:|comment| "Unjustified is a kind of Motive."))
(defIndividual SelfDefence (|rdf|:|type| Justified)   (|rdfs|:|comment| "SelfDefence is a Justified."))
(defIndividual Retaliation (|rdf|:|type| Unjustified) (|rdfs|:|comment| "Retaliation is a Unjustified."))
(defIndividual Unprovoked (|rdf|:|type| Unjustified)  (|rdfs|:|comment| "Unprovoked is a Unjustified."))

;;; Crime

(defResource CrimeType (|rdf|:|type| |owl|:|Class|) (|rdfs|:|comment| "There is a CrimeType."))
(defIndividual Homicide (|rdf|:|type| CrimeType)(|rdfs|:|comment| "Homicide is a kind of CrimeType."))

;; Crime is a kind of Case
;;   and has a CrimeType, an Actor as defendant,
;;   an Actor as victim, an EventSequence as events,
;;   an OutcomeSequence as outcomes,
;;   and escalations, motives and sentence are calculated.
(defResource Crime (|rdf|:|type| |owl|:|Class|)
  (|owl|:|intersectionOf|
   Case
   (|owl|:|Restriction| (|owl|:|onProperty| crimeType)
                    (|owl|:|allValuesFrom| CrimeType)))
  (|rdfs|:|subClassOf| (|owl|:|Restriction| (|owl|:|onProperty| defendant)
                                    (|owl|:|allValuesFrom| Actor)))
  (|rdfs|:|subClassOf| (|owl|:|Restriction| (|owl|:|onProperty| victim)
                                    (|owl|:|allValuesFrom| Actor)))
  (|rdfs|:|subClassOf| (|owl|:|Restriction| (|owl|:|onProperty| events)
                                    (|owl|:|allValuesFrom| EventSequence)))
  (|rdfs|:|subClassOf| (|owl|:|Restriction| (|owl|:|onProperty| outcomes)
                                    (|owl|:|allValuesFrom| OutcomeSequence)))
  (|rdfs|:|subClassOf| (|owl|:|Restriction| (|owl|:|onProperty| escalations)
                                    (|owl|:|hasValue| (Pattern (calcFn calc-escalations)))))
  (|rdfs|:|subClassOf| (|owl|:|Restriction| (|owl|:|onProperty| motives)
                                    (|owl|:|hasValue| (Pattern (calcFn calc-motives)))))
  (|rdfs|:|subClassOf| (|owl|:|Restriction| (|owl|:|onProperty| sentence)
                                    (|owl|:|hasValue| (Pattern (calcFn adapt-sentence))))))

;;; Severity

(defProperty severity (|rdf|:|type| |owl|:|DatatypeProperty|)
  (|rdfs|:|range| |xsd|:|nonNegativeInteger|))

;;; Frequency

(defResource Frequency (|rdf|:|type| |rdfs|:|Class|)
  (|rdfs|:|comment| "Frequency has a severity."))
(defIndividual Once (|rdf|:|type| Frequency)
  (|rdfs|:|comment| "Once is a Frequency and has 0.")
  (severity 0))
(defIndividual SeveralTimes (|rdf|:|type| Frequency)
  (|rdfs|:|comment| "SeveralTimes has 1.")
  (severity 1))
(defIndividual Repeatedly (|rdf|:|type| Frequency)
  (|rdfs|:|comment| "Repeatedly has 2 as severity.")
  (severity 2))

;;;
;;; State
;;;

;; There is a State.
;; PhysState is a kind of State and has a severity.
;; Buised is a State and has severity 1.
;; KnockedDown is a State and has severity 2.
;; Cut is a State and has severity 3.
;; Dead is a State and has severity 5.

(defResource State (|rdf|:|type| |owl|:|Class|))
(defResource PhysState (|rdf|:|type| |owl|:|Class|)
  (|rdfs|:|subClassOf| State)
  (|rdfs|:|subClassOf| (|owl|:|Restriction| (|owl|:|onProperty| severity)
                                    (|owl|:|cardinality| 1))))
(defIndividual Buised (|rdf|:|type| PhysState)
  (severity 1))
(defIndividual KnockedDown (|rdf|:|type| PhysState)
  (severity 2))
(defIndividual Cut (|rdf|:|type| PhysState)
  (severity 3))
(defIndividual Dead (|rdf|:|type| PhysState)
  (severity 5))

;;;
;;; Outcome
;;;

;; There is an Outcome.
;; FightOutcome is a kind of Outcome
;;   and has PhysState as state and Actor as actor.

(defProperty state (|rdf|:|type| |owl|:|ObjectProperty|))
(defProperty actor (|rdf|:|type| |owl|:|ObjectProperty|))

(defResource Outcome (|rdf|:|type| |owl|:|Class|))
(defResource FightOutcome (|rdf|:|type| |owl|:|Class|)
  (|rdfs|:|subClassOf| Outcome)
  (|rdfs|:|subClassOf| (|owl|:|Restriction| (|owl|:|onProperty| state)
                                    (|owl|:|allValuesFrom| PhysState)))
  (|rdfs|:|subClassOf| (|owl|:|Restriction| (|owl|:|onProperty| actor)
                                    (|owl|:|allValuesFrom| Actor))))

;;; Range Constraint

(defResource RangeConstraint (|rdf|:|type| swmop:ConstraintFnClass)
  (|rdfs|:|subClassOf| swmop:ConstraintFn))

(defResource Range (|rdf|:|type| swmop::PatternClass)
  (|rdfs|:|subClassOf| swmop::PatternClass)
  (|rdfs|:|subClassOf|
   (|owl|:|Restriction| (|owl|:|onProperty| abstFn)
                    (|owl|:|hasValue| RangeConstraint)))
  (|rdfs|:|comment| "meta-class for Range class"))

(defProperty above (|rdf|:|type| |owl|:|DatatypeProperty|)
  (|rdfs|:|domain| Range)
  (|rdfs|:|range| |xsd|:|positiveInteger|))
(defProperty below (|rdf|:|type| |owl|:|DatatypeProperty|)
  (|rdfs|:|domain| Range)
  (|rdfs|:|range| |xsd|:|positiveInteger|))

(defun RangeConstraint (constraint filler)
  (and (numberp filler)
       (let ((below (-> constraint 'below))
             (above (-> constraint 'above)))
         (and (or (null below) (< filler below))
              (or (null above) (< above filler))))))

(defmethod gx::typep-using-class-in-owl (object (type Range))
  (let ((abst-fn (and (slot-boundp type 'abstFn) (slot-value type 'abstFn))))
    ;(format t "~%RANGE TYPEP ~S ~S ~S" object type abst-fn)
    (etypecase abst-fn
      (swmop:ConstraintFnClass (funcall (symbol-function (name abst-fn)) type object)))))

;;;
;;; Act
;;;

;; FightAct is a kind of Act and has a severity.
;; HurtAct is a kind of FightAct and has a severity below 5.
;; Slap is a HurtAct and has severity 1.
;; Hit is a HurtAct and has severity 1.
;; Strike is a HurtAct and has severity 2.
;; KnockDowm is a HurtAct and has severity 3.
;; SLASH is a HurtAct and has severity 4.

(defResource Act (|rdf|:|type| |owl|:|Class|))

(defResource FightAct (|rdf|:|type| |owl|:|Class|)
  (|owl|:|intersectionOf|
   Act
   (|owl|:|Restriction| (|owl|:|onProperty| severity)
                    (|owl|:|cardinality| 1))))

(defResource HurtAct (|rdf|:|type| |owl|:|Class|)
  (|owl|:|intersectionOf|
   FightAct
   (|owl|:|Restriction| (|owl|:|onProperty| severity)
                    (|owl|:|allValuesFrom| (Range (below 5))))))

(defIndividual Slap (|rdf|:|type| HurtAct)
  (severity 1))
(defIndividual Hit (|rdf|:|type| HurtAct)
  (severity 1))
(defIndividual Strike (|rdf|:|type| HurtAct)
  (severity 2))
(defIndividual KnockDowm (|rdf|:|type| HurtAct)
  (severity 3))
(defIndividual Slash (|rdf|:|type| HurtAct)
  (severity 4))

;; WoundAct is a kind of FightAct and has a severity above 4.
;; Stab is a WoundAct and has severity 5.
;; Shoot is a WoundAct and has severity 5.
;; BreakSkull is a WoundAct and has severity 5.

(defResource WoundAct (|rdf|:|type| |owl|:|Class|)
  (|rdfs|:|subClassOf| FightAct)
  (|rdfs|:|subClassOf| (|owl|:|Restriction| (|owl|:|onProperty| severity)
                                    (|owl|:|allValuesFrom| (Range (above 4))))))
(defIndividual Stab (|rdf|:|type| WoundAct)
  (severity 5))
(defIndividual Shoot (|rdf|:|type| WoundAct)
  (severity 5))
(defIndividual BreakSkull (|rdf|:|type| WoundAct)
  (severity 5))


;;;
;;; Events
;;;

(defResource Event (|rdf|:|type| |owl|:|Class|))

;; FightEvent is a kind of Event
;;   and has a FightAct as action.

(defProperty action (|rdf|:|type| |owl|:|ObjectProperty|))

(defResource FightEvent (|rdf|:|type| |owl|:|Class|)
  (|rdfs|:|subClassOf| Event)
  (|rdfs|:|subClassOf| (|owl|:|Restriction| (|owl|:|onProperty| action)
                                    (|owl|:|allValuesFrom| FightAct))))

;; EventSequence is a kind of |rdf|:|Seq| about events.
;; OutcomeSequence is a kind of |rdf|:|Seq| about outcomes.
;; EscalationSequence is a kind of |rdf|:|Seq| about ranges.
;; MotiveSequence is a kind of |rdf|:|Seq| about motives.

(defProperty |rdf|:|_1| (|rdf|:|type| |rdfs|:|ContainerMembershipProperty|))
(defProperty |rdf|:|_2| (|rdf|:|type| |rdfs|:|ContainerMembershipProperty|))
(defProperty |rdf|:|_3| (|rdf|:|type| |rdfs|:|ContainerMembershipProperty|))
(defProperty |rdf|:|_4| (|rdf|:|type| |rdfs|:|ContainerMembershipProperty|))
(defProperty |rdf|:|_5| (|rdf|:|type| |rdfs|:|ContainerMembershipProperty|))
(defProperty |rdf|:|_6| (|rdf|:|type| |rdfs|:|ContainerMembershipProperty|))
(defProperty |rdf|:|_7| (|rdf|:|type| |rdfs|:|ContainerMembershipProperty|))
(defProperty |rdf|:|_8| (|rdf|:|type| |rdfs|:|ContainerMembershipProperty|))
(defProperty |rdf|:|_9| (|rdf|:|type| |rdfs|:|ContainerMembershipProperty|))

(defResource EventSequence (|rdf|:|type| |owl|:|Class|)
  (|rdfs|:|subClassOf| |rdf|:|Seq|)
  (|rdfs|:|subClassOf| (|owl|:|Restriction| (|owl|:|onProperty| |rdf|:|_1|)
                                    (|owl|:|allValuesFrom| Event))))
(defResource OutcomeSequence (|rdf|:|type| |owl|:|Class|)
  (|rdfs|:|subClassOf| |rdf|:|Seq|)
  (|rdfs|:|subClassOf| (|owl|:|Restriction| (|owl|:|onProperty| |rdf|:|_1|)
                                    (|owl|:|allValuesFrom| Outcome))))

(defResource EscalationSequence (|rdf|:|type| |owl|:|Class|)
  (|rdfs|:|subClassOf| |rdf|:|Seq|)
  (|rdfs|:|subClassOf| (|owl|:|Restriction| (|owl|:|onProperty| |rdf|:|_1|)
                                    (|owl|:|allValuesFrom| Range))))

(defResource MotiveSequence (|rdf|:|type| |owl|:|Class|)
  (|rdfs|:|subClassOf| |rdf|:|Seq|)
  (|rdfs|:|subClassOf| (|owl|:|Restriction| (|owl|:|onProperty| |rdf|:|_1|)
                                    (|owl|:|allValuesFrom| Motive))))
;;;
;;; Calculation
;;;

(defun adapt-sentence (mop pattern)
  (declare (ignore pattern))
  "ADAPT-SENTENCE <pattern> <mop>
   gets another case with the same immediate abstraction as <mop>,
   and compares the actions, motives, and outcomes in the two cases,
   working backwards, until a difference is found and an adjusted
   sentence is calculated."
  (let ((old-mop (get-filler mop 'old)))
    (let ((old-size (sequence-size (get-filler old-mop events)))
          (size (sequence-size (get-filler mop events)))
          (old-sentence (get-filler old-mop sentence)))
      ;(format t "~&--------------")
      ;(format t "~&Adapting the sentence in ~S" (frame-name old-mop))
      (or (loop for old-pos downfrom old-size to 1
                for pos downfrom size to 1
            thereis (mop-calc
                      `((:role sentence) (:index ,(- size pos))
                        (:old-sentence ,old-sentence)
                        ,@(crime-compare-slots old-mop old-pos
                           `(:old-action :old-motive :old-severity))
                        ,@(crime-compare-slots mop pos
                           `(:this-action :this-motive :this-severity)))))
          (progn ;(format t "~&--------------------")
                 ;(format t "~&No major difference found")
                 ;(format t "~&Using old sentence")
            old-sentence)))))

(defun calc-escalations (mop pattern)
  (declare (ignore pattern))
  "CALC-ESCALATIONS <mop> <pattern>
   takes the sequence of events in <mop> and calculates a sequence of
   corresponding escalations"
  ;(format t "~&---------------")
  ;(format t "~&Calculating escalations in ~S" (name mop))
  (list->sequence
   (let ((prev-severity 0))
     (loop for event in (sequence->list (role-filler mop events))
       collect (let ((this-severity (path-filler event '(action severity))))
                  (let ((result (- this-severity prev-severity)))
                    (setf prev-severity this-severity)
                    result))))))

(defun calc-motives (mop pattern)
  (declare (ignore pattern))
  "CALC-MOTIVES <pattern> <mop>
   takes the sequence of events in <mop> and calculates a sequence of
   corresponding motives."
  ;(format t "~&---------------")
  ;(format t "~&Calculating motives in ~S" (frame-name mop))
  (list->sequence
   (let ((prev-motive 0))
     (loop for escalation in (sequence->list (get-filler mop escalations))
       collect (setf prev-motive
                     (mop-calc `((role motive)
                                 (:escalation ,escalation)
                                 (:prev-motive ,prev-motive))))))))

(defProperty motive (|rdf|:|type| |owl|:|ObjectProperty|))

#|
;; slots like ((role sentence) (:index ...) (:old-sentence ...)
;;             (:old-action ...) (:old-motive ...) (:old-severity ...)
;;             (:this-action ..) (:this-motive ..) (:this-severity ..))
;; is input in the adaptation.
(defun mop-calc (slots)
  "MOP-CALC <slot-list>
   finds the specialization of m-calc with the given slots and
   returns its :value filler."
  (let ((instance (addForm (cons 'm-calc slots))))
    (and instance (get-filler instance :value))))

(defResource m-calc (M-ROOT))

(defResource m-calc-motive (m-calc)
  :role :motive
  :value nil)

;; If the :role is :motive, and the :escalation is above 0,
;; then it is m-calc-escalation-motive, and its :value is Retaliation.
(defResource m-calc-escalation-motive (m-calc-motive)
  :escalation (Range (above 0))
  :value Retaliation)

;; If the :role is :motive, and the :escalation is above 1,
;; and :prev-motive is UNJUSTIFIED,
;; then it is m-calc-SelfDefence-motive, and its :value is SelfDefence.
(defResource m-calc-SelfDefence-motive (m-calc-motive)
  :escalation (Range (below 1))
  :prev-motive Unjustified
  :value SelfDefence)

;; If the :role is :motive, and the :escalation is below 1,
;; and :prev-motive is JUSTIFIED,
;; then it is m-calc-Retaliation-motive, and its :value is Retaliation.
(defResource m-calc-Retaliation-motive (m-calc-motive)
  :escalation (Range (below 1))
  :prev-motive Justified
  :value Retaliation)

(defun adjust-sentence (mop pattern)
  (declare (ignore pattern))
  "ADJUST-SENTENCE <pattern> <mop>
   is called by the sentence calculation MOPs to adjust the sentence
   appropriately. The slots in <mop> contain the old sentence, a
   wieghing factor (between 0.00 and 0.50) that says how critical
   the difference is, an index (between 0 and the number of events
   in the shortest fight) that says how close the difference was to
   the final act, and a :direction (1 or -1) that says which case is
   worse, the old one(-1) or the new one(1)."
  ;(format t "~&---------------")
  ;(format t "~&~S applied, ~S events from the end"
          (fr-name mop) (get-filler mop ':index))
  (adjust-fn
   (get-filler mop ':old-sentence)
   (get-filler mop ':weight)
   (get-filler mop ':index)
   (get-filler mop ':direction)))

(defun adjust-fn (x y index direction)
  "ADJUST-FN <sentence> <weight> <index> <direction>
   determines the :value of the new sentence, based on the old sentence,
   using the formula
       sentence + (sentence x (weight + closeness) x :direction)
   where closeness is 0.25 if <index> is 0 or 1, i.e., the last or
   sencond last events differed, or 0.00 if <index> >= 1."
  (+ x  (* x
           (+ y (cond ((< index 2) 0.25)
                      (t 0.0)))
           direction)))

(defun crime-compare-slots (mop pos roles)
  "CRIME-COMPARE-SLOTS <mop> <n> <role-list>
   returns a list of slots, where the first :role in <role-list> is
   filled with the action of the nth-last event, the second :role
   with the nth-last :motive, and the third :role with the severity
   of the nth-last outcome."
  (let ((paths `((events ,pos action)
                 (motives ,pos)
                 (outcomes ,pos state severity))))
    (assert (eql (length roles) (length paths)) ()
      "CRIME-COMPARE-SLOT: illegal length of roles ~S"
      roles)
    (loop for role in roles
          for path in paths
      collect (create-slot role (path-filler mop path)))))

;;;
;;; Comparison MOPs
;;;

(defResource compare-constraint (swmop:ConstraintFn))

(defResource m-compare (Pattern)                ; define generic function
  ;; You should know that (:to ...) slot is top level in slots.
  :abst-fn compare-constraint :to M-ROLE     ; method for generic
  :compare-fn M-FUNCTION)                    ; hands-on function

(defResource eql (M-FUNCTION))
(defResource < (M-FUNCTION))

(defResource m-equal (m-compare) :compare-fn eql)
(defResource m-less-than (m-compare) :compare-fn <)

(defun compare-constraint (constraint filler slots)
  "COMPARE-CONSTRAINT <constraint> <filler> <slots>
   applies the comparison function in <constraint> to <filler> and the filler
   of <role> in <slots>, where <role> is :role that fills the TO slot in
   <constraint>."
  (funcall (frame-name (get-filler constraint ':compare-fn))
           filler
           (indirect-filler ':to constraint slots)))

(defun indirect-filler (role mop slots)
  "INDIRECT-FILLER <role> <mop> <slots>
   gets the filler of <role> in <mop>, which should be a role, and
   gets the filler of that role in <slots>."
  (get-filler slots (fr-name (get-filler mop role))))

;;;
;;; Micro JUDGE MOPs for Adaptation
;;;

(defIndividual sentence (M-ROLE))
(defIndividual :old-severity (M-ROLE)) ; needed becase of (:to :old-severity)
(defIndividual :this-severity (M-ROLE)); needed becase of (:to :this-severity)

(defResource adjust-sentence (M-FUNCTION))

(defResource m-adapt-sentence (m-calc)
  :role sentence
  :value (Pattern (calcFn adjust-sentence)))

;;;
;;; Micro JUDGE Force and Motive Adaptation MOPs
;;;
;;; The most important thing is if every old-xxxx and this-xxxx
;;; is same each other, it does not match anything of the followings.

;; If :old-action is WoundAct, and :this-action is not WoundAct,
;; and :old-motive is Unjustified, and :this-motive is Unjustified, too,
;; then the m-adapt-sentence is m-adapt-extreme-force-old,
;; and the older is worse extremely.
(defResource m-adapt-extreme-force-old (m-adapt-sentence)
  :old-action WoundAct
  :this-action (M-NOT (object WoundAct))
  :old-motive Unjustified
  :this-motive Unjustified
  :weight 0.50
  :direction -1)

;; If :old-action is not WoundAct, :this-action is also WoundAct,
;; :old-motive is Unjustified, and :this-motive is Unjustified, too,
;; then the m-adapt-sentence is m-adapt-extreme-force-new,
;; and the newer is worse extremely.
(defResource m-adapt-extreme-force-new (m-adapt-sentence)
  :old-action (M-NOT (object WoundAct))
  :this-action WoundAct
  :old-motive Unjustified
  :this-motive Unjustified
  :weight 0.50
  :direction 1)

;; For any :old-severity,
;; if :this-severity is equal to the :old-severity,
;; and :old-motive is Unjustified, and :this-motive is Justified,
;; then the m-adapt-sentence is m-adapt-worse-motive-old,
;; and the older is worse.
(defResource m-adapt-worse-motive-old (m-adapt-sentence)
  :old-severity nil
  :this-severity (m-equal (:to :old-severity))
  :old-motive Unjustified
  :this-motive Justified
  :weight 0.25
  :direction -1)

;; For any :old-severity,
;; if :this-severity is equal to :old-severity,
;; and :old-motive is Justified, and :this-motive is Unjustified,
;; then the m-adapt-sentence is m-adapt-worse-motive-new,
;; and the newer is worse.
(defResource m-adapt-worse-motive-new (m-adapt-sentence)
  :old-severity nil
  :this-severity (m-equal (:to :old-severity))
  :old-motive Justified
  :this-motive Unjustified
  :weight 0.25
  :direction 1)

;;;
;;; Micro JUDGE Mixed Comparison Adaptation MOPs
;;;

;; For any :old-severity,
;; if :this-severity is less than :old-severity,
;; and :old-motive is Justified, and :this-motive is Unjustified,
;; then the m-adapt-sentence is m-adapt-mixed-old,
;; and the older is not so bad.
(defResource m-adapt-mixed-old (m-adapt-sentence)
  :old-severity nil
  :this-severity (m-less-than (:to :old-severity))
  :old-motive Justified
  :this-motive Unjustified
  :weight 0.00
  :direction -1)

;; For any :this-severity,
;; if :old-severity is less than :this-severity,
;; and :old-motive is Unjustified, and :this-motive is Justified,
;; then the m-adapt-sentence is m-adapt-mixed-new,
;; and the older is not so bad.
(defResource m-adapt-mixed-new (m-adapt-sentence)
  :this-severity nil
  :old-severity (m-less-than (:to :this-severity))
  :old-motive Unjustified
  :this-motive Justified
  :weight 0.00
  :direction -1)

(defun judge (slots)
  "JUDGE <slot-list>
   finds or creates a case under Crime with the given slots
   and returns it."
  (let ((instance (addForm (cons 'Crime slots))))
    (and (get-filler instance sentence)
         instance)))

;;;
;;; Demonstration
;;;

(setf *case1*
  '((crimeType Homicide)
    (defendant Ted) (victim Al)
    (events (EventSequence
             (|rdf|:|_1| (FightEvent
                      (action Slash)
                      (actor Ted) (object Al)
                      (freq Once)))
             (|rdf|:|_2| (FightEvent
                      (action Slash)
                      (actor Al) (object Ted)
                      (freq Once)))
             (|rdf|:|_3| (FightEvent
                      (action Stab)
                      (actor Ted) (object Al)
                      (freq Repeatedly)))))
    (outcomes (OutcomeSequence
               (|rdf|:|_1| (FightOutcome
                        (state Cut) (actor Al)))
               (|rdf|:|_2| (FightOutcome
                        (state Cut) (actor Ted)))
               (|rdf|:|_3| (FightOutcome
                        (state Dead) (actor Al)))))
    (sentence 40)))

(setf *case2*
  '((crimeType Homicide)
    (defendant Randy) (victim Chuck)
    (events (EventSequence
             (|rdf|:|_1| (FightEvent
                      (action Strike)
                      (actor Randy) (object Chuck)
                      (freq Repeatedly)))
             (|rdf|:|_2| (FightEvent
                      (action Strike)
                      (actor Chuck) (object Randy)
                      (freq Repeatedly)))
             (|rdf|:|_3| (FightEvent
                      (action Slash)
                      (actor Randy) (object Chuck)
                      (freq Once)))
             (|rdf|:|_4| (FightEvent
                      (action Slash)
                      (actor Chuck) (object Randy)
                      (freq Once)))
             (|rdf|:|_5| (FightEvent
                      (action Stab)
                      (actor Randy) (object Chuck)
                      (freq Repeatedly)))))
    (outcomes (OutcomeSequence
               (|rdf|:|_1| (FightOutcome
                        (state Buised) (actor Chuck)))
               (|rdf|:|_2| (FightOutcome
                        (state Buised) (actor Randy)))
               (|rdf|:|_3| (FightOutcome
                        (state Cut) (actor Chuck)))
               (|rdf|:|_4| (FightOutcome
                        (state Cut) (actor Randy)))
               (|rdf|:|_5| (FightOutcome
                        (state Dead) (actor Chuck)))))))

(setf *case3*
  '((crimeType Homicide)
    (defendant Tim) (victim David)
    (events (EventSequence
             (|rdf|:|_1| (FightEvent
                      (action Slap)
                      (actor David) (object Tim)
                      (freq SeveralTimes)))
             (|rdf|:|_2| (FightEvent
                      (action Strike)
                      (actor Tim) (object David)
                      (freq SeveralTimes)))
             (|rdf|:|_3| (FightEvent
                      (action KnockDowm)
                      (actor David) (object Tim)
                      (freq Once)))
             (|rdf|:|_4| (FightEvent
                      (action Stab)
                      (actor Tim) (object David)
                      (freq SeveralTimes)))))
    (outcomes OutcomeSequence
              (|rdf|:|_1| (FightOutcome
                       (state Buised) (actor Tim)))
              (|rdf|:|_2| (FightOutcome
                       (state Buised) (state David)))
              (|rdf|:|_3| (FightOutcome
                       (state KnockedDown) (actor Tim)))
              (|rdf|:|_4| (FightOutcome
                       (state Dead) (state David))))))

(defun judge-demo ()
  (run-judge *case1* '*case1* 'first)
  (run-judge *case2* '*case2* 'second)
  (run-judge *case3* '*case3* 'third))

(defun run-judge (case case-name iter)
  ;(format t "~&---------------")
  ;(format t "~&Sentencing ~S in ~S"
            (role-filler case defendant) case-name)
  (let ((instance (judge case)))
    (assert (not (null instance)) () "RUN-JUDGE: null instance.")
    ;(format t "~&Sentence in ~S ~S is ~S years"
              iter (name (class-of instance)) (role-filler instance 'sentence))
    instance))

;;; To demonstrate, type "judge-demo"
|#
