;;; for Demonstration at SWO
;;; Prolog Programming
(require :prolog)
(in-package :prolog)
;;;---------------------------------------------------------
;; database
(<-- (Knows John Jane))
(<- (Knows ?y Leonid))
(<- (Knows ?x Elizabeth))
(<- (Human John))
(<- (Human Jane))
(<- (Human Leonid))
(<- (Cat Elizabeth))
;; rules
(<- (Hates John ?x) (Human ?x) (Knows John ?x))
(<- (Likes John ?x) (Cat ?x) (Knows John ?x))
;; query
(?- (Hates John ?x))
(?- (Likes ?x ?y))
;;;----------------------------------------------------------




