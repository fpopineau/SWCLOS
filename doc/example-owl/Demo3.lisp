;;; Demonstration3 for SWO
(satisfied-p '(()) `(Intersect ,Human ,Cat))
(unsatisfied-p '(())  `(Intersect ,Human ,Cat))
;;; Load Food&Wine Ontologies
;; Atomic
(satisfied-p '(()) 'vin:Region)
(satisfied-p '(()) vin:Region)
(satisfied-p '(()) '(Not vin:Region))
(satisfied-p '(()) '(Intersect vin:Region
                               (Not vin:Region)))
(satisfied-p '(()) `(Intersect ,vin:Region
                               (Not ,vin:Region)))
;; Complements
(satisfied-p '(()) `(Intersect ,food:ConsumableThing
                               ,food:NonConsumableThing))
(satisfied-p '(()) '(Intersect food:ConsumableThing
                               food:NonConsumableThing))
;; Disjoints
(satisfied-p '(()) '(Intersect vin:EarlyHarvest
                               vin:LateHarvest))
;; Intersects
(satisfied-p '(()) 'vin:Zinfandel)
(satisfied-p '(()) 'vin:RedWine)
(satisfied-p '(()) '(Intersect vin:Wine vin:RedWine))
(satisfied-p '(()) `(Intersect ,vin:WhiteWine
                               ,vin:RedWine))
(satisfied-p '(()) `(Intersect ,vin:WineBody
                               ,vin:WineSugar))
(unsatisfied-p '(()) `(Intersect ,vin:WineBody
                                 ,vin:WineSugar))
(satisfied-p '(()) `(Intersect ,vin:WineTaste
                               ,vin:WineSugar))
(satisfied-p '(()) `(Intersect ,vin:WineTaste
                               ,vin:WineColor))
;;owl:unionOf
(satisfied-p '(()) vin:WineDescriptor)












(satisfied-p '(()) `(Intersect ,vin:WineDescriptor
                               ,vin:WineTaste))
(satisfied-p '(()) `(Intersect ,vin:WineDescriptor
                               ,vin:WineBody))

(satisfied-p '(()) `(Intersect ,|owl|:|Thing|
                               ,|owl|:|Nothing|))

(satisfied-p '(()) `(Intersect ,vin:RedWine
                               (Not ,vin:Wine)))
