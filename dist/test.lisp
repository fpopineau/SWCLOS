(asdf:operate 'asdf:load-op :rdf)
(asdf:operate 'asdf:load-op :rdfs)
(asdf:operate 'asdf:load-op :owl)

(cd "LUBM")

(in-package :gx-user)

;; (trace gx::%compute-effective-slot-definition-initargs)
;; (trace (method mop::compute-effective-slot-definition-initargs (|rdfs|:|Class| t t)))
;; (trace (method mop::effective-slot-definition-class (|rdfs|:|Class|)))
;; (trace (method mop::direct-slot-definition-class (|rdfs|:|Class|)))
;; (trace gx::addForm gx::addInstance)

;; (trace (method mop::shared-initialize :after Property-effective-slot-definition)

(defun lubm ()
  (lw:set-default-character-element-type 'lw:simple-char)
  (load "load-lubm.cl")
  (load "queries.cl")
  (time
   (let ((*error-output* cl:nil)
	 (*autoepistemic-local-closed-world* cl:nil))
     (load-lubm10ver17)
   ))
  (query))
  
(lubm)
