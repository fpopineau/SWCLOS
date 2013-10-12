(defpackage :closette
  (:use :common-lisp :mop)
  (:export std-compute-class-precedence-list std-sort-class-list))

(in-package :closette)

(defun mapappend (fun &rest args)
  (if (some #'null args)
      ()
      (append (apply fun (mapcar #'car args))
              (apply #'mapappend fun (mapcar #'cdr args)))))
;;; Class precedence lists

(defun std-compute-class-precedence-list (class)
  (let ((classes-to-order (collect-superclasses* class)))
    (topological-sort classes-to-order
                      (remove-duplicates
                        (mapappend #'local-precedence-ordering
                                   classes-to-order))
                      #'std-tie-breaker-rule)))


(defun std-sort-class-list (classes-to-order)
  (topological-sort classes-to-order
                    (remove-duplicates
                     (mapappend #'local-precedence-ordering
                                classes-to-order))
                    #'global-tie-breaker-rule))

;;; topological-sort implements the standard algorithm for topologically
;;; sorting an arbitrary set of elements while honoring the precedence
;;; constraints given by a set of (X,Y) pairs that indicate that element
;;; X must precede element Y.  The tie-breaker procedure is called when it
;;; is necessary to choose from multiple minimal elements; both a list of
;;; candidates and the ordering so far are provided as arguments.

(defun topological-sort (elements constraints tie-breaker)
  (let ((remaining-constraints constraints)
        (remaining-elements elements)
        (result ()))
    (loop
     (let ((minimal-elements
            (remove-if
             #'(lambda (class)
                 (member class remaining-constraints
                         :key #'cadr))
             remaining-elements)))
       (when (null minimal-elements)
             (if (null remaining-elements)
                 (return-from topological-sort result)
               (error "Inconsistent precedence graph.")))
       (let ((choice (if (null (cdr minimal-elements))
                         (car minimal-elements)
                       (funcall tie-breaker
                                minimal-elements
                                result))))
         (setq result (append result (list choice)))
         (setq remaining-elements
               (remove choice remaining-elements))
         (setq remaining-constraints
               (remove choice
                       remaining-constraints
                       :test #'member)))))))

;;; In the event of a tie while topologically sorting class precedence lists,
;;; the CLOS Specification says to "select the one that has a direct subclass
;;; rightmost in the class precedence list computed so far."  The same result
;;; is obtained by inspecting the partially constructed class precedence list
;;; from right to left, looking for the first minimal element to show up among
;;; the direct superclasses of the class precedence list constituent.
;;; (There's a lemma that shows that this rule yields a unique result.)

(defun std-tie-breaker-rule (minimal-elements cpl-so-far)
  (dolist (cpl-constituent (reverse cpl-so-far))
    (let* ((supers (class-direct-superclasses cpl-constituent))
           (common (intersection minimal-elements supers)))
      (when (not (null common))
        (return-from std-tie-breaker-rule (car common))))))

(defun global-tie-breaker-rule (minimal-elements cpl-so-far)
  #+nil (format t "minimal-elements ~S cpl-so-far ~S~%" minimal-elements cpl-so-far)
  (dolist (cpl-constituent (reverse cpl-so-far))
    (when cpl-constituent
    (let* ((supers (class-direct-superclasses cpl-constituent))
           (common (intersection minimal-elements supers)))
      (when (not (null common))
        (return-from global-tie-breaker-rule (car common))))))
  (car minimal-elements))


;;; This version of collect-superclasses* isn't bothered by cycles in the class
;;; hierarchy, which sometimes happen by accident.

(defun collect-superclasses* (class)
  (labels ((all-superclasses-loop (seen superclasses)
              (let ((to-be-processed
                       (set-difference superclasses seen)))
                (if (null to-be-processed)
                    superclasses
                    (let ((class-to-process
                             (car to-be-processed)))
                      (all-superclasses-loop
                        (cons class-to-process seen)
                        (union (class-direct-superclasses
                                 class-to-process)
                               superclasses)))))))
    (all-superclasses-loop () (list class))))

;;; The local precedence ordering of a class C with direct superclasses C_1,
;;; C_2, ..., C_n is the set ((C C_1) (C_1 C_2) ...(C_n-1 C_n)).

(defun local-precedence-ordering (class)
  (mapcar #'list
          (cons class
                (butlast (class-direct-superclasses class)))
          (class-direct-superclasses class)))

(in-package :cl-user)

#+nil
(defmethod clos:compute-class-precedence-list ((class standard-class))
  (format t "Rewiring compute-class-precedence-list for class ~S => ~S~%" class
            (closette::std-compute-class-precedence-list class))
  (closette::std-compute-class-precedence-list class))

