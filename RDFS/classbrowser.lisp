;; -=Begin Copyright Notice=-
;; copyright (c) 1986-2004 Franz Inc, Oakland, CA  All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;; -=End Copyright Notice=-
;;; $Id: classbrowser.cl,v 1.5 2004/02/14 08:23:47 cheetham Exp $ 

;;; The Class Browser Dialog

(in-package :ide.class-browser)
;(export '#:display-resource-outline)

(defparameter *resource-outline-subject-color* ;; bug11670
  (make-instance 'light-or-dark-color
    :hue-symbol 'cyan
    :base-lightness-when-light 128
    :lightness-when-dark 128))
  
(defparameter *resource-outline-parent-color* ;; bug11670
  (make-instance 'light-or-dark-color
    :hue-symbol 'blue
    :base-lightness-when-light 224
    :lightness-when-dark 96))

(defparameter *resource-outline-multiple-parent-color*
  (make-instance 'light-or-dark-color
    :hue-symbol 'red
    :base-lightness-when-light 0
    :lightness-when-dark 255))
  
(defparameter *resource-outline-noted-parent-color*
  (make-instance 'light-or-dark-color
    :hue-symbol 'red
    :base-lightness-when-light 192
    :lightness-when-dark 160))
  
;;; chee   19sep03 use the ide-outline mixin
(defclass resource-outline (ide-outline outline)
  ()
  (:default-initargs
      :range nil))

(defclass resource-outline-pane (outline-top-pane)()) ;; spr18613

(defmethod widget-device ((item resource-outline) dialog) ;; spr18613
  (declare (ignore dialog))
  'resource-outline-pane)

(defmethod selected-string ((outline-pane resource-outline-pane))
  ;; Don't include the package qualifier in the Find In Files
  ;; string widget when invoking it from the class browser dialog.
  (princ-to-string (selected-symbol outline-pane)))

(defclass resource-outline-dialog (shrinking-dialog) nil)

(defmethod default-width ((window resource-outline-dialog))
  (let ((interior 581))
    (+ interior (position-x (minimum-size window)))))

(defmethod default-height ((window resource-outline-dialog))
  (let ((interior 280)) ;; cg1768
    (+ interior (position-y (minimum-size window)))))
   
(defmethod title-for-windows-menu ((window resource-outline-dialog))
  "Class Browser")

(defmethod resource-outline-key (value)
  (let* ((supers (class-direct-superclasses value))
         (*print-case* :downcase)) ;; bug7743c
    (format nil "~a~a"
      (class-name value)
      (if (cdr supers)
          (format nil "   ~a" (length supers))
        ""))))

(defun make-resource-outline-item (class)
  (let* ((requested-class (getf (plist (resource-outline))
                                :displayed-class))
         (rangep (and (class-direct-subclasses class) t)))
    (make-instance 'outline-item 
      :value class 
      :kind nil 
      :state (if rangep :partially-open :open)
      :has-range-on-open rangep ;; bug11140
      :range nil
      :background-color
      (cond ((eq requested-class class)
             *resource-outline-subject-color*)) ;; bug11670
      :foreground-color
      (cond ((eq requested-class class)
             *resource-outline-subject-color*) ;; bug11670
            ((subtypep requested-class class)
             *resource-outline-parent-color* :foregroundp t) ;; bug11670
            ((cdr (class-direct-superclasses class))
             *resource-outline-multiple-parent-color* ;; bug11670
             :foregroundp t)))))

(defvar *resource-outline-initial-display* nil)

(defun resource-outline-initial-class-p (class)
  (or (subtypep *resource-outline-initial-display* class)
      (eq class *resource-outline-initial-display*)
      (subtypep class *resource-outline-initial-display*)))

(defun class-sorter (class1 class2)
  (string< (class-name class1)(class-name class2)))

(defmethod range-on-open ((outline resource-outline) item-value range)
  (let* ((subclasses (class-direct-subclasses item-value)))
    
    ;; Initially do not display the siblings of the ancestors
    ;; of the selected class (so they are "partially open").
    (when *resource-outline-initial-display*
      (setq subclasses (remove-if-not #'resource-outline-initial-class-p
                                      subclasses)))
    (sort (remove-duplicates
           
           ;; When fully-opening a partially open ancestor,
           ;; keep any partially-open range open, and open
           ;; the other range also.
           (append (mapcar #'make-resource-outline-item subclasses)
                   range)
           :key #'value)
          #'class-sorter :key #'value)))

(defmethod range-on-close ((outline resource-outline) item-value range)
  (declare (ignore range item-value))
  
  ;; If return range as in default method, apparently
  ;; they get re-opened in :closed state and the leafs
  ;; get the plus icon.
  nil)

(defmethod browse-class-command ((window cg-stream))
  (or (pass-to-child window 'browse-class-command)
      (display-resource-outline
       (selected-class window))))

(defun display-resource-outline (&optional class)
  (let* ((dialog (resource-outline))
         (outline (find-component :outline dialog))
         (history (find-component :history dialog))
         (old-value (value outline))
         class-name item index package)
    (unless (and class ;; cg1333
                 (or (typep class 'class)
                     (and (symbolp class)
                          (setq class (find-class class)))
                     (setq class (find-class (object-class class)
                                             nil))))
      
      ;; Allow the user to simply bring up the class outline
      ;; on the previously-displayed class.
      (select-window dialog)
      
      ;; If no class has yet been displayed, show class "t".
      (unless (getf (plist dialog) :displayed-class)
        (display-resource-outline t))
      
      (set-focus-component history)
      (return-from display-resource-outline nil))
    (setq class-name (class-name class))
    (setf (getf (plist dialog) :displayed-class) class)
    (setq package (symbol-package class-name))
    (lisp-message "Preparing outline for ~a class ~a ..."
      (if package (name-string package) "uninterned")
      class-name)
    
    ;; This is needed to make it scroll to the new value
    ;; when it's the same as before.
    (when (eq old-value class)
      (setf (value outline t) nil))
    
    (with-hourglass
      (with-delayed-redraw (dialog :invalidate t :invalidate-frame t
                                   :invalidate-children t) ;; cg1161
        (setf (title dialog)
          (format nil "Class Browser for ~a" class-name))
        (setf (range outline)
          (list (setq item (make-instance 'outline-item
                             :value (find-class t)))))
        (cond ((eq class-name t)
               (outline-item-toggle-open outline item nil :open)
               (setf (value outline)
                 (find-class t)))
              (t
               (resource-outline-recursion outline (list class)
                                        class class)
               (setf (value outline) class)
               (invalidate outline :erase nil)
               (setq index (or (focus-index outline) 0))
               (scroll-indices-into-view
                outline index
                (+ index (length (class-direct-subclasses class))))))
        (update-page-size (image-pane outline))
        (scroll-current-index-into-view outline)
        (unless (member class-name (range history) :test #'eq)
          (setf (range history)
            (sort (cons class-name
                        (copy-list (range history)))
                  #'string-lessp)))
        (setf (value history) class-name)
        ) ;; with-delayed-redraw
      
      (set-focus-component outline)
      (unless (eq dialog
                  (selected-window (development-main-window *system*)))
        (select-window dialog)
        (set-focus-component outline))
      (let* ((*print-nickname* t))
        (lisp-message "~a     ~
                       Cyan background = the requested class     ~
                       Blue text = a superclass     ~
                       Red text = has multiple superclasses"
          class-name)))))

(defun resource-outline-recursion
    (outline super-list class original-class)
  (let* ((supers (class-direct-superclasses class)))
    (cond (supers
           (dolist (super supers)
             (resource-outline-recursion outline
                                      (cons super super-list)
                                      super original-class)))
          (t
           (do* ((*resource-outline-initial-display* original-class)
                 (supers super-list (cdr supers))
                 (items (range outline)
                        (and super-item (range super-item)))
                 (super-item nil)
                 opening-requested-class-now)
                ((or (null supers)
                     (null items)))
             (unless (or super-item
                         (eq t (class-name class)))
               (setf (value (car items))
                 class))
             (setq super-item (find (car supers) items
                                    :key #'value
                                    :test #'eq))
             (setq opening-requested-class-now
                   (and super-item ;; bug11587
                        (eq (value super-item)
                            original-class)))
             (when super-item
               (when opening-requested-class-now
                 (setf (background-color super-item)
                   *resource-outline-subject-color*)) ;; bug11670
               (setf (foreground-color super-item)
                 *resource-outline-parent-color*) ;; bug11670
               
               ;; Don't initially show the sub-items of the requested
               ;; class, since this takes up a lot of space when
               ;; the requested class appears several times due
               ;; to multiple parents.
               (when opening-requested-class-now ;; cg1297
                 (setf (range super-item)
                   (range-on-open outline (value super-item) nil)))
               (outline-item-toggle-open
                outline super-item nil
                (if opening-requested-class-now
                    :closed :partially-open)
                t ;; no-redisplay-p
                nil ;; recurse-p
                t ;; no-focus-p ;; cg961
                t ;; no-scroll-p
                t ;; no-update-scroll-range
                )))))))

;;; chee   12mar03 bug6895b decided to once again make the class browser
;;;        finalize the selected class when it was not already finalized,
;;;        now that I have removed the automatic finalization of all CG
;;;        classes that were defined with defcomponent and I see how
;;;        annoying it is not to see the class slots; but this time
;;;        use an ignore-errors to avoid breaks when, for example, there
;;;        is a forward-referenced superclass
(defun resource-outline-on-change (outline new old)
  (declare (ignore old))
  (when (and (windowp (parent outline)) new)
    (unless (class-finalized-p new) ;; bug6895b
      (ignore-errors (finalize-inheritance new)))
    (let* ((doc (documentation new 'type)))
      (if* doc
         then (lisp-message "~a     ~a" (class-name new) doc)
         else (quick-class-message new)))
    (let* ((dialog (parent outline))
           (tab (find-component :tab dialog))
           (class (value outline)) ;; cg1768
           (changed nil)
           (properties nil)
           (event-handlers nil))
      (when (class-finalized-p class) ;; bug6895
        (setq properties (non-event-properties class))
        (setq event-handlers (event-handlers class)))
      (if properties ;; cg1768
          (unless (find-tab tab :properties)
            (restore-tab tab (get-stream-prop dialog :properties-tab))
            (setq changed t))
        (when (find-tab tab :properties)
          (set-stream-prop dialog :properties-tab
                           (remove-tab tab :properties))
          (setq changed t)))
      (if event-handlers
          (unless (find-tab tab :events)
            (restore-tab tab
                         (get-stream-prop dialog :events-tab))
            (setq changed t))
        (when (find-tab tab :events)
          (set-stream-prop dialog :events-tab
                           (remove-tab tab :events))
          (setq changed t)))
      (when changed
        (fit-resource-outline-list-to-tabs dialog)
        
        ;; If removing a tab caused a new tab to be set, that
        ;; focused on the tab's list widget, so set it back.
        (set-focus-component outline)))
    
    (update-resource-outline-list))
  t)

(defun resource-outline-history-on-change
    (widget new-value old-value)
  (declare (ignore old-value))
  (let* ((dialog (parent widget))
         (*no-pop-up-in-object-symbol* t))
    (typecase new-value
      
      ;; User selected this from the history list.
      (symbol
       
       ;; Make sure it's not instead passed in by selecting a
       ;; class somewhere else on the dialog.
       (unless (eq (find-class new-value nil)
                   (getf (plist dialog)
                         :displayed-class))
         
         (resource-outline-action dialog)))
      (string
       (let* ((*package* (stream-package dialog)) ;; bug9838d
              (class (object-class new-value)))
         
         ;; Remember that this will no longer show the message
         ;; when the user has typed the last character of a class
         ;; (before pressing enter), because the alternative is
         ;; to use delayed=nil which annoyingly makes it also
         ;; generate the tree for a class that just happens to
         ;; begin the name of the class that is being typed.
         (when (and class
                    (symbolp class) ;; not e.g. (simple-string 100)
                    (setq class (find-class class nil))) ;; bug4323
           (quick-class-message class)))))
    t))

(defun class-tab-on-change (tab new old)
  (declare (ignore old))
  (when (and (windowp (parent tab)) new)
    (update-resource-outline-list))
  (lisp-message
      (case new
        ((:supers :subs :precedence)
         #.(format nil "To regenerate the outline on one of these ~
                        classes, either double-click it or select it ~
                        and press ENTER."))
        (:slots
         #.(format nil "Direct slots (slots defined directly on ~
                        this class rather than its superclasses)."))
        (:all-slots
         #.(format nil "All slots (slots defined on this class ~
                        or any of its superclasses)"))
        ((:methods :all-methods)
         #.(format nil "To edit one of these methods, either ~
                        double-click it or select it and press ~
                        ENTER.  Use the find-definition or ~
                        browse-methods gestures to see other ~
                        methods of these generic functions."))
        (t "")))
  t)

(defun structure-slot-sorter (a b)
  (and a b (string< a b)))

(defun structure-name-p (class-name) ;; cg1125
  (let ((class (find-class class-name nil)))
    (when (and class (typep class 'structure-class))
      class)))

(defmethod all-specializer-direct-methods ((class class))
  (let* ((methods nil))
    (dolist (class (class-precedence-list class))
      (unless (eq (class-name class) t)
	(setf methods (append methods
			      (specializer-direct-methods class)))))
    methods))

(defun update-resource-outline-list ()
  (let* ((dialog (find-application-window :resource-outline)))
    (when dialog
      (let* ((outline (find-component :outline dialog))
             (tab (find-component :tab dialog))
             (tab-value (value tab))
             (list (find-component :list dialog))
             (class (value outline))
             class-name)
        (unless (and class tab-value)
          (return-from update-resource-outline-list))
        (setq class-name (class-name class))
        (setf (value list) nil)
        (setf (range list)
          (case tab-value
            (:supers
             (sorted (class-direct-superclasses class)
                     #'string< :key #'class-name))
            (:subs
             (sorted (class-direct-subclasses class)
                     #'string< :key #'class-name))
            (:precedence
             (cond ((class-finalized-p class) ;; bug6895
                    (class-precedence-list class))
                   (t
                    (lisp-message "Cannot find the precedence list ~
                                   for a non-finalized class.")
                    nil)))
            (:slots
             (typecase class
               (standard-class
                (if (class-finalized-p class) ;; cg903
                    (sorted-slots class :direct-only-p t)
                  (class-direct-slots class))) ;; bug6895
               (structure-class
                (and (structure-name-p class-name)
                     (sorted-slots class)
                     ))))
            (:all-slots
             (typecase class
               (standard-class
                (and (class-finalized-p class) ;; cg903 bug6895
                     (sorted-slots class)))
               (structure-class
                ;; PATHNAME fails this test
                (and (structure-name-p class-name)
                     (sorted-slots class)))))
            
            (:methods
             
             ;; Avoid showing all the methods for T, which takes
             ;; a long time; do this especially since the graph
             ;; for T often comes up due to no class being selected.
             (if (eq (class-name class) t)
                 nil
               
               ;; I think that using SORTED failed here presumably
               ;; because the list is destructively modified as
               ;; methods are added or removed, so the hash table
               ;; is not decached --- so use SORT every time.
               (with-hourglass
                 (sort (copy-list (specializer-direct-methods class))
                       #'string< :key #'method-name))))
            
            (:all-methods
             
             ;; Avoid showing all the methods for T, which takes
             ;; a long time; do this especially since the graph
             ;; for T often comes up due to no class being selected.
             (if (eq (class-name class) t)
                 nil
               
               (with-hourglass
                 (cond ((class-finalized-p class) ;; cg903
                        (sort (copy-list 
                               (all-specializer-direct-methods class))
                              #'string< :key #'method-name))
                       (t ;; bug6895
                        (lisp-message "Cannot find all methods ~
                                       for a non-finalized class.")
                        nil)))))
            (:properties ;; cg1768
             (non-event-properties class))
            (:events
             (event-handlers class))))))))
   
(defun resource-outline-list-on-double-click (dialog list)
   (declare (ignore list))
   (resource-outline-action dialog)
   t)
   
(defun resource-outline-default-button-fn (button new old)
  (declare (ignore new old))
  (resource-outline-action (parent button))
  t)

(defun resource-outline-action (resource-outline-dialog)
  (let* ((wij (focus-component))
         (value (and wij (value wij))))
    (typecase value
      (string (let* ((*package* (stream-package ;; bug9838d
                                 resource-outline-dialog)))
                (setq value (find-class
                             (object-class value) nil)))
              (unless value
                (lisp-warning "~s does not name a class."
                              (value wij))))
      (symbol (setq value (find-class value nil))))
    (typecase value
      (class (display-resource-outline value))
      (slot-definition (inspect value))
      (cons (inspect value)) ;; a structure-slot
      (method (run-ide-hook show-source-code-hook value))
      (property (inspect value))))) ;; cg1768

;;; chee   09dec03 bug13787d new
(defclass resource-outline-list (single-item-list)())

;;; chee   09dec03 bug13787d new
(defclass resource-outline-list-pane (single-item-list-pane)())

;;; chee   09dec03 bug13787d new
(defmethod widget-device ((widget resource-outline-list) dialog)
  (declare (ignore dialog))
  'resource-outline-list-pane)

;;; chee   09dec03 bug13787d new
(defmethod shortcut-commands ((window resource-outline-list-pane)
                              (menu development-shortcut-menu))
  (let* ((items (call-next-method)))
    (when (typep (value (dialog-item window)) 'method)
      (nconc items (list menu-separator
                         (make-instance 'menu-item
                           :name :delete-command
                           :title "Remove Method"
                           :event-synonym 'vk-delete
                           :value 'delete-command))))
    items))

;;; chee   09dec03 bug13787d new; allow removing a method
;;;        in the class browser
(defmethod delete-selection ((widget-window resource-outline-list-pane))
  (let* ((widget (dialog-item widget-window))
         (value (value widget)))
    (when (and (typep value 'method)
               (y-or-n-dialog
                "Do you really want to remove the ~
                   method ~a from your lisp environment?"
                (prin1-to-string value)))
      (remove-method (method-generic-function value) value)
      (call-next-method))))

(defun resource-outline-list-on-change (widget new-value old-value)
  (let* ((*print-nickname* t)) ;; cg1591
    (typecase new-value
      (class
       (quick-class-message new-value))
      (method
       
       ;; If a method has been redefined since it was displayed here,
       ;; then regenerate this list of methods to get the new one.
       (unless (method-generic-function new-value) ;; cg2687
         (pop-up-information-message (parent widget) "Method Removed"
           (format nil "That method is no longer valid (most likely ~
                due to it being recompiled).  This method list will ~
                now be regenerated to get the current set of methods, ~
                and you need to select the method again."
             :warn-on-no-action-taken t :beep nil))
         (update-resource-outline-list))
       (let* ((method-name (method-name new-value))
              (package (symbol-package method-name)))
         (lisp-message "~a~a method ~a" ;; bug4302
           (if (and package
                    (external-symbol-p method-name package))
               "external "
             "")
           (package-short-name package)
           (name-string new-value))))
      (slot-definition
       (info-message new-value))
      (cons ;; a structure-slot
       (lisp-message "~a slot   ~a       accessor   ~s       ~
                      default value   ~sType~sRead-Only?   ~s"
         (name-string (symbol-package (first new-value)))
         (first new-value)(second new-value)(third new-value)
         (fourth new-value)(fifth new-value)))
      (property ;; cg1768
       (lisp-message "~a" (help-string new-value))))
    (when (typep (or new-value old-value) 'slot-definition) ;; rfe5056
      (let* ((outline (find-sibling :outline widget)))
        (highlight-classes
         outline (range outline)
         :slot-name (and new-value
                         (slot-definition-name new-value)))))
    (when (typep (or new-value old-value) 'method)
      (let* ((outline (find-sibling :outline widget)))
        (highlight-classes
         outline (range outline)
         :specializers (and new-value
                            (method-specializers new-value)))))
    t))

(defun highlight-classes (outline range &key slot-name specializers)
  (let* ((highlight-color *resource-outline-noted-parent-color*) ;; bug11670
         old-color value old-on new-on)
    
    ;; Make the class browser outline highlight the classes
    ;; that define a selected slot.
    (dolist (item range) ;; rfe5056
      (setq value (value item))
      (setq old-color (background-color item))
      (setq old-on (getf (plist item) :specially-highlighted))
      (setq new-on (and (or slot-name specializers)
                        (typep value 'standard-class)
                        (if* specializers ;; bug11670
                           then (member value specializers :test #'eq)
                           else (member slot-name
                                        (class-direct-slots value)
                                        :key #'slot-definition-name
                                        :test #'eq))))
      (when (xor new-on old-on)
        (setf (getf (plist item) :specially-highlighted)
          (and new-on t))
        (when new-on
          (setf (getf (plist item) :real-highlight-color) old-color))
        (setf (background-color item)
          (if new-on
              highlight-color
            (getf (plist item) :real-highlight-color))))
      (highlight-classes outline (range item)
                         :slot-name slot-name
                         :specializers specializers))))

(defmethod info-message ((slot-definition slot-definition)) ;; cg2457
  (let* ((slotname (slot-definition-name slot-definition)))
    (lisp-message "~a slot"
      (package-short-name (symbol-package slotname))
      slotname)))

(defmethod info-message ((slot-definition standard-slot-definition))
  (let* ((slotname (slot-definition-name slot-definition))
         (stuff
          `((initargs ,(slot-definition-initargs slot-definition))
            (initform ,(slot-definition-initform slot-definition))
            (readers ,(and (typep slot-definition
                                  'direct-slot-definition)
                           (slot-definition-readers
                            slot-definition)))
            (writers ,(and (typep slot-definition
                                  'direct-slot-definition)
                           (slot-definition-writers
                            slot-definition)))
            #+maybe ;; breaks
            (location ,(slot-definition-location slot-definition
                                                 class))
            (allocation ,(slot-definition-allocation slot-definition))
            (type ,(slot-definition-type slot-definition)))))
    (setq stuff (delete nil stuff :test #'eq :key #'second))
    (lisp-message "~a slot   ~:@(~a~)~{       ~a~}"
      (package-short-name (symbol-package slotname))
      slotname
      (mapcar #'(lambda (a)
                  (format nil "~a  ~a"
                    (first a)
                    (format nil (if (and (not (eq (first a)
                                                  'initform))
                                         (listp (second a)))
                                    "~{~s~^ ~}" "~s")
                      (second a))))
        stuff))))

(defmethod name ((object slot-definition)) ;; cg903
  (slot-definition-name object))

(defun resource-outline-list-key (object)
  (lowercase-object 
   (typecase object
     (method (name-string object))
     (cons (first object)) ;; a structure-slot
     (t (name object)))))

(defmethod name-string ((method standard-method))
  
  ;; Copied from closbrfn.cl in gr-cg package.
  (let ((gf (method-generic-function method))
        (specs (method-specializers method))
        (quals (method-qualifiers method)))
    (with-output-to-string (s)
      (if* gf
         then (format s "~a " (generic-function-name gf))
         else (write-string "unnamed " s))
      (dolist (qual quals)
        (format s "~s " qual))
      (write-char #\( s)
      (loop
        (unless specs (return))
        (let ((spec (pop specs)))
          (if* (consp spec)
             then (format s "~a" spec)
             else (format s "~a"
                    (name-string spec))))
        (when specs (write-char #\space s)))
      (write-char #\) s))))

(defmethod resize-window :after ((window resource-outline-dialog)
                                 position)
  (declare (ignore position))
  (fit-resource-outline-list-to-tabs window))

(defun fit-resource-outline-list-to-tabs (window)
  
  ;; If resizing the class browser outline dialog causes Windows
  ;; to use a different number of tab rows, then resize the
  ;; list widget inside the tab-control to fit that number of rows.
  (let* ((list-widget (find-component :list window))
         (tab-widget (find-component :tab window))
         (box (copy-box (box list-widget))))
    (setf (box-top box)
      (+ (box-top (box tab-widget))
         (rows-height tab-widget)
         5))
    
    ;; Re-adjust left edge due to "scale" round-off error.
    (setf (box-left box)
      (+ (box-left (box tab-widget))
         7))
    (setf (box list-widget) box)))

(defmethod mouse-moved ((dialog resource-outline-dialog)
                        buttons cursor-position)
  (declare (ignore buttons))
  (when *with-cursor* ;; spr18459b
    (return-from mouse-moved (call-next-method)))
  
  ;; When the user moves the mouse cursor in the area between the
  ;; widgets on each side of the Class Browser, change the mouse
  ;; cursor to indicate that an invisible divider between the
  ;; widgets may be dragged to resize the widgets.
  (if (< (box-right (box (find-component :outline dialog)))
         (position-x cursor-position)
         (box-left (box (find-component :tab dialog))))
      (setf (cursor (screen *system*))
        (vertical-splitbar-cursor)) ;; cg1742
    (call-next-method)))

(defmethod mouse-left-down ((dialog resource-outline-dialog)
                            buttons cursor-position)
  (declare (ignore buttons))
  (if (< (box-right (box (find-component :outline dialog)))
         (position-x cursor-position)
         (box-left (box (find-component :tab dialog))))
      (stretch-class-browser-widgets dialog)
    (call-next-method)))

(defmethod stretch-class-browser-widgets
    ((window resource-outline-dialog)) ;; cg1726
  (with-positions (pos1)
    (let* ((outline (find-component :outline window))
           (outline-box (box outline))
           (tab-control (find-component :tab window))
           (tab-control-box (box tab-control))
           (history (find-component :history window))
           (history-box (box history))
           (list (find-component :list window))
           (list-box (box list))
           (start-x (floor (+ (box-right outline-box)
                              (box-left tab-control-box))
                           2))
           (min 50)
           (max (- (interior-width window) 50))
           (top-left (interior-top-left window))
           dx)
      (grid-drag-box (screen *system*)
                     (position-y top-left)
                     (+ (position-y top-left)
                        (interior-height window))
                     (+ (position-x top-left) min)
                     (+ (position-x top-left) max)
                     2 nil t -2) ;; cg1742
      (setq dx (- (position-x (ncursor-position window pos1))
                  start-x))
      (setq dx (max (- min start-x)
                    (min (- max start-x) dx)))
      (incf (box-right outline-box) dx)
      (incf (box-right history-box) dx)
      (incf (box-left tab-control-box) dx)
      (incf (box-left list-box) dx)
      (setf (box outline) outline-box)
      (setf (box list) list-box)
      (setf (box tab-control) tab-control-box)
      (setf (box history) history-box)
      (fit-resource-outline-list-to-tabs window))))

(defmethod shortcut-commands ((pane resource-outline-pane)
                              (menu development-shortcut-menu))
  (class-shortcut-commands))

(defun class-shortcut-commands ()
  
  ;; Shortcut commands for both the class browser and class grapher.
  (list 
   (make-instance 'menu-item
     :name 'quick-class-info-command
     :title "Quick Class Info"
     :value 'quick-class-info-command)
   (make-instance 'menu-item
     :name 'quick-find-definition-command
     :title "Find Class Definition"
     :help-string
     "Finds the source code that defines the selected class."
     :value 'quick-find-definition-command)
   menu-separator
   (make-instance 'menu-item
     :name 'browse-class-command
     :title "Browse Selected Class"
     :value 'browse-class-command)
   (make-instance 'menu-item
     :name 'graph-subclasses-command
     :title "Graph Subclasses"
     :value 'graph-subclasses-command)
   (make-instance 'menu-item
     :name 'graph-superclasses-command
     :title "Graph Superclasses"
     :value 'graph-superclasses-command)
   menu-separator
   (make-instance 'menu-item
     :name 'inspect-command
     :title "Inspect Selected Class"
     :value 'inspect-command)
   (make-instance 'menu-item
     :name 'return-selected-object-command
     :title "Return Selected Class"
     :value 'return-selected-object-command)))

(defun resource-outline ()
  
  ;; Remember that this returns the right window only when called
  ;; in the IDE GUI process, since find-or-make-application-window
  ;; keys off of the current process.
  (find-or-make-application-window :resource-outline 'make-resource-outline))
 
;;; chee   09dec03 bug13787d allow removing a method
;;;        in the class browser
;;; chee   13feb04 changing the DELAYED property of the class browser's
;;;        history combo-box to true so that it will stop automatically
;;;        triggering a lookup when you have typed the last letter of
;;;        a class name that's already in the history list; this is
;;;        bad when that happens while you are in the middle of typing
;;;        a longer class name that begins with the other class name,
;;;        especially when the class name begins with "t", since t
;;;        if often in the history list as the default class; perhaps
;;;        delayed was nil because at one time we always triggered
;;;        the lookup when any class name was completed, but that's
;;;        no longer done --- it's just when it's in the history list
(defun make-resource-outline ()
  (let ((win (make-window :resource-outline
               :device 'resource-outline-dialog 
               :parent (development-main-window *system*) 
               :title "Class Browser" 
               :state :shrunk 
               :border nil 
               :left-attachment nil 
               :top-attachment nil 
               :right-attachment nil 
               :bottom-attachment nil 
               :title-bar t
               :minimize-button t
               :resizable t
               :scrollbars nil 
               :overlapped nil 
               :package-name :pc
               :dialog-help-file "class-browser-dialog" ;; cg2641
               :widgets
               (list 
                (make-instance 'static-text 
                  :name :name 
                  :title "Static Text 3" 
                  :value "~Name" 
                  :left 4 :top 5 :width 41 :height 22
                  )
                (make-instance 'combo-box 
                  :name :history 
                  :title "Combo Box 1" 
                  :value nil
                  :left 49 :top 5 :width 290 :height 194
                  :right-attachment :scale 
                  :help-string 
                  #.(format nil "Type the name of a class here ~
                        and press Enter to browse it, or select a ~
                        class from the drop-down history.")
                  :on-change 'resource-outline-history-on-change 
                  :on-change-test 'string-equal 
                  :delayed t ;; 13feb04
                  :typable t 
                  :range nil
                  :on-print 'princ-to-string )
                (make-instance 'resource-outline 
                  :name :outline 
                  :select-on-right-click t
                  :value nil 
                  :editable-in-place nil
                  :left 4 :top 33 :width 335 :height 241
                  :opened-pixmap-name :opened ;; cg1446
                  :closed-pixmap-name :closed
                  :leaf-pixmap-name :leaf
                  :right-attachment :scale 
                  :bottom-attachment :bottom 
                  :help-string 
                  #.(format nil "Pressing Enter with the focus here ~
                        generates an outline for the selected class.")
                  :on-change 'resource-outline-on-change 
                  :on-print 'resource-outline-key 
                  :range nil  
                  :scrollbars t)
                (make-instance 'resource-outline-list ;; bug13787d
                  :name :list
                  :title "Single Item List 3"
                  :left 360 :top 58 :width 208 :height 207
                  :left-attachment :scale
                  :right-attachment :right
                  :bottom-attachment :bottom
                  :state :shrunk
                  :cuttable t ;; bug13787d
                  :help-string
                  #.(format nil "Double-click an item here (or ~
                        select it and press Enter) to browse a class ~
                        or find method source code or inspect a slot." )
                  :on-change 'resource-outline-list-on-change 
                  :on-double-click 'resource-outline-list-on-double-click 
                  :on-print 'resource-outline-list-key 
                  :tab-control 
                  '(:tab :subs :supers :precedence :slots
                         :all-slots :methods :all-methods
                         :properties :events) ;; cg1768
                  :range nil)
                (make-instance 'default-button 
                  :name :default-button 
                  :title "~Display" 
                  :left 318 :top 5 :width 80 :height 25
                  :left-attachment :scale 
                  :right-attachment :scale 
                  :hidden-p t 
                  :state :shrunk 
                  :on-change 'resource-outline-default-button-fn )
                (make-instance 'tab-control 
                  :name :tab 
                  :title "Tab Control 1" 
                  :tabstop t ;; cg1779
                  :value nil
                  :left 352 :top 5 :width 224 :height 268
                  :left-attachment :scale 
                  :right-attachment :right 
                  :bottom-attachment :bottom 
                  :help-string 
                  #.(format nil "Select the class attribute to display.  ~
                        Methods may take a bit of time to find.")
                  :on-change 'class-tab-on-change 
                  :tab-width 35 
                  :tab-height 21 
                  :range 
                  (list 
                   (make-instance 'tab-info :id :supers
                     :label "Supers" :widgets '(:list))
                   (make-instance 'tab-info :id :subs
                     :label "Subs" :widgets '(:list))
                   (make-instance 'tab-info :id :precedence
                     :label "Precedence" :widgets '(:list))
                   (make-instance 'tab-info :id :slots
                     :label "Direct Slots" :widgets '(:list))
                   (make-instance 'tab-info :id :all-slots
                     :label "All Slots" :widgets '(:list))
                   (make-instance 'tab-info :id :methods
                     :label "Methods" :widgets '(:list))
                   (make-instance 'tab-info :id :all-methods
                     :label "All Methods" :widgets '(:list))
                   (make-instance 'tab-info :id :properties
                     :label "Properties" :widgets '(:list))
                   (make-instance 'tab-info :id :events
                     :label "Events" :widgets '(:list))))))))
    (fit-resource-outline-list-to-tabs win) ;; bug9057
    win))

