;; -=Begin Copyright Notice=-
;; copyright (c) 1986-2004 Franz Inc, Oakland, CA  Partly rights reserved.
;; copyright (c) 2005 Galaxy Express Corporation, Japan.
;;
;; This program is modified from Franz Inc. classgrapher.
;;
;; -=End Copyright Notice=-
;;; $Id: classgrapher.cl,v 1.2 2003/12/11 07:56:47 cheetham Exp $

(in-package :gx)

(defmethod gx::class-direct-subclasses-for-graph ((object |rdfs|:|Resource|))
  (let ((classes (mop:class-direct-subclasses object)))
    (remove-if #'gx::owl-restriction-p (remove-if-not #'gx::resource-p classes))))

(defmethod gx::class-direct-superclasses-for-graph ((object |rdfs|:|Resource|))
  (let ((classes (mop:class-direct-superclasses object)))
    (remove-if #'gx::owl-restriction-p (remove-if-not #'gx::resource-p classes))))

;;;============================================================================
(in-package :ide.class-grapher)
;(export #:graph-subclasses)

;(add-function-to-ide-hook 'graph-subclasses 'graph-subclasses-hook)

(defun graph-subclasses (class)
  (cond ((gx:resource? class)
         (graph-resource class :direction :kid)
         t)
        (t (graph-class class :direction :kid)
           t)))

;(add-function-to-ide-hook 'graph-superclasses 'graph-superclasses-hook)

(defun graph-superclasses (class)
  (cond ((gx:resource? class)
         (graph-resource class :direction :parent)
         t)
        (t (graph-class class :direction :parent)
           t)))

(defclass class-graph-frame (graph-frame)
  ()
  (:default-initargs

      ;; Turn the keep-configuration flag off for class graph windows,
      ;; because when it's on if you show a very small graph, then
      ;; exit and restart (loading prefs.cl), then further graphs
      ;; that should be large will be just as small.
      :keep-configuration nil)) ;; bug10472

(defclass resource-graph-pane (class-graph-pane)())

(defmethod shortcut-commands ((pane ide.class-grapher::resource-graph-pane)
                              (menu development-shortcut-menu)) ;; cg958
  (append
   (list (make-instance 'menu-item
           :name 'quick-resource-form-command
           :title "Quick Resource Form"
           :value 'ide.base::quick-resource-form-command)
         (make-instance 'menu-item
           :name 'quick-xml-form-command
           :title "Quick XML Form"
           :value 'ide.base::quick-xml-form-command)
         menu-separator)
   (class-shortcut-commands)
   (list menu-separator
         (make-instance 'menu-item
           :name 'include-children-command
           :title "Include Children"
           :help-string
           "For a leaf node, adds any subclasses to the existing class graph."
           :value 'include-children-command)
         (make-instance 'menu-item
           :name 'include-parents-command
           :title "Include Parents"
           :help-string
           "For a root node, adds any superclasses to the existing class graph."
           :value 'include-parents-command))))

(defmethod all-windows-of-type-title ((window class-graph-frame))
  "All Class Graphs")

(defmethod mouse-double-click ((pane ide.class-grapher::resource-graph-pane)
                               shift cursor-position)
  (declare (ignore shift))
  (let* ((gnode (find-gnode-under-event pane cursor-position)))
    (if* gnode
       then (ide.class-browser::display-resource-outline (gnode-data gnode))
       else (call-next-method))))

;;; chee   reorg; extract this class-graph-specific code
;;;        from the method on the general cg-grapher-window-display
;;;        class
(defmethod virtual-key-down ((display ide.class-grapher::resource-graph-pane)
                             buttons key-code)
  (declare (ignore buttons))
  (declare (fixnum key-code))
  (case key-code
    (#.vk-space
     (ide.class-browser::display-resource-outline (selected-class display)))
    (t (call-next-method))))

(defmethod default-width ((window class-graph-frame))
  (floor (* .7 (interior-width (screen *system*)))))

(defmethod default-height ((window class-graph-frame))
  (floor (* .6 (interior-height (screen *system*)))))

(defun class-nice-name (class)
  (lowercase-object
   (if (eq class (find-class (class-name class) nil))
       (class-name class)
     class)))

(defun graph-resource (class &key (direction :kid))
  (let ((class-in class))
    (with-hourglass
      (when (symbolp class)
        (setq class
              (or (find-class class nil)
                  (ide.base:pop-up-bound-symbol-in-all-packages
                   class nil t)))
        (unless class
          (lisp-warning
           "~s is not a class, so you can't graph it as one."
           class-in)
          (return-from graph-resource))))
    (lisp-message "Graphing ~a of ~s ..."
      (if (eq direction :kid) "subclasses" "superclasses")
      (class-name class))
    (let* ((frame (make-window :class-graph-frame
                    :device 'class-graph-frame
                    :parent (development-main-window *system*)
                    :state :shrunk
                    :scrollbars nil
                    :title (format nil "~aclass Graph for ~a"
                             (if (eq direction :kid)
                                 "Sub" "Super")
                             (class-nice-name class))))
           (pane (make-window :cg-grapher-window-display
                   :device 'ide.class-grapher::resource-graph-pane ;; cg958
                   :parent frame ; put the pane inside the frame
                   :right-attachment :right
                   :bottom-attachment :bottom
                   :exterior
                   (cg.win::make-box	; make it fill the frame initially
                    0 0
                    (cg.base::interior-width frame)
                    (cg.base::interior-height frame)))))
      (apply #'ide.grapher::graph-descendants
             class
             :generations (ide.base::class-graph-initial-depth
                           (cg.base::configuration *ide-system*))
             :display pane
             :gnode-class 'resource-gnode
             :title "foo"
             :children-fn 'gx::class-direct-subclasses-for-graph
             :parents-fn 'gx::class-direct-superclasses-for-graph
             :complete-name '("composer" "classGraph")
             :complete-class '("Composer" "Grapher")
             :direction direction
             nil)

      ;; Perhaps we should resize the window.
      (resize-graph-to-fit frame)

      (let ((gnode (locate-gnode-by-data pane class)))
        (when gnode (set-current-gnode pane gnode)))

      (select-window frame)
      (lisp-message
          "GRAPHED ~a of ~s       Navigate with the arrow keys ~
           and home/end/pageup/pagedown, and with the CONTROL ~
           key for alternate behavior       Left-click the ~
           background and drag to scroll."
        (if (eq direction :kid) "subclasses" "superclasses")
        (class-name class)))))

(defmethod display-current-gnode  ((window class-graph-frame))
  (display-current-gnode (car (windows-scratch-list window))))

(defclass resource-gnode (class-gnode)
  ())

(defmethod gnode-to-string  ((gnode resource-gnode))
  (let* ((class (gnode-object gnode))
         (name (class-name class)))
    (if (eq class (find-class name))
        (cond ((slot-boundp class '|rdfs|:|label|)
               (let ((label (|rdfs|:|label| class)))
                 (flet ((get-str (lb)
                                 (cond ((cl:typep lb '|rdf|:|inLang|)
                                        (gx::content lb))
                                       (t lb))))
                   (cond ((consp label) (get-str (car label)))
                         (t (get-str label))))))
              (t (lowercase-object name)))
      (lowercase-object class))))

(defmethod (setf class-graph-font)
    :after (value (configuration ide-configuration))
  (declare (ignore value))
  (do-windows (child (screen *system*)) ;; cg359
    (when (typep child 'graph-frame)
      (recompute-class-graph child))))

(defun recompute-class-graph (frame)
  (let* ((pane (car (windows-scratch-list frame)))
         (current-gnode (display-current-gnode pane))
         (class (and current-gnode
                     (gnode-data current-gnode))))
    (setf (display-current-gnode pane) nil)
    (setf (font pane) (class-graph-font (configuration *ide-system*)))
    (scroll-to pane (make-position 0 0))
    (with-hourglass
      (recompute-graph
       pane :generations
       (class-graph-initial-depth (configuration *ide-system*))))
    (let ((gnode (locate-gnode-by-data pane class)))
      (when gnode (set-current-gnode pane gnode)))))

;;;===========================================================================
;;;
;;;

(in-package :ide.grapher)

(defun unset-current-gnode (pane)
  (let ((old-gnode (display-current-gnode pane)))
    (setf (display-current-gnode pane) nil)
    (when (and old-gnode (gnode-region pane old-gnode))
      (render-gnode-contents
       pane old-gnode ;; cg2584
       :background-color (effective-background-color pane)))
    (lisp-message "")))

(defmethod map-over-gnodes ((layout fundamental-graph-layout) fn)
  (maphash #'(lambda (key value)
               (declare (ignore value))
               (when (gnode-present-p layout key)
                 (funcall fn key)))
           (registry-element-hasht layout)))

(defmethod locate-gnode-by-data ((display grapher-window-display) data)
  (let ((layout (display-layout display)))
    (flet ((locate-node (gnode)
                        (when (eq data (gnode-data gnode))
                          (return-from locate-gnode-by-data gnode))))
      (map-over-gnodes layout #'locate-node))))

(defmethod gnode-data-status-message ((display cg-grapher-window-display)
                                      (data |rdfs|:|Resource|))
  (ide.base::quick-resource-message data :symbol (class-name data)))

(defmethod mouse-left-down :before  ((pane ide.class-grapher::resource-graph-pane)
                                     shift cursor-position)
  (declare (ignore shift))
  (let ((gnode (find-gnode-under-event pane cursor-position)))
    (cond (gnode (set-current-gnode pane gnode))
          (t (unset-current-gnode pane)))))

(defmethod mouse-right-down ((pane ide.class-grapher::resource-graph-pane)
                             buttons cursor-position)
  (case (logand buttons #.(logior shift-key control-key alt-key))
    (0 ;; no key
     (let ((gnode (find-gnode-under-event pane cursor-position)))
       (if gnode
           (progn
             (set-current-gnode pane gnode)

             ;; Do the general right-button menu.
             (call-next-method))
         (cg.shortcut-menu::pop-up-background-shortcut-menu pane)))) ;; cg1025
    (4 ;; shift-key
     (call-next-method))
    (8 ;; control-key
     (call-next-method))
    (512 ;; alt-key
     (call-next-method))
    (t
     (call-next-method))))

;;;==================================================================================
;;;
;;;

(in-package :ide.base)

(defmethod quick-resource-form-command ((window ide.class-grapher::resource-graph-pane))
  (or (pass-to-child window 'quick-resource-form-command)
      (let ((object (selected-object window)))
        (cond ((and object (gx:resource-p object))
               (quick-resource-form-message object
                                            :pane window :show-long t))
              ;(object
              ; (quick-class-message object
              ;                      :symbol symbol :pane window)
              ; (quick-class-message object
              ;                      :symbol symbol :pane window :show-long t))
              (t
               (multiple-value-bind (object nil-is-selected)
                   (selected-object window)
                 (if* (and (or object nil-is-selected) ;; bug13349
                           (or (symbolp object)(stringp object)))
                    then (lisp-message
                             "~s does not name a class in ~a package."
                           object
                           (if* (some #'(lambda (symbol)
                                          (find-class symbol nil))
                                      (find-all-symbols object))
                              then (format nil "the ~(~a~)"
                                     (package-name
                                      (or (stream-package window)
                                          *package*)))
                              else "any"))
                    else (conversion-error window :resource
                                           'quick-resource-form-command
                                           :beep nil))))))))

(defmethod quick-xml-form-command ((window ide.class-grapher::resource-graph-pane))
  (or (pass-to-child window 'quick-xml-form-command)
      (let ((object (selected-object window)))
        (cond ((and object (gx:resource-p object))
               (quick-xml-form-message object
                                       :pane window :show-long t))
              ;(object
              ; (quick-class-message object
              ;                      :symbol symbol :pane window)
              ; (quick-class-message object
              ;                      :symbol symbol
              ;                      :pane window :show-long t))
              (t
               (multiple-value-bind (object nil-is-selected)
                   (selected-object window)
                 (if* (and (or object nil-is-selected) ;; bug13349
                           (or (symbolp object)(stringp object)))
                    then (lisp-message
                             "~s does not name a class in ~a package."
                           object
                           (if* (some #'(lambda (symbol)
                                          (find-class symbol nil))
                                      (find-all-symbols object))
                              then (format nil "the ~(~a~)"
                                     (package-name
                                      (or (stream-package window)
                                          *package*)))
                              else "any"))
                    else (conversion-error window :resource
                                           'quick-xml-form-command
                                           :beep nil))))))))

(defmethod quick-resource-form-message ((class |rdfs|:|Resource|) &key symbol pane show-long
                                        use-local-status-bar)
  (declare (ignore symbol use-local-status-bar))
  (let ((string (quick-resource-form-string class :include-header t))
        (listener-pane (selected-listener-pane)))
    (cond ((and show-long
                (not (eq pane listener-pane)))
           (file-position listener-pane :end)
           (format listener-pane "~&~a" string)
           (new-prompt listener-pane)
           (move-window-behind ;; cg2537
            (top-level-window listener-pane)
            (top-level-window pane)))
          (t
           (lisp-message string)))))

(defmethod quick-xml-form-message ((class |rdfs|:|Resource|) &key symbol pane show-long
                                   use-local-status-bar)
  (declare (ignore symbol use-local-status-bar))
  (let ((string (quick-xml-form-string class :include-header t))
        (listener-pane (selected-listener-pane)))
    (cond ((and show-long
                (not (eq pane listener-pane)))
           (file-position listener-pane :end)
           (format listener-pane "~&~a" string)
           (new-prompt listener-pane)
           (move-window-behind ;; cg2537
            (top-level-window listener-pane)
            (top-level-window pane)))
          (t
           (lisp-message string)))))

(defun quick-resource-form-string (class &key include-header)
  (declare (ignore include-header))
  (format nil "~S" (gx:get-form class)))

(defun quick-xml-form-string (class &key include-header)
  (declare (ignore include-header))
  (with-output-to-string (s nil)
    (gx::write-xml class s)))

(defmethod quick-resource-message ((class |rdfs|:|Resource|) &key symbol pane show-long
                                  use-local-status-bar)
  (declare (ignore use-local-status-bar))
  (let* ((string (quick-resource-string
                  class :symbol symbol :show-long show-long
                  :include-header t))
         (listener-pane (selected-listener-pane)))
    (cond ((and show-long ;; cg1591
                (not (eq pane listener-pane)))
           (file-position listener-pane :end)
           (format listener-pane "~&~a" string)
           (new-prompt listener-pane)
           (move-window-behind ;; cg2537
            (top-level-window listener-pane)
            (top-level-window pane)))
          (t
           (lisp-message string)))))

(defmethod quick-resource-string ((class |rdfs|:|Resource|) &rest args)
  (cond ((gx::class-direct-instances class)
         (format nil "instances ~S" (mapcar #'name (gx::class-direct-instances class))))
        (t "no direct instances")))

;;;===========================================================================
(in-package :cg.shortcut-menu)

(defmethod pop-up-background-shortcut-menu ((window basic-pane))
  ;(format t "~%Background menu ~S" window)
  (with-positions (pos1 pos2)
    (let* ((*no-pop-up-in-object-symbol* t)
           (class (shortcut-menu-class window))
           (menu-position (or (shortcut-menu-position window)
                              (position- (ncursor-position
                                          (screen *system*) pos2)
                                         #.(make-position 80 12))))
           menu commands)
      (when (< (position-x menu-position) 0) ;; rfe5415
        (setf (position-x menu-position) 0))
      (unwind-protect
          (progn
            (setq menu (open-menu nil class
                                  (screen *system*)
                                  :name :shortcut-menu
                                  :window window
                                  :menu-click-position
                                  (ncursor-position window pos1)))
            (when (setf commands (shortcut-commands window menu))
              (setf (menu-items menu) (menu-item-list commands))
              (pop-up-menu menu (screen *system*)
                           menu-position :left :top)

              ;; Return true if we really showed a menu.
              t)) ;; rfe3704b
        (when menu
          (close menu))))))
