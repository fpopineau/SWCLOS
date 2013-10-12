;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; URI and NameSpace module
;;;
;;; IT Program Project in Japan:
;;;    Building Operation-Support System for Large-scale System using IT
;;;
;;; This module is separated from RDFShare module for more efficient modularity.
;;;
;;; Copyright (c) 2002, 2004 by Galaxy Express Corporation
;;;
;;; Copyright (c) 2007, 2008 Seiji Koide
;;;
;; History
;; -------
;; 2009.09.10    Some functions are rearanged for turtle.
;; 2008.12.11    Structure resource is introduced.
;; 2008.09.11    This file is created and the content is moved from RdfShare module.
;; 2008.09.10    The definition of duration is moved here from the file duration.
;; 2008.08.12    Revised based on http://www.w3.org/TR/2004/REC-rdf-syntax-grammar-20040210/
;; 2007.12.18    RdfShare is separated from Rdf module in order to share routines with RDFGate program
;;; ==================================================================================

(cl:provide :namespace)

(eval-when (:execute :load-toplevel :compile-toplevel)

  ) ; end of eval-when

(cl:defpackage :gx
  (:shadow uri uri-p parse-uri type typep value typep)
  (:import-from :net.uri render-uri uri-fragment copy-uri uri-scheme)
  (:use :common-lisp :net.uri)
  (:export uri uri-p uri-value set-uri-namedspace set-uri-namedspace-from-pkg get-uri-namedspace uri-namedspace
           uri2package uri2env uri2symbol irregular-name&pkg export-as-QName *base-uri* *default-namespace*
           symbol2uri name-ontology nodeID? nodeID2symbol *uri2symbol-name-mapping-fun* *uri2symbol-package-mapping-fun*)
  )

(in-package :gx)

;;;
;;;; URI in SWCLOS and turtle
;;; gx:uri has a value just like lisp symbol or such as QName.
;;; Read macro '&lt' reads a uri string and turns out a uri.
;;; Two trailing characters '&lt&lt' returns a value bound to the uri.
;;; See, double-angle-bracket-reader.
;;; ----------------------------------------------------------------------------------
;;; <http://www.w3.org/2000/01/rdf-schema#Resource>    ->
;;;                              #<uri http://www.w3.org/2000/01/rdf-schema#Resource>
;;; <<http://www.w3.org/2000/01/rdf-schema#Resource>>  -> #<|rdfs|:|Class| |rdfs|:|Resource|>
;;; |rdfs|:|Resource|                                      -> #<|rdfs|:|Class| |rdfs|:|Resource|>
;;; (eq <http://somewhere> <http://somewhere>)         -> true
;;; (eq <http://some%20where> <http://some%20where>)   -> true
;;; (eq <http://somewhere> <http://some%20where>)      -> false
;;; ----------------------------------------------------------------------------------
;;; <uri-boundp> and <uri-value> is available for uri just like <boundp> and <symbol-value>.

(defclass uri (net.uri:uri) ((value :accessor uri-value))
  (:documentation "tailord uri in SWCLOS that is a subclass of net.uri:uri and able to bind a value to, like symbol."))
(defmethod uri-value ((uri string))
  "returns bound value of <uri>."
  (uri-value (uri uri)))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (proclaim '(inline uri-p uri-boundp uri-value)))

(defun uri-p (x)
  "Is <x> an instance of gx:uri?"
  (cl:typep x 'uri))

(defun uri-boundp (x)
  "Is <x> a uri and bound at its value slot?"
  (etypecase x
    (string (uri-boundp (uri x)))
    (uri (slot-boundp x 'value))))

(defmacro %uri-value (uri)
  "This macro should be used by programmers, when <uri> is definitely <uri> here."
  (slot-value uri 'value))

;;;
;;;; URI APIs fixes
;;;
;;; Note that method <uri> always interns <thing>, but <parse-uri> does not intern it.
;;;

(defun parse-uri (thing &rest args)
  "when uri host exists but no uri path on <thing>, this method adds '/' to uri path.
   This is required for the namespace interning. See also,
   \<a href='http://www.franz.com/support/documentation/8.1/doc/operators/uri/parse-uri.htm'\>parse-uri\</a\>
   in ACL document."
  (let ((parsed (net.uri:parse-uri thing)))
    (cond ((and (net.uri:uri-host parsed) (null (net.uri:uri-path parsed)))
           (copy-uri parsed :path "/"))
          (t parsed))))

(defun null-uri-p (uri)
  "returns true if <uri> is null string, or <uri> is a uri and its rendered string is null."
  (etypecase uri
    (string (string= "" uri))
    (net.uri:uri (string= "" (net.uri:render-uri uri nil)))))

;;;
;;; Three methods are defined for generic function <uri>: when <thing> is a uri (instance of class net.uri:uri),
;;; its interned value is returned. When <thing> is a string, <intern-uri> is applied to create a interned uri
;;; from the string.
;;;
;;; A uri is always interned for a given uri string, because the uniqueness is required for uri to be bound to a
;;; value. This notion is same as lisp symbol.

(defmethod uri ((thing uri))
  "returns interned <thing> for class <gx:uri>."
  (intern-uri thing))

(defmethod uri ((thing net.uri:uri))
  "change class <net.uri:uri> of <thing> to <gx:uri> and returns interned <thing>."
  (when (and (net.uri:uri-host thing) (null (net.uri:uri-path thing)))
    (setf (net.uri:uri-path thing) "/"))
  (unless (cl:typep thing 'uri)
    (change-class thing 'uri))
  (intern-uri thing))

(defmethod uri ((thing cl:string))
  "when uri host exists but no uri path on the uri in <thing>, this method adds '/' to uri path.
   This is required for the namespace interning."
  (let ((parsed (net.uri:parse-uri thing)))
    (when (and (net.uri:uri-host parsed) (null (net.uri:uri-path parsed)))
      (setf (net.uri:uri-path parsed) "/"))
    (unless (cl:typep parsed 'uri)
      (change-class parsed 'uri))
    (intern-uri parsed)
    ))

(defmethod uri ((thing t))
  "signals an error."
  (error "Cannot coerce ~S to a gx:uri"))

;;;
;;; To list all uris for resource, call <list-all-entity-uris>. See gxutils module.
;;;

;;;
;;;; OntologySpace and NameSpaces
;;;
;;; Every uri for RDF is globally unique in the world. Therefore, a uri in system must be unique.
;;; The uniqueness of uri is assured in SWCLOS by interning a uri. Furthermore, every uri in RDF may be related to
;;; the corresponding QName, which consists of Prefix and LocalPart, if it exists.
;;; Such a named uri must have a name space that coincide with Prefix. The prefix name space can be shared among
;;; uris that has the same Prefix so that LocalPart has a unique entry in each named space on Prefix. This machinery
;;; is very similar to interning mechanism of lisp symbol, which consists of a symbol name and package, and a symbol
;;; name is unique in the package.
;;; ----------------------------------------------------------------------------------
;;; (get-uri-namedspace "http://www.w3.org/2000/01/rdf-schema")
;;;   -> #<uri-namedspace http://www.w3.org/2000/01/rdf-schema>
;;; (uri2package "http://www.w3.org/2000/01/rdf-schema")
;;;   -> #<The rdfs package>
;;; ----------------------------------------------------------------------------------
;;;
;;; In most of cases, the mapping from a uri to a QName is algorithmically decidable. However, there may be cases
;;; that system cannot decide how a uri should be mapped to a QName. In such a case, a user must provide the
;;; mapping function for additional rules or giving each mapping by replying the query from system one by one.
;;; Therefore, we need a maintenance device to keep irregular mapping for such cases.
;;; Followings provide such a mapping device and the named uri namespace functionality.
;;;
;;; The first thing to be done is, in spite that whether the mapping is regular or irregular, to divide a uri into
;;; a Prefix part and a LocalPart part. The Prefix uri part is associated to a uri named space (which is also
;;; associated a symbol package of QName), in which LocalParts of uris which shares the identical
;;; Prefix are associated to a symbol name of QName.
;;;
;;; Note that PrefixedAttName declaration in XML documents set the namespace with NCname (Prefix) and property value
;;; (associated prefix-uri). Note that DefaultAttName declaration in XML documents set the default namespace.
;;; See Rdf module.

(defvar *NameSpaces* (make-uri-space)
  "hasharray where a Prefix relating uri part is interned. This space ensures the uniqueness of a Prefix
   relating uri by interning it. See make-uri-space in Allegro Common Lisp documentation.")

;;;
;;; All namespaces in system are listed by <list-all-uri-namedspaces>. See gxutil module.
;;;
#|
(loop for nsc being each hash-key in *NameSpaces* do (print nsc))
|#

;;; In case of regular mapping from URI to QName, any local name to symbol name mapping is needless.
;;; However, in case of irregular mapping, the mapping from full URI to local name is needed in the namespace.
;;; This bookkeeping is done in the environment slot of <uri-namedspace>.

(defclass uri-namedspace (net.uri:uri)
  ((package :initform nil :accessor uri-namedspace-package)
   (env :initform nil :accessor uri-namedspace-env)) ; env is a uri to symbol-name assoc list for irregular symbol.
  (:documentation "A subclass of net.uri:uri, which has additional two slots, i.e., an associated symbol package slot and
uri to symbol name mapping environment slot.")
  )

(defun set-uri-namedspace (prefix-uri)
  "after interning <prefix-uri> to <*NameSpaces*>, change the class of <prefix-uri> from net.uri:uri to uri-namedspace.
   After that, symbol to uri mapping can be placed in this namespace."
  (declare (optimize (speed 3) (safety 1)))
  (setq prefix-uri (intern-uri (parse-uri prefix-uri) *NameSpaces*))
  (cl:change-class prefix-uri 'uri-namedspace))

(defun get-uri-namedspace (prefix-uri)
  "retrieves a uri-namedspace on <prefix-uri> from <*NameSpaces*> by interning it."
  (declare (optimize (speed 3) (safety 1)))
  (setq prefix-uri (intern-uri (parse-uri prefix-uri) *NameSpaces*))
  (when (cl:typep prefix-uri 'uri-namedspace) prefix-uri))

(defun set-uri-namedspace-from-pkg (pkg)
  "supposing <pkg> has a documentation that is the same string as rendered <prefix-uri>,
   sets the <prefix-uri> as uri-namedspace, and puts this <pkg> into uri-namedspace-package slot."
  (let* ((prefix-uri-str (documentation pkg t))
         (prefix-uri (parse-uri prefix-uri-str))
         (uri-namedspace (set-uri-namedspace prefix-uri)))
    (setf (uri-namedspace-package uri-namedspace) pkg)))

(defun uri2package (prefix-uri)
  "returns a package associated to <prefix-uri>."
  (declare (optimize (speed 3) (safety 1)))
  (setq prefix-uri (intern-uri prefix-uri *NameSpaces*))
  (when (and (cl:typep prefix-uri 'uri-namedspace) (slot-boundp prefix-uri 'package))
    (uri-namedspace-package prefix-uri)))

(defun uri2env (prefix-uri)
  "returns a LocalPart-symbol-name association list from <prefix-uri>.
   Note that the return value is null if there is no irregular mapping
   from uri to QName and no mapping given by replying a query."
  (declare (optimize (speed 3) (safety 1)))
  (setq prefix-uri (intern-uri (parse-uri prefix-uri) *NameSpaces*))
  (when (and (slot-exists-p prefix-uri 'env) (slot-boundp prefix-uri 'env))
    (uri-namedspace-env prefix-uri)))

(defvar *uri2symbol-name-mapping-fun* 'default-uri2symbol-name-mapping-fun
  "a function to be invoked when uri to symbol name mapping is irregular.")
(defvar *uri2symbol-package-mapping-fun* 'default-uri2symbol-package-mapping-fun
  "a function to be invoked when uri to symbol package mapping is irregular.")

(defun uri2symbol (uri)
  "transforms <uri> to a QName symbol. If irregular mapping has been established,
   the mapping is reused. When <uri> is a string, recursively calls with uri of <uri>.
   If <uri> is regular, namely a uri with fragment, <%uri2symbol> is used, else
   <irregular-name&pkg> is used. When <uri> is nil, nil is returned."
  (etypecase uri
    (null nil)
    (string (uri2symbol (uri uri)))
    (net.uri:uri (if (uri-fragment uri) (%uri2symbol uri)
                   ;; irregular process
                   (irregular-name&pkg uri)))))

(defun %uri2symbol (uri)
  "in case of <uri> with fragment, mapping is regular. Then, <uri> without fragment is Prefix part and fragment of <uri>
   is LocalPart part of <uri>. This function returns the QName of <uri> without consulting the LocalPart environment in .
   its namespace. If there is no package information on <uri> without fragment part, a function bound to
   <*uri2symbol-package-mapping-fun*> is invoked. Note that uri-namedspace-env is unused in this regular case.
   Note that QName symbol is automatically exported in this function."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((name (uri-de-escape (uri-fragment uri)))
         (butnameuri (copy-uri uri :fragment nil))
         (pkg (uri2package butnameuri))
         (namedspace nil)
         (symbol nil))
    (unless pkg
      (when (setq pkg (funcall *uri2symbol-package-mapping-fun* butnameuri))
        (unless (documentation pkg t)
          (setf (documentation pkg t) (render-uri butnameuri nil)))
        (setq namedspace (set-uri-namedspace butnameuri))
        (unless (uri-namedspace-package namedspace)
          (setf (uri-namedspace-package namedspace) pkg))))
    (when pkg
      (shadow name pkg)
      (setq symbol (intern name pkg))
      (export symbol pkg)
      symbol)))

(defun irregular-name&pkg (uri)
  "when the mapping is irregular, this function is called.
   Firstly, a function bound to <*uri2symbol-name-mapping-fun*> is invoked with <uri> argument.
   If it gives a symbol then the symbol is returned.
   If it gives a string, the string is used as symbol name in a package that is obtained from <uri> through <uri2package> or
   from a function bound to <*uri2symbol-package-mapping-fun*>.
   When the package is newly obtained from <*uri2symbol-package-mapping-fun*>, the package is associated this <uri> itself.
   Then, in the worst case, each irregular <uri> has its own namespace, as system cannot know general rules from one by one Q&A.
   You had better provide a smarter application-oriented function on <*uri2symbol-name-mapping-fun*>
   that provides always an appropriate QName."
  (let ((name (funcall *uri2symbol-name-mapping-fun* uri)))
    (cond ((symbolp name) name)
          ((stringp name)
           (let ((pkg (uri2package uri))
                 (symbol nil))
             (unless pkg
               (when (setq pkg (funcall *uri2symbol-package-mapping-fun* uri))
                 (setq uri (set-uri-namedspace uri))
                 (unless (uri-namedspace-package uri)
                   (setf (uri-namedspace-package uri) pkg))))
             (when pkg
               (unless (assoc uri (uri2env uri) :test #'net.uri:uri=)
                 (setf (uri-namedspace-env uri) (acons uri name (uri2env uri))))
               (shadow name pkg)
               (setq symbol (intern name pkg))
               (export symbol pkg)
               symbol))))))

;;;
;;;; Mapping URI to package and symbol, ID to symbol, anonymousID to symbol
;;;
;;;; Query for Users

(defun ask-user-for-string (prompt string1 option1 option2 prompt2)
  "This function is used in <ask-user-package-name> and <ask-user-symbol-name>."
  #+:common-graphics (cg:ask-user-for-string prompt string1 option1 option2 prompt2)
  #-:common-graphics
  (progn
    (format t "~%~A ~A:" prompt prompt2)
    (let ((str (read-line t)))
      (if (zerop (length str)) (values str nil "" nil)
        (values str nil "" t)))))

(let ((ask-user-pkg-name-canceled nil)
      (force-cancel nil))
  (defun ask-user-package-name (uri)
    "asks to user package name associated to <uri>."
    (unless force-cancel
      (let ((rendered (net.uri:render-uri uri nil)))
        (multiple-value-bind (pkg str2 button enter)
            (ask-user-for-string
             "QName prefix"
             (car (last (net.uri:uri-parsed-path uri))) "Enter" "Cancel"
             (format nil "as ~A" rendered))
          (declare (ignore str2 button))
          (cond (enter
                 (setq ask-user-pkg-name-canceled nil)
                 (unless (zerop (length pkg)) pkg))
                (t ;; canceled
                 (cond ((null ask-user-pkg-name-canceled)
                        (setq ask-user-pkg-name-canceled t)
                        nil)
                       ((y-or-n-p "Do you force cancelation forever?")
                        (setq force-cancel t)
                        nil)
                       (t (setq ask-user-pkg-name-canceled nil)
                          (setq force-cancel nil)))))))))

  (defun ask-user-symbol-name (uri)
    "asks to user a symbol name associated to <uri>."
    ;; This function is called only uri without fragment.
    (unless force-cancel
      (let ((rendered (net.uri:render-uri uri nil)))
        (multiple-value-bind (name str2 button enter)
            (ask-user-for-string
             "Symbol name"
             "" "Enter" "Cancel"
             (format nil "for ~A" rendered))
          (declare (ignore str2 button))
          (when (and enter (not (zerop (length name))))
            name)))))
  )

(defun default-uri2symbol-package-mapping-fun (uri)
  "This function is bound to <*uri2symbol-name-mapping-fun*> as default. This function just makes a query for users."
  (let ((pkg (ask-user-package-name uri)))
    ;; pkg is a string or nil
    (when pkg
      (setq pkg (or (find-package pkg) (make-package pkg :use nil))) ; by smh
      pkg)))

(defun default-uri2symbol-name-mapping-fun (uri)
  "This function is bound to <*uri2symbol-package-mapping-fun*> as default. If <uri> has a uri path,
   then the returned value of <%%uri2symbol> is returned. Othewise a query is made for users."
  (cond ((and (uri-path uri) (not (string= (uri-path uri) "/")))
         (%%uri2symbol uri))                                   ; symbol
        ((cdr (assoc uri (uri2env uri) :test #'net.uri:uri=))) ; string
        (t (error "Check it") (ask-user-symbol-name uri))))                       ; string or nil

(defun %%uri2symbol (uri)
  "Even if <uri> has no fragment, plausible separation is done by this function. In short,
   the file name or the most subfolder of <uri> path is taken as symbol name, and the remaining part
   of <uri> path is taken for namespace (package) association. If you can find some application specific rules
   for making QName, you had better program it as well as this function does."
  (let* ((path (parse-namestring (uri-path uri)))
         (type (pathname-type path))
         (name (pathname-name path))
         (directory (pathname-directory path))
         (butnameuri nil)
         (pkg nil)
         (symbol nil))
    (when (string= type "nt")
      (setq name (concatenate 'string name "_" type)))
    ;(assert (and (eq (car directory) :absolute) (or name (cdr directory))))
    ;; ex: "http://somewhere/somedirectory/subdir/JohnSmith" -> #<uri http://somewhere/somedirectory/subdir/>
    ;; ex: "http://somewhere/JohnSmith"                      -> #<uri http://somewhere/>
    (setq directory (cdr directory)) ; delete :absolute
    (unless name
      (setq name (car (last directory)))
      (setq directory (butlast directory)))
    (setq butnameuri
          (copy-uri uri
                    :path (apply #'concatenate 'cl:string "/"
                                 (mapcan #'(lambda (d) (list d "/")) directory))))
    (setq butnameuri (set-uri-namedspace butnameuri))
    (setq pkg (uri2package butnameuri))
    (unless pkg
      (when (setq pkg (funcall *uri2symbol-package-mapping-fun* butnameuri))
        (unless (documentation pkg t)
          (setf (documentation pkg t) (render-uri butnameuri nil)))
        (unless (uri-namedspace-package butnameuri)
          (setf (uri-namedspace-package butnameuri) pkg))))
    (when pkg
      (shadow name pkg)
      (setq symbol (intern name pkg))
      (export symbol pkg)
      (let ((env (uri-namedspace-env butnameuri)))
        (unless (assoc symbol env)
          (setf (uri-namedspace-env butnameuri)
            (acons symbol uri env))))
      symbol)))

(defvar *default-namespace* nil
  "Default name space URI for XML parsing in current time. This value is set by <read-rdf-from-http> and <read-rdf-file>.")
(defvar *base-uri* nil
  "Base URI that is indicated in XML file. This value is set by <read-rdf-from-http>, <read-rdf-file>, and <read-rdf-from-string>.")

(defun uri-reserved-char-p (char)
  "Is this <char> reserved for uri?"
  (or (char= char #\:)
      (char= char #\/)
      (char= char #\?)
      (char= char #\#)
      (char= char #\[)
      (char= char #\])
      (char= char #\@)
      (char= char #\!)
      (char= char #\$)
      (char= char #\&)
      (char= char #\')
      (char= char #\()
      (char= char #\))
      (char= char #\*)
      (char= char #\+)
      (char= char #\,)
      (char= char #\;)
      (char= char #\=)
      ))

(defun uri-escape (str)
  "This function performs Percent-Encoding. Namely, RESERVED CHARACTERS for URI that is contained
   in <str> are escaped with percent(%) character to a triplet of &lt;% HEXDIG HEXDIG&gt;."
  (cond ((and (> (length str) 5) (string= "http:" (subseq str 0 5))) str) ; this is for ontology URIs
        (t (%uri-escape (%uri-escape-for-percent str)))))
(defun %uri-escape-for-percent (str)
  "changes character #\% in <str> to string '%25'."
  (let ((pos 0))
    (cond ((setq pos (position #\% str)) ;; found
           (concatenate 'cl:string (subseq str 0 pos) "%25" (%uri-escape-for-percent (subseq str (1+ pos)))))
          (t str))))
(defun %uri-escape (str)
  "encodes the uri reserved characters to hexadecimals."
  (let ((pos 0))
    (cond ((setq pos (position-if #'uri-reserved-char-p str))
           (let ((c (char str pos)))
             (concatenate 'cl:string
               (subseq str 0 pos)
               (format nil "%~X"  (char-code c))
               (%uri-escape (subseq str (1+ pos))))))
          (t str))))

(defun uri-de-escape (str)
  "This function decodes Percent-Encoding to characters."
  (let ((pos 0))
    (cond ((setq pos (position #\% str :test #'char=))
           (let ((c (code-char (parse-integer (subseq str (1+ pos) (min (length str) (+ pos 3)))
                                              :radix 16))))
             (concatenate 'cl:string
               (subseq str 0 pos)
               (string c)
               (uri-de-escape (subseq str (+ pos 3))))))
          (t str))))

(defun double-angle-bracket-reader (stream char)
  (let ((nc (peek-char nil stream t nil t)))
    (cond ((char= nc #\<)          ; double #\<
           (read-char stream)      ; discard it
           (let* ((uri-str
                   (coerce
                    (loop with char until (char= #\> (setq char (read-char stream))) collect char)
                    'cl:string))
                  (uri (uri uri-str))
                  (symbol nil))
             (when (char= #\> (peek-char nil stream t nil t))
               (read-char stream))  ; discard one more #\>
             (cond ((uri-boundp uri)
                    (list 'quote (uri-value uri)))
                   ((setq symbol (uri2symbol uri))
                    (let ((obj nil))
                      (cond ((and (boundp symbol) (object? symbol))
                             (setf (uri-value uri) (setq obj (symbol-value symbol))))
                            (t (setf (uri-value uri)
                                 (setq obj (addInstance (list (symbol-value '|rdfs:Resource|)) symbol)))))
                      (setf (slot-value obj '|rdf|:|about|) uri)
                      (list 'quote obj)))
                   (t ;; blank node
                    (setf (uri-value uri) (addInstance (list (symbol-value '|rdfs:Resource|)) nil))
                    (list 'quote (uri-value uri))))))
          ((NCNameStartChar-p nc)
           (let* ((uri-str
                   (coerce
                    (loop with char until (char= #\> (setq char (read-char stream))) collect char)
                    'cl:string))
                  (uri (uri uri-str)))
             uri))
          (t (cl-user::read-token stream char)
             ))))

(defun QName2PrefixedName (QName)
  "transforms <QName> to PrefixedName string. <QName> should be a lisp symbol."
  (concatenate 'cl:string (package-name (symbol-package QName)) ":" (symbol-name QName)))

(defun QName2UnPrefixedName (QName)
  "transforms <QName> to UnPrefixedName string. <QName> should be a lisp symbol."
  (symbol-name QName))

(defun export-as-QName (symbol)
  "export this <symbol> as QName. The symbol-package of <symbol> is stored
   into the related uri namespace."
  (proclaim `(special ,symbol))
  (let* ((pkg (symbol-package symbol))
         (ns (documentation pkg t)))
    (when pkg
      ;; if symbol is a name of Ontology, no pkg
      (export symbol pkg)
      (when (and ns (setq ns (get-uri-namedspace ns)))
        (unless (uri-namedspace-package ns)
          (setf (uri-namedspace-package ns) pkg))))))

(defun QNameString2symbol (QName)
  "transforms <QName> string to a lisp symbol."
  (let (Prefix LocalPart pkg)
    (cond ((find #\: QName)
           (let (pos)
             (setq Prefix (subseq QName 0 (setq pos (position #\: QName))))
             (setq LocalPart (subseq QName (1+ pos)))))
          (t (setq Prefix nil)
             (setq LocalPart QName)))
    (cond (Prefix
           (setq pkg (find-package Prefix)) ; nicknames availabel
           (when (null pkg)
             (warn "There is no package for ~A." Prefix)
             (setq pkg (make-package Prefix :use nil))  ; by smh
             (warn "~W created." pkg))
           (shadow LocalPart pkg)
           (setq QName (intern LocalPart pkg))
           (export QName pkg))
          (*default-namespace*
           (setq pkg (uri-namedspace-package *default-namespace*))
           (shadow LocalPart pkg)
           (setq QName (intern LocalPart pkg))
           (export QName pkg))
          (*base-uri*
           (setq pkg (uri-namedspace-package *base-uri*))
           (cond (pkg (shadow LocalPart pkg)
                      (setq QName (intern LocalPart pkg))
                      (export QName pkg))
                 (t (setq QName LocalPart))))
          (t (setq QName LocalPart))) ; returns a string
    QName))

(defun symbol2QNameString (symbol)
  "transforms <symbol> to QName string in the current namespace."
  ;(format t "~%Default namespace package =~S" (uri-namedspace-package *default-namespace*))
  (cond ((eql (symbol-package symbol) (uri-namedspace-package *default-namespace*))
         (symbol-name symbol))
        (t (concatenate 'cl:string (package-name (symbol-package symbol)) ":" (symbol-name symbol)))))

;;;
;;;; Symbol to URI
;;;

(defun symbol2uri (symbol)
  "transforms <symbol> to its associated uri. The symbol package affects.
   The namespace uri should has been registered and documented in package."
  ;(when (multiple-value-bind (sym in/ex) (intern (string symbol) (symbol-package symbol))
  ;        (declare (ignore sym))
  ;        (not (eq in/ex :external)))
  ;  (error "Internal symbol ~S is designated in symbol2uri." symbol))
  (or (and (boundp symbol)
           (not (null (symbol-value symbol)))
           (slot-boundp (symbol-value symbol) '|rdf|:|about|)
           (slot-value (symbol-value symbol) '|rdf|:|about|)
           (uri (slot-value (symbol-value symbol) '|rdf|:|about|)))
      (let* ((name (uri-escape (symbol-name symbol)))
             (pkg (symbol-package symbol))
             (uri (documentation pkg t))
             (ns (when uri
                   (or (get-uri-namedspace uri)
                       (set-uri-namedspace uri))))
             (env (when ns (uri-namedspace-env ns))))
        (or (cdr (assoc symbol env))
            (and ns
                 (cond ((and (pathname-name (merge-pathnames (pathname (net.uri:uri-path ns))))
                             (null (net.uri:uri-fragment ns)))
                        (uri
                         (copy-uri (net.uri:parse-uri (net.uri:render-uri ns nil)) :fragment name)))
                       (t ;; no fragment but name
                        (uri
                         (copy-uri (net.uri:parse-uri (concatenate 'cl:string (net.uri:render-uri ns nil) name)))))))
            ))))

;;;; NodeID
;;;
;;; A nodeID is an exorted symbol in package "_".  See the following example.
;;; ----------------------------------------------------------------------------------
;;; (nodeID2symbol "abc")      -> _:abc
;;; (make-unique-nodeID "abc") -> _:abc0
;;; ----------------------------------------------------------------------------------
(defun nodeID? (name)
  "Is this <name> a nodeID?"
  (declare (optimize (speed 3) (safety 1)))
  (and (symbol-package name)
       (string= "_" (package-name (symbol-package name)))))

(defun nodeID2symbol (str)
  (declare (optimize (speed 3) (safety 1)))
  "simply transforms <str> to a exported symbol in anonymous node package :_
   and returns it."
  (let ((nodeID (intern str :_)))
    (export nodeID (find-package :_))
    nodeID))

(defun make-unique-nodeID (str)
  (declare (optimize (speed 3) (safety 1)))
  "makes a unique node ID from <str>. Namely, adds numbers at the end of <str> and makes unique symbol."
  (let ((symbol (gentemp str :_)))
    (export symbol (find-package :_))
    symbol))

(defun name-ontology (ontouri)
  "transforms <ontouri> to special symbol of which string is equal to <ontouri>."
  (when (null-uri-p ontouri)
    (cond (*default-namespace* (setq ontouri (uri *default-namespace*)))
          (*base-uri* (setq ontouri (uri *base-uri*)))
          (t (error "Cant happen!"))))
  (let ((pkg (uri2package ontouri)))
    (when (null pkg)
      (setq pkg (funcall *uri2symbol-package-mapping-fun* ontouri)))
    (make-symbol (net.uri:render-uri ontouri nil))
    ))

;
;; Advice Package-name-to package function for Allegro Reader
;
#+never
(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf (symbol-function '%.reader-error)
    (symbol-function 'excl::.reader-error))
  )
#+never
(define-condition package-not-found-in-reader-error (reader-error) ())
#+never
(define-condition symbol-not-found-in-reader-error (reader-error) ())
#+never
(define-condition symbol-not-external-in-reader-error (reader-error) ())

#+never
(eval-when (:execute :load-toplevel :compile-toplevel)
  (excl:without-package-locks
      (defun excl::.reader-error (stream format &rest args)
        (cond ((string= (car args) "Package ~S not found.")
               (cerror "Create it?"
                       'package-not-found-in-reader-error
                       :stream stream
                       :format-control format :format-arguments args)
               (make-package (car (second args)))
               )
              ((string= (car args) "Symbol ~S not found in the ~A package.")
               (error 'symbol-not-found-in-reader-error
                       :stream stream
                       :format-control format :format-arguments args)
               )
              ((string= (car args) "~
The symbol ~s is not external in the ~a package.")
               (cerror "Export it?"
                       'symbol-not-external-in-reader-error
                       :stream stream
                       :format-control format :format-arguments args)
               (let ((pkg (find-package (second (second args)))))
                 (export (find-symbol (car (second args)) pkg) pkg)
                 (find-symbol (car (second args)) pkg))
               )
              (t (error 'reader-error :stream stream
                   :format-control format :format-arguments args)))

        )
    )
  )

;; End of module
;; --------------------------------------------------------------------
;;;
;;; Seiji Koide Aug-04-2009
;;;
