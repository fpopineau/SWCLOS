;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; RDF/XML parser module
;;;
;;; IT Program Project in Japan:
;;;    Building Operation-Support System for Large-scale System using IT
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan,
;;;
;;; Copyright © 2002,2004 by Galaxy Express Corporation, Japan.
;;;
;; History
;; -------
;; 2008.08.12    Revised based on http://www.w3.org/TR/2004/REC-rdf-syntax-grammar-20040210/
;; 2004.05.09    File created and contents are copied from Rdf.cl

(cl:provide :rdfparser)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :rdfscore)
  )

(cl:defpackage :gx
  (:export read-rdf-file addRdfXml)
  )

(in-package :gx)

;;
;; Description structure to S-expression form
;;

(defun make-form (x)
  "<x> may be a null list, string, number, literal, an instance of description, comment, or cons.
This function returns a S-expression of <x>. If <x> is a comment, nil is returned."
  (etypecase x
    (null nil)
    (string (list x))
    (number (list x))
    (|xsd|:|anySimpleType| (list x))
    (|rdfs|:|Literal| (list x))
    (|rdf|:|Description| (list (Description-form x)))
    (comment nil) ; depress comment
    (cons (mapcan #'make-form x))))

(defun prop-form (prop)
  (let ((name (prop-name prop))
        (atts (prop-att&vals prop))
        (value (prop-value prop)))
    ;(format t "~%name:~S atts:~S value:~S *base*:~S" name atts value *base-uri*)
    (let ((resource (getf atts '|rdf|:|resource|))
          (nodeID (getf atts '|rdf|:|nodeID|))
          (datatype (getf atts '|rdf|:|datatype|))
          (lang (getf atts '|xml|:|lang|)))
      ;(format t "~%resource:~S nodeID:~S datatype:~S lang:~S" resource nodeID datatype lang)
      (cond (nodeID
             (assert (null resource) () "resource cannot be placed with nodeID.")
             (assert (null value))
             (list name (nodeID2symbol nodeID)))
            (resource
             (cond ((char= (elt resource 0) #\#)
                    (list name (copy-uri
                                   (uri
                                    (render-uri (or *base-uri* *default-namespace*) nil))
                                   :fragment (subseq resource 1))))
                   (t (let ((rsc (uri resource)))
                        (cond ((uri-scheme rsc) (list name rsc))
                              (t (let ((base (render-uri
                                              (or *base-uri* *default-namespace*)
                                              nil)))
                                   (cond ((char= #\/ (char base (1- (length base))))
                                          (list name
                                                   (uri (concatenate 'cl:string base resource))))
                                         (t (list name rsc))))))))))
            (datatype
             (setq datatype (uri2symbol (uri datatype)))
             (assert (null (cdr value)))
             (list name (read-as-datatype (car value) datatype)))
            (lang
             (cons name (mapcar #'(lambda (val) (cons '@ (list val (intern lang "keyword"))))
                          (make-form value))))
            (t (cons name (make-form value)))))))

(defun Description-form (description)
  "generates S-exression of <description>."
  (%Description-form (Description-TAG description)
                     (Description-ATT&VALS description)
                     (Description-ELEMENTS description)))

(defun %Description-form (class attrs props)
  "generates S-expression from <class>, <attrs>, and <props>.
   <class> is a QName symbol that indicates type tag in RDF/XML.
   <attrs> are a attribute/value list for attributes in RDF/XML.
   <props> are a property/value list for properties in RDF/XML."
  (let ((about (getf attrs '|rdf|:|about|))
        (id (getf attrs '|rdf|:|ID|))
        (nodeID (getf attrs '|rdf|:|nodeID|))
        (lang (getf attrs '|xml|:|lang|))
        (slots (loop for prop in props when (prop-p prop) collect (prop-form prop))))
    ;(format t "~%about:~S id:~S slots:~S" about id slots)
    ;(format t "~%*default-namespace*:~S" *default-namespace*)
    ;(format t "~%*base-uri*:~S" *base-uri*)
    (when about
      (setq about (cond (*base-uri* (net.uri:merge-uris (net.uri:parse-uri about)
                                                        (net.uri:parse-uri
                                                         (net.uri:render-uri *base-uri* nil))))
                        (*default-namespace* (net.uri:merge-uris (net.uri:parse-uri about)
                                                                 (net.uri:parse-uri
                                                                  #+:mswindows
                                                                  (let ((path (pathname *default-namespace*)))
                                                                    (substitute
                                                                     #\/ #\\
                                                                     (namestring
                                                                      (make-pathname :directory (pathname-directory path)
                                                                                     :name (pathname-name path)
                                                                                     :type (pathname-type path)))))
                                                                  #-:mswindows
                                                                  (namestring *default-namespace*))
                                                                 ))
                        (t (net.uri:parse-uri about))))
      (remf attrs '|rdf|:|about|))
    ;(format t "~%about:~S" about)
    (when id
      (setq id (net.uri:copy-uri (net.uri:parse-uri
                                  (net.uri:render-uri
                                   (or *base-uri* *default-namespace*) nil))
                                 :fragment id))
      (remf attrs '|rdf|:|ID|))
    (when nodeID
      (setq nodeID (nodeID2symbol nodeID))
      (remf attrs '|rdf|:|nodeID|))
    (when lang
      (when (stringp lang) (setq lang (intern lang "keyword")))
      (remf attrs '|xml|:|lang|))
    (setq attrs (loop for (prop val) on attrs by #'cddr
                    collect (cond ((property? prop)
                                   (let ((range (get-range (symbol-value prop))))
                                     (cond ((null range) (list prop val))
                                           ((and (symbolp range) (datatype? range))
                                            (list prop (read-as-datatype val range)))
                                           (t (list prop val)))))
                                  (t (list prop val)))))
    (when lang
      (setq attrs (cons `(|xml|:|lang| ,lang) attrs)))
    ;(format t "~%attrs:~S" attrs)
    ;(when (and (stringp about) (zerop (length about)))
    ;  (setq about *base-uri*))
    (cons class (cond (about (cons `(|rdf|:|about| ,about) (append attrs slots)))
                      (id (cons `(|rdf|:|ID| ,(uri2symbol id)) (append attrs slots)))
                      (t (append attrs slots))))))

(defun addRdfXml (description)
  (cond ((Description-P description)
         (let* ((form (Description-form description))
                (about (second (assoc '|rdf|:|about| (cdr form))))
                (id (second (assoc '|rdf|:|ID| (cdr form))))
                (nodeID (second (assoc '|rdf|:|nodeID| (cdr form))))
                (name (cond ((string= (string (Description-TAG description)) "Ontology")
                             (name-ontology about))
                            (about (uri2symbol about)))))
           (when id
             (assert (null nodeID))
             (setq form (cons (car form)
                              (remove (assoc '|rdf|:|ID| (cdr form)) (cdr form)))))
           (when nodeID
             (setq form (cons (car form)
                              (remove (assoc 'nodeID (cdr form)) (cdr form)))))
           (unless name
             (when id (setq name id))
             (when nodeID (setq name (nodeID2symbol nodeID))))
           ;(format t "~%~S" (list* (car form) `(:name ,name) (cdr form)))
           (addForm (list* (car form) `(:name ,name) (cdr form)) nil)))
        ((error "What ~S" description))))

;;;; Producer-Consumer Model
;;; It is much better to incrementaly process data in each small fragment from input stream, without
;;; waiting until whole contents of file are read. In this case, a fragment from input stream is
;;; parsed by a parser and the result is handed to an interpreter that processes parsed data. If two
;;; processes are concurrently cooperate through pipe-like data connection, we call such computation
;;; model producer-consumer model. In Unix, it will be implemented with two processes and pipe.
;;; In Scheme, it will be implemented as coroutine with continuation. Unfortunately, Common Lisp is
;;; not equiped such a native model.Then, following <make-rdfxml-reader> and <make-accepter-for-rdf>
;;; create a producer and a consumer dedicated to parsing and interpreting RDF file.
;;;
;;;

(defun make-rdfxml-reader (stream)
  "This function creates a creater of a producer in producer-consumer model.
   The returned function must take a consumer."
  #'(lambda (writer)
      (labels ((read-loop (writer)
                 (skipbl stream)
                 (cond ((RDFdeclEndp? stream)
                        (throw 'RDF-completed t)) ; stop reading
                       ((RDFdecl? stream)         ; eat up "<RDF"
                        (skipbl stream)
                        (let ((attributes
                               (loop until (match-pattern-p ">" stream)
                                   append (read-Attribute-in-RDF stream)
                                   do (skipbl stream))))
                          (skip-pattern ">" stream)
                          (funcall writer
                                   attributes
                                   #'(lambda (wrtr) (read-loop wrtr)))))
                       ((match-pattern-p "<?xml " stream)
                        (skip-pattern "<?xml " stream)
                        (funcall writer
                                 (parse-XMLDecl stream)
                                 #'(lambda (wrtr) (read-loop wrtr))))
                       ((match-pattern-p "<!DOCTYPE" stream)
                        (skip-pattern "<!DOCTYPE" stream)
                        (funcall writer
                                 (parse-doctypedecl stream)
                                 #'(lambda (wrtr) (read-loop wrtr))))
                       ((Comment? stream)
                        (funcall writer
                                 (parse-Comment stream)
                                 #'(lambda (wrtr) (read-loop wrtr))))
                       (t ; Description
                        (funcall writer
                                 (parse-Description stream)
                                 #'(lambda (wrtr) (read-loop wrtr)))))))
        (read-loop writer))))

(defun make-accepter-for-rdf (accepter-fun)
  "This function creates a creater of a consumer function in producer-consumer model.
   The returned function must take a producer."
  (let ((*autoepistemic-local-closed-world* nil))
    #'(lambda (reader)
        (labels ((accept-loop (element reader)
                              ;(format t "~%Reading ... ~S" element)
                              (cond ((null element)     ; initial calling and null attributes
                                     (funcall reader #'(lambda (elm rdr) (accept-loop elm rdr))))
                                    ((consp element)    ; attributes in top envelop
                                     (let ((ontouri (loop for (key val) on element  by #'cddr
                                                        when (string= key "owlx:name")
                                                        return val)))
                                       (when ontouri (name-ontology ontouri))))
                                    ((XMLDecl-P element)
                                     nil)
                                    ((doctypedecl-p element)
                                     nil)
                                    ((comment-p element)
                                     nil)
                                    ((Description-P element)
                                     (let ((*base-uri* *base-uri*)
                                           (*default-namespace* *default-namespace*))
                                       (funcall accepter-fun element)))
                                    ((error "Can't happen")))))
          (accept-loop nil reader)))))

(defun read-rdf-file (accepter-fun &optional (file (ask-user-rdf-file)) (code :default))
  "reads an rdf <file> and hands parsed data to <accepter-fun>. If XMLDecl in <file>
   includes any code statement, it is set as character-code of this <file>. If no statement
   for character-code in <file> and <code> supplied, then this <file> is parsed in <code>.
   In parsing, QNames of referenced resources and defined resources are stored into
   <*referenced-resources*> and <*defined-resources*>. At the end this procedure,
   the set difference of <*referenced-resources*> and <*defined-resources*> is printed."
  (unless file (return-from read-rdf-file nil))
  (setq code (or (peep-XMLDecl-code-from-file file) code))
  (with-open-file (stream (pathname file) :external-format (excl:find-external-format code))
    (let ((*line-number* 1)
          (*line-pos* 0)
          (*pos* 0)
          (*default-namespace* (or *default-namespace*
                                   (cond ((stringp file) file)
                                         ((pathnamep file) (namestring file))
                                         ((error "Cant happen!")))))
          (*base-uri* *base-uri*)
          (reader (make-rdfxml-reader stream))
          (accept (make-accepter-for-rdf accepter-fun)))
      (flush-buf)
      (catch 'RDF-completed
             (loop (funcall accept reader)))))
  (let ((refered (set-difference *referenced-resources* *defined-resources* :key #'car)))
    ;(warn "Defined resources: ~{~S ~}" *defined-resources*)
    ;(warn "Referenced resources: ~{~S ~}" *referenced-resources*)
    (when refered
      (warn "REFERENCED BUT NOT DEFINED RESOURCES: ~{~S ~}" refered)))
  :done)

(defun read-rdf-from-string (accepter-fun rdf-string &optional (code :default))
  (unless rdf-string (return-from read-rdf-from-string nil))
  (when (equal rdf-string "") (return-from read-rdf-from-string nil))
  (unless (stringp rdf-string) (return-from read-rdf-from-string nil))
  (setq code (or (peep-XMLDecl-code-from-string rdf-string) code))
  (with-input-from-string (stream rdf-string ;  :external-format (excl:find-external-format code)
                                  )
    (let ((*line-number* 1)
          (*base-uri* *base-uri*)
          (reader (make-rdfxml-reader stream))
          (accept (make-accepter-for-rdf accepter-fun)))
      (flush-buf)
      (catch 'RDF-completed
             (loop (funcall accept reader)))))
  (let ((refered (set-difference *referenced-resources* *defined-resources* :key #'car)))
    ;(warn "Defined resources: ~{~S ~}" *defined-resources*)
    ;(warn "Referenced resources: ~{~S ~}" *referenced-resources*)
    (when refered
      (warn "REFERENCED BUT NOT DEFINED RESOURCES: ~{~S ~}" refered)))
  'done)

(defun ask-user-rdf-file ()
  "asks an rdf file to user."
  #+:common-graphics
  (cg:ask-user-for-existing-pathname
   "" :allowed-types '(("RDF/XML format file" . "*.rdf")
                       ("OWL/XML format file" . "*.owl")
                       ("Any file" . "*.*")))
  #-:common-graphics
  (progn
    (format t "~%RDF/XML format file name? ")
    (let ((filename (read-line t)))
      (if (zerop (length filename)) nil filename))))

#|
:cd C:\allegro-projects\SWCLOS\RDFS\  -> C:\allegro-projects\SWCLOS\RDFS\
(with-open-file (p "Intro.rdf") (parse-rdf p))
->
(<?xml version="1.0" ?>
<|rdf|:|RDF| |xmlns|:|rdf|="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         |xmlns|:|gxpr|="http://galaxy-express.co.jp/MEXT/RDF/0.1/Prolog#"
         xmlns="http://galaxy-express.co.jp/MEXT/RDF/0.1/Prolog#">
  <|rdf|:|Property| |rdf|:|ID|="likes"/>
  <|rdf|:|Description| |rdf|:|ID|="Kim"><gxpr:likes |rdf|:|resource|="#Robin" /></|rdf|:|Description|>
  <|rdf|:|Description| |rdf|:|ID|="Sandy"><gxpr:likes |rdf|:|resource|="#Lee" /><gxpr:likes |rdf|:|resource|="#Kim" /></|rdf|:|Description|>
  <|rdf|:|Description| |rdf|:|ID|="Robin"><gxpr:likes |rdf|:|resource|="#cats" /></|rdf|:|Description|>
</|rdf|:|RDF|>)

:cd C:\allegro-projects\SWCLOS\RDFS\IntroExample
(with-open-file (p "Example.rdf") (parse-rdf p))
|#

;; End of module
;; --------------------------------------------------------------------
