;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;; RDFWalker
;;;
;;; This program is newly developed for the efficient RDF parsing, which reads RDF/XML data from stream
;;; and directly translates into RDF objects or CLOS objects.
;;;
;;; copyright (c) 2008 Seiji Koide
;;;
;; History
;; -------
;; 2008.08.11    file created, based on http://www.w3.org/TR/2004/REC-rdf-syntax-grammar-20040210/
;;; ==================================================================================

(defpackage :gx
  (:export rdf-walker))

(in-package :gx)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :rdf)
  (require :rdfskernel)
  ) ; end of eval-when

(defun walk-Description (stream &optional role)
  (if (match-pattern-p "<" stream)
      (if (CDStart-p stream) (read-CData-to-CDEnd stream)
        (multiple-value-bind (name attrs contents) (read-Description-for-walk stream role)
          (let ((about (getf attrs '|rdf|:|about|))
                (id (getf attrs '|rdf|:|ID|))
                (nodeID (getf attrs '|rdf|:|nodeID|))
                (lang (getf attrs '|xml|:|lang|))
                (slots contents))
            (when about
              (setq about (cond (*base-uri* (net.uri:merge-uris (net.uri:parse-uri about) *base-uri*))
                                (t (net.uri:parse-uri about))))
              (remf attrs '|rdf|:|about|))
            (when id
              (setq id (net.uri:copy-uri (net.uri:parse-uri
                                          (net.uri:render-uri (or *base-uri* *default-namespace*) cl:nil))
                                         :fragment id))
              (remf attrs '|rdf|:|ID|))
            (when nodeID
              (setq nodeID (nodeID2symbol nodeID))
              (remf attrs '|rdf|:|nodeID|))
            (when lang
              (when (stringp lang) (setq lang (intern (string-downcase lang) "keyword")))
              (remf attrs '|xml|:|lang|))
            (setq attrs (loop for (prop val) on attrs by #'cddr
                            collect (cond ((property? prop)
                                           (let ((range (get-range (symbol-value prop))))
                                             (cond ((null range) (cl:list prop val))
                                                   ((and (symbolp range) (datatype? range))
                                                    (cl:list prop (read-as-datatype val range)))
                                                   (t (cl:list prop val)))))
                                          (t (cl:list prop val)))))
            (when lang
              (setq attrs (cons `(|xml|:|lang| ,lang) attrs)))
            (cons name (cond (about (cons `(|rdf|:|about| ,about) (append attrs slots)))
                             (id (cons `(|rdf|:|ID| ,(uri2symbol id)) (append attrs slots)))
                             (t (append attrs slots)))))))
    (read-plane-text stream)))

(defun read-Description-for-walk (stream &optional role)
  "reads a description from <stream> and returns tag, attributes, and contents.
   Note that contents are a list of sexpr instead of RDF/XML.
   Any comments are discarded."
  (multiple-value-bind (tag attributes) (read-STag-or-EmptyElemTag-in-Description stream)
    ;(format t "~%Tag:~S Att:~S" tag attributes)
    (cond ((match-pattern-p ">" stream)
           (read-pattern-p ">" stream)
           (skipbl stream)  ; this line might be deleted to be exact in case of leaf nodes, but need at intermediate node
           (let ((contents (loop until (ETag-p-with-eat-up tag stream)
                               unless (when (Comment? stream) (parse-Comment stream))
                               collect (walk-property stream role)
                               do (skipbl stream))))
             (values (QNameString2symbol tag) attributes contents)))
          ((match-pattern-p "/>" stream)
           (read-pattern-p "/>" stream)
           (values (QNameString2symbol tag) attributes cl:nil))
          ((error "Cant happen!")))))

(defun walk-property (stream role)
  "reads an predicate/object from <stream> and returns a tuple of prop-name, attributes, and value."
  (if (match-pattern-p "<" stream)
      (multiple-value-bind (name atts value) (read-property-for-walk stream role)
        ;(format t "~%Name:~S Atts:~S Value:~S" name atts value)
        (let ((resource (getf atts '|rdf|:|resource|))
              (nodeID (getf atts '|rdf|:|nodeID|))
              (datatype (getf atts '|rdf|:|datatype|))
              (lang (getf atts '|xml|:|lang|)))
          (cond (nodeID
                 (remf atts '|rdf|:|nodeID|)
                 (assert (null resource) () "resource cannot be placed with nodeID.")
                 (assert (null value))
                 (assert (null atts))
                 (cl:list name (nodeID2symbol nodeID)))
                (resource
                 (cond ((char= (elt resource 0) #\#)
                        (cl:list name (net.uri:copy-uri
                                       (net.uri:parse-uri
                                        (net.uri:render-uri (or *base-uri* *default-namespace*) cl:nil))
                                       :fragment (subseq resource 1))))
                       (t (let ((rsc (net.uri:uri resource)))
                            (cond ((net.uri:uri-scheme rsc) (cl:list name rsc))
                                  (t (let ((base (net.uri:render-uri
                                                  (or *base-uri* *default-namespace*)
                                                  cl:nil)))
                                       (cond ((char= #\/ (char base (1- (length base))))
                                              (cl:list name
                                                       (net.uri:uri (concatenate 'cl:string base resource))))
                                             (t (cl:list name rsc))))))))))
                (datatype
                 (remf atts '|rdf|:|datatype|)
                 (assert (null atts))
                 (assert (length=1 value))
                 (cl:list name (read-as-datatype (car value) (uri2symbol (uri datatype)))))
                (lang
                 (cons name (cons (intern (string-downcase lang) "keyword") value)))
                (atts (error "From Here Seiji!"))
                (t (cons name value)))))
    (read-plane-text stream)))

(defun read-property-for-walk (stream role)
  "reads a property/value pair from <stream> and returns <prop-name>, <attributes>, and consed <value>."
  (multiple-value-bind (tag attributes) (read-STag-or-EmptyElemTag-in-property stream)
    (let ((name (QNameString2symbol tag)))
      (cond ((match-pattern-p ">" stream)
             (read-pattern-p ">" stream)
             (skipbl stream)
             (let ((parse (getf attributes '|rdf|:|parseType|)))
               (if parse
                   (cond ((string= parse "Resource")
                          (remf attributes '|rdf|:|parseType|)
                          (values name attributes
                                  (cons '|rdf|:|Description|
                                        (loop until (ETag-p-with-eat-up tag stream)
                                            unless (when (Comment? stream) (parse-Comment stream))
                                            collect (walk-property stream role)
                                            do (skipbl stream)))))
                         ((string= parse "Literal")
                          (remf attributes '|rdf|:|parseType|)
                          (values name attributes
                                  (cl:list (^^ (read-string-until-Etag-with-eat-up tag stream)
                                               (symbol-value '|rdf|:|XMLLiteral|)))))
                         ((string= parse "Collection")
                          (remf attributes '|rdf|:|parseType|)
                          (values name attributes
                                  (loop until (ETag-p-with-eat-up tag stream)
                                      unless (when (Comment? stream) (parse-Comment stream))
                                      collect (walk-Description stream role)
                                      do (skipbl stream))))
                         ((error "Not Yet parseType ~A" (getf attributes '|rdf|:|parseType|))))
                 (values name attributes
                         (loop until (ETag-p-with-eat-up tag stream)
                             unless (when (Comment? stream) (parse-Comment stream))
                             collect (walk-Description stream role)
                             do (skipbl stream))))))
            ((match-pattern-p "/>" stream)
             (read-pattern-p "/>" stream)
             (values name attributes cl:nil))
            ((error "Cant happen!"))))))

(defun walk-rdf (stream)
  (skipbl stream)
  (cond ((RDFdeclEndp? stream)
         (throw 'RDF-completed t)) ; stop reading
        ((RDFdecl? stream)
         (skipbl stream)
         (let ((attributes
                (loop until (match-pattern-p ">" stream)
                    append (read-Attribute-in-RDF stream)
                    do (skipbl stream))))
           (declare (ignore attributes))
           (skip-pattern ">" stream)
           cl:nil)) ; attributes are attributes for RDF
        ((match-pattern-p "<?xml " stream)
         (skip-pattern "<?xml " stream)
         (parse-XMLDecl stream))
        ((match-pattern-p "<!DOCTYPE" stream)
         (skip-pattern "<!DOCTYPE" stream)
         (parse-doctypedecl stream))
        ((Comment? stream)
         (parse-Comment stream))
        (t ; Description
         (walk-Description stream))))

(defun walk-rdf-file (accepter-fun &optional (file (ask-user-rdf-file)) (code :default))
  (unless file (return-from walk-rdf-file cl:nil))
  (setq code (or (peep-XMLDecl-code-from-file file) code))
  (with-open-file (stream (pathname file) :external-format (excl:find-external-format code))
    (let ((*line-number* 1)
          (*line-pos* 0)
          (*pos* 0)
          (*default-namespace* (or *default-namespace* file))
          (*base-uri* *base-uri*)
          (sexpr cl:nil))
      (flush-buf)
      (catch 'RDF-completed
             (loop (setq sexpr (walk-rdf stream))
                   (when (consp sexpr) (funcall accepter-fun sexpr))))))
  (let ((refered (set-difference *referenced-resources* *defined-resources* :key #'car)))
    ;(warn "Defined resources: ~{~S ~}" *defined-resources*)
    ;(warn "Referenced resources: ~{~S ~}" *referenced-resources*)
    (when refered
      (warn "REFERENCED BUT NOT DEFINED RESOURCES: ~{~S ~}" refered)))
  :done)

#|
(walk-rdf-file #'print)
(walk-rdf-file #'addForm)
|#
