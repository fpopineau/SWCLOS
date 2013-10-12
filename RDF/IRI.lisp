;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; IRI module
;;;

(cl:provide :iri)

(eval-when (:execute :load-toplevel :compile-toplevel)

  ) ; end of eval-when

(cl:defpackage :gx
  (:shadow uri )
  (:use :common-lisp :net.uri)
  (:export "iri" "boundp" "bound-value" 
   ))

(in-package :gx)

;;;
;;;; IRI in SWCLOS and Rbase system
;;;
;;; Every uri for RDF is globally unique in the WWW. Therefore, a uri in SWCLOS must 
;;; be unique. The uniqueness of uri is assured by interning a uri. 
;;;
;;; A triple subject/predicate/object in RDF is embodied as CLOSobject/slotname/slotvalue
;;; in SWCLOS, and subjective CLOSobject is bound to the subjective uri. Precisely, 
;;; a subjective uri is an instance of class <iri> in SWCLOS and Rbase that is subclass 
;;; of <net.uri:uri> in ACL library. 
;;;
;;; Read macro `\<' reads a uri string and produces an <iri>.
;;; A uri reference is internalized to an instance of class <iri>.
;;; An instance of class <iri> is externalized (printed by `%W') as the same 
;;; appearance of input uri data.
;;; ----------------------------------------------------------------------------------
;;; <http://www.w3.org/2000/01/rdf-schema#Resource>    -> 
;;;                                   <http://www.w3.org/2000/01/rdf-schema#Resource>
;;; rdfs:Resource (if defined as node)                 -> rdfs:Resource
;;; (eq <http://somewhere> <http://somewhere>)         -> true
;;; (eq <http://some%20where> <http://some%20where>)   -> true
;;; (eq <http://somewhere> <http://some%20where>)      -> false
;;; ----------------------------------------------------------------------------------
;;;
;;; An instance of <iri> has an extra slot for value just like QName. <iri-boundp> and 
;;; <iri-value> is available for an <iri> just like <boundp> and <symbol-value>.
;;;
;;; Two trailing characters '\<\<' returns a value bound to the <iri>.
;;; See, reader macro <gx::double-angle-bracket-reader>.

(eval-when (:execute :compile-toplevel :load-toplevel)
  (proclaim '(inline iri-p boundp iri-value bound-value)))

(defclass iri (net.uri:uri)
  ((value :accessor iri-value))
  (:documentation "iri in SWCLOS and Rbase that is a subclass of net.uri:uri and able to bind a value to, 
just like lisp symbol."))

(defmethod print-object ((iri iri) stream)
  (if *print-escape*
      (format stream "<~a>" (render-uri iri nil))
    (render-uri iri stream)))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (proclaim '(inline iri-p iri-boundp iri-value)))

(defun iri-boundp (x)
  "Is <x> a uri and bound at its value slot?"
  (etypecase x
    (string (iri-boundp (iri x)))
    (iri (slot-boundp x 'value))))

(defmethod iri-value ((str string))
  "returns bound value of iri value from <str>."
  (iri-value (iri str)))

(defmacro %iri-value (uri)
  "This macro should be used by programmers, when <uri> is definitely <uri> here."
  (slot-value uri 'value))

(defun iri-p (x)
  "Is <x> an instance of iri?"
  (cl:typep x 'iri))

#|
(defmethod iri ((sym symbol))
  (cond ((cl:boundp sym)
         (let ((val (symbol-value sym)))
           (cond ((cl:typep val 'node) (slot-value val 'iri))
                 (t (error "~S is not in RDF universe." sym)))))
        (t (error "Not defined symbol ~S." sym))))

(defun boundp (x)
  "Is <x> a symbol or a iri and bound at its value slot?"
  (etypecase x
    (symbol (cl:boundp x))
    (iri (slot-boundp x 'value))
    (node t)))

(defun bound-value (x)
  "returns a bound value of <x>."
  (etypecase x
    (symbol (symbol-value x))
    (iri (slot-value x 'value))
    (node x)))

(defmethod (setf bound-value) (value (x symbol))
  (setf (symbol-value x) value))
(defmethod (setf bound-value) (value (x iri))
  (setf (iri-value x) value))
|#
;;;
;;;; IRI Escaping
;;;
;;; See, rfc2396 for URI escaping

(defun iri-reserved-char-p (char)
  "Is this <char> reserved for iri?"
  (or (char= char #\:)
      (char= char #\/)
      (char= char #\?)
      (char= char #\@)
      (char= char #\$)
      (char= char #\&)
      (char= char #\+)
      (char= char #\,)
      (char= char #\;)
      (char= char #\=)
      ))

(defun iri-marked-char-p (char)
  "Is this <char> marked for iri?"
  (or (char= char #\-)
      (char= char #\_)
      (char= char #\.)
      (char= char #\~)
      (char= char #\!)
      (char= char #\')
      (char= char #\()
      (char= char #\))
      (char= char #\*)
      ))

(defun iri-delimiter-p (char)
  (or (char= char #\<)
      (char= char #\>)
      (char= char #\#)
      (char= char #\%)
      (char= char #\")))

(defun iri-unwise-p (char)
  (or (char= char #\{)
      (char= char #\})
      (char= char #\|)
      (char= char #\\)
      (char= char #\^)
      (char= char #\[)
      (char= char #\])
      (char= char #\`)))

(defun iri-escape (str)
  "This function performs Percent-Encoding. Namely, RESERVED CHARACTERS, DELIMITERS, and UNWISE 
   CHARACTERS for URI that is contained in <str> are escaped with percent(%) character to a triplet 
   of \<% HEXDIG HEXDIG&gt;. Spaces and newlines are removed from <str>."
  (cond ((and (> (length str) 5) (string= "http:" (subseq str 0 5))) str) ; this is for ontology URIs
        (t (flet ((space-p (c)
                           (declare (optimize (speed 3) (safety 1)))
                           (let ((code (char-code c)))
                             (or (eq code #x20)
                                 (eq code #x9)
                                 (eq code #xD)
                                 (eq code #xA)))))
             (%iri-escape (%iri-escape-for-delimiter (remove-if #'space-p str)))))))

(defun %iri-escape-for-delimiter (str)
  "escapes uri delimiter char in <str> before making uri."
  (let ((pos 0))
    (cond ((setq pos (position-if #'iri-delimiter-p str)) ;; found
           (let ((c (char str pos)))
             (concatenate 'cl:string
               (subseq str 0 pos)
               (format nil "%~X"  (char-code c))
               (%iri-escape-for-delimiter (subseq str (1+ pos))))))
          (t str))))
(defun %iri-escape (str)
  "encodes the uri reserved characters to hexadecimals."
  (let ((pos 0))
    (cond ((setq pos (position-if #'iri-reserved-char-p str))
           (let ((c (char str pos)))
             (concatenate 'cl:string
               (subseq str 0 pos)
               (format nil "%~X"  (char-code c))
               (%iri-escape (subseq str (1+ pos))))))
          (t str))))

(defun iri-de-escape (str)
  "This function decodes Percent-Encoding to characters."
  (let ((pos 0))
    (cond ((setq pos (position #\% str :test #'char=))
           (let ((c (code-char (parse-integer (subseq str (1+ pos) (min (length str) (+ pos 3)))
                                              :radix 16))))
             (concatenate 'cl:string 
               (subseq str 0 pos)
               (string c)
               (iri-de-escape (subseq str (+ pos 3))))))
          (t str))))

;;;
;;;; URI APIs fixes
;;;

(defun null-iri-p (uri)
  "returns true if <uri> is nil, null string, or <uri> is a uri and its rendered string is null."
  (etypecase uri
    (null t)
    (string (string= "" uri))
    (net.uri:uri (string= "" (net.uri:render-uri uri nil)))))

;;;
;;;; IRI Methods
;;;
;;; Three methods are defined for generic function <iri>: when <thing> is a iri (instance of 
;;; class iri), its interned value is returned. When <thing> is a string, <intern-uri> 
;;; is applied to create a interned uri from the string. 
;;;
;;; A uri is always interned for a given uri string, because the uniqueness is required for uri 
;;; to be bound to a value. This notion is the same as lisp symbol.

(defmethod iri ((thing iri))
  "returns interned <thing> for class <gx:iri>."
  (intern-uri thing))

(defmethod iri ((thing net.uri:uri))
  "change class <net.uri:uri> of <thing> to <iri> and returns interned <thing>."
  (when (and (net.uri:uri-host thing) (null (net.uri:uri-path thing)))
    (setf (net.uri:uri-path thing) "/"))
  (unless (cl:typep thing 'iri)
    (change-class thing 'iri))
  (intern-uri thing))

;;;
;;; See also, 
;;; \<a href='http://www.franz.com/support/documentation/8.1/doc/operators/uri/parse-uri.htm\>parse-uri'\</a\>
;;; in ACL document.
;;;

(defmethod iri ((thing cl:string))
  "when iri host exists but no iri path on the iri in <thing>, this method adds '/' to iri path. 
   This is required for the namespace interning."
  ;(setq str (substitute-pattern "&" "&#38;" str))   ; "&#38;" is converted to "&"
  (let ((parsed (net.uri:parse-uri thing)))
    (when (and (net.uri:uri-host parsed) (null (net.uri:uri-path parsed)))
      (setf (net.uri:uri-path parsed) "/"))
    (unless (cl:typep parsed 'iri)
      (change-class parsed 'iri))
    (intern-uri parsed)
    ))

(defmethod iri ((thing t))
  "signals an error."
  (error "Cannot coerce ~S to a gx:iri"))

;;;
;;; To list all iris for resource, call <list-all-entity-uris>. See gxutils module.
;;;

;; End of module
;; --------------------------------------------------------------------
;;;
;;; Seiji Koide Nov-15-2010
;;;
