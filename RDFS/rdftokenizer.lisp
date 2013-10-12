;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; Tokenizer
;;;
;;; This module is programed by Seiji Koide with inspiration obtained from Doug Cutting's code for Lucene original.
;;; See, "An Object-Oriented Architecture for Text Retrieval"
;;;
;;; Copyright (c) 2008 Seiji Koide
;;;
;; History
;; -------
;; Oct.23,2008    File created
;;; ==================================================================================

(cl:provide :rdftokenizer)

(cl:defpackage :gx
  (:shadow nil)
  (:use :common-lisp)
  (:export *line-number* *line-pos* *pos*)
  )

(in-package :gx)

;;;
;;;; RDF Tokenizer
;;;
;;; Specail tokens are '<RDF', '<|rdf|:|RDF|', '<?', '<?xml', '<!ATTLIST', '<![CDATA[', '<!--',  '<!DOCTYPE', and '<!ENTITY'.
;;;
;;; [16]    PI    ::=    '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
;;; [23]    XMLDecl    ::=    '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
;;; [19]    CDStart    ::=    '<![CDATA['
;;; [15]    Comment    ::=    '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
;;; [28]    doctypedecl    ::=
;;;             '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
;;; [71]    GEDecl       ::=    '<!ENTITY' S Name S EntityDef S? '>'
;;; [72]    PEDecl       ::=    '<!ENTITY' S '%' S Name S PEDef S? '>'
;;; [52]    AttlistDecl    ::=    '<!ATTLIST' S Name AttDef* S? '>'
;;; [40]    STag    ::=    '<' Name (S Attribute)* S? '>' [WFC: Unique Att Spec]
;;; [5]    Name    ::=    NameStartChar (NameChar)*
;;; [4a]    NameChar    ::=
;;;          NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] |
;;;          [#x203F-#x2040]
;;; [4]    NameStartChar    ::=
;;;         ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] |
;;;         [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] |
;;;         [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] |
;;;         [#x10000-#xEFFFF]
;;; Namespaces[6]    NCNameStartChar    ::=    NameStartChar - ':'
;;; Namespaces[5]    NCNameChar    ::=    NameChar - ':'
#|
(defclass rdftokenizer ()
  ((char-stream :initarg :char-stream))
  (:documentation "Tokenizer dedicated to RDF format stream"))
(defmethod next-token ((token-stream rdftokenizer))
  (with-slots (char-stream) token-stream
    (with-output-to-string (string-stream)  ; token is stored into string-stream and returned as string.
      (let ((in-token-p nil))
        (loop (let ((char (read-char char-stream nil)))
                (cond ((null char) ; EOF
                       (if in-token-p (return) (return-from next-token nil)))
                      ((char= char #\<)
                       (let ((nextchar (peek-char nil char-stream nil)))
                         (cond ((NCNameStartChar-p nextchar)
                                (write-char char string-stream)
                                (return))
                               ((char= nextchar #\?)    ; XMLDecl or PI
                                (write-char char string-stream)
                                (write-char (read-char char-stream nil) string-stream)
                                (cond ((char= (peek-char nil char-stream nil) #\x)
                                       (write-char (read-char char-stream nil) string-stream)
                                       (cond ((char= (peek-char nil char-stream nil) #\m)
                                              (write-char (read-char char-stream nil) string-stream)
                                              (cond ((char= (peek-char nil char-stream nil) #\l)
                                                     ;; xml read up
                                                     (write-char (read-char char-stream nil) string-stream)
                                                     (return))
                                                    (t (error "OOps! Sorry you cannot use '<?xm' in this version."))))
                                             (t (error "OOps! Sorry you cannot use '<?x' in this version."))))
                                      (t ;; PI
                                       (return))))
                               ((char= nextchar #\!)    ; ATTLIST, CDATA, comment, DOCTYPE, ENTITY
                                (cond ((char= (peek-char nil char-stream nil) #\-)
                                       (write-char (read-char char-stream nil) string-stream)
                                       (cond ((char= (peek-char nil char-stream nil) #\-)
                                              (write-char (read-char char-stream nil) string-stream)
                                              ;; comment
                                              (return))
                                             (t (error "Illegal XML form at line number ~S" *line-number*))))
                                      (t ;; ATTLIST, CDATA, DOCTYPE, ENTITY
                      ((char= char #\>) (return))
                      ((and (not in-token-p) (NameStartChar-p char))
                       (write-char char string-stream)
                       (setq in-token-p t))
                                       ((and in-token-p (
                                                         |#
