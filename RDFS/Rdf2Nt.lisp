;;;-*- mode: common-lisp; syntax: common-lisp; package: rdf; base: 10 -*-
;;;
;;; Rdf module
;;;
;;; IT Program Project in Japan:
;;;    Building Operation-Support System for Large-scale System using IT
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan,
;;;
;;; Copyright © 2002,2004,2006
;;;    Galaxy Express Corporation
;;;
;;; History
;;; -------
;;; 2004.07.07    Rdf2Nt is separated here from Rdf.

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :rdf)
  )

(in-package :gx)

;;;
;;; RDF/XML to NTriple Translator
;;;

(defun make-producer (stream)
  #'(lambda (writer)
      (labels ((read-loop (writer)
                 (skipbl stream)
                 (cond ((RDFdecl? stream)
                        (multiple-value-bind (tag attributes) (read-STag-or-EmptyElemTag-in-RDF stream)
                          (declare (ignore attributes))
                          (assert (eq tag '|rdf|:|RDF|))
                          ;; attributes???
                          (cond ((match-pattern-p ">" stream)
                                 (read-pattern-p ">" stream)
                                 (read-loop writer))
                                ((match-pattern-p "/>" stream)
                                 (read-pattern-p "/>" stream)
                                 :EOF)                             ; stop reading
                                ((error "Cant happen!")))))
                       ((ETag-p-with-eat-up '|rdf|:|RDF| stream) :EOF) ; stop reading
                       ((XMLDecl? stream)
                        (funcall writer
                                 (parse-XMLDecl stream)
                                 #'(lambda (wrtr) (read-loop wrtr))))
                       ((doctypedecl? stream)
                        (funcall writer
                                 (parse-doctypedecl stream)
                                 #'(lambda (wrtr) (read-loop wrtr))))
                       ((Comment? stream)
                        (funcall writer
                                 (parse-Comment stream)
                                 #'(lambda (wrtr) (read-loop wrtr))))
                       ((eq (peek-next-char stream) :EOF)
                        (error "EOF encountered."))
                       (t ; Description
                        (funcall writer
                                 (parse-Description stream)
                                 #'(lambda (wrtr) (read-loop wrtr)))))))
        (read-loop writer))))

(defun comment->nt (comment outstream)
  (let ((stream (make-string-input-stream (comment-body comment)))
        (line '()))
    (loop until (eq :EOF (setq line (read-line stream cl:nil :EOF)))
        do (format outstream "#~A~%" line))))

(defun slot->nt (subj prop outstream)
  (%slot->nt subj (prop-name prop) (prop-att&vals prop) (prop-value prop) outstream))

(defun %slot->nt (subj name attvals value outstream)
  ;(format t "%slot->nt: ~S ~S ~S ~S ~S~%" subj name attvals value outstream)
  (setq subj
        (cond ((stringp subj) (format cl:nil "<~A>" subj))
              ((and (symbolp subj) (eql (symbol-package subj) (find-package :_)))
               (setq subj (QName2PrefixedName subj)))
              (t (setq subj (format cl:nil "~S" subj)))))
  (cond ((null attvals)
         (format outstream "~A <~A> ~S .~%" subj (symbol2uri name) (car value)))
        ((eq (car attvals) '|rdf|:|datatype|)
         (format outstream "~A <~A> ~S^^<~A> .~%"
           subj (symbol2uri name) (car value) (second attvals)))
        ((and (eq (car attvals) '|rdf|:|parseType|) (string= (cadr attvals) "Literal"))
         (format outstream "~A <~A> ~S^^<~A> .~%"
           subj (symbol2uri name) (car value) (net.uri:render-uri (symbol2uri '|rdf|:|XMLLiteral|) cl:nil)))
        ((eq (car attvals) '|xml|:|lang|)
         (cond ((cl:member '|rdf|:|parseType| attvals)
                (%slot->nt (subseq subj 1 (1- (length subj))) name (cddr attvals) value outstream))
               (t (format outstream "~A <~A> ~S@~A .~%"
                    subj (symbol2uri name) (car value) (cadr attvals)))))
        (t (format outstream "~A <~A> ~S .~%"
             subj (symbol2uri name) (car attvals) (second attvals)))))

(defun description->nt (description outstream)
  (%description->nt (Description-TAG description)
                    (Description-ATT&VALS description)
                    (Description-slots description)
                    cl:nil
                    outstream))

(defun %description->nt (tag attvals slots subject outstream)
  ;(format t "%description->nt: ~S ~S ~S ~S ~S~%" tag attvals slots subject outstream)
  (cond ((null attvals)
         (when (null subject) (setq subject (make-anonymous-nodeID "a")))
         (loop for slot in slots do (slot->nt subject slot outstream)))
        ((eq (car attvals) '|rdf|:|about|)
         (%description->nt tag (cddr attvals) slots (second attvals) outstream))
        ((eq tag 'Description)
         (when (null subject) (setq subject (make-anonymous-nodeID "a")))

         (cond ((stringp subject)
                (format outstream "<~A> <~A> ~S .~%"
                  subject (symbol2uri (car attvals)) (second attvals)))
               ((and (symbolp subject) (eql (symbol-package subject) (find-package :_)))
                (format outstream "<~A> <~A> ~S .~%"
                  (QName2PrefixedName subject) (symbol2uri (car attvals)) (second attvals)))
               (t (format outstream "~S <~A> ~S .~%"
                    subject (symbol2uri (car attvals)) (second attvals))))
         (when (or (cddr attvals) slots)
           (%description->nt tag (cddr attvals) slots subject outstream)))))

(defun make-consumer (stream)
  #'(lambda (element reader)
      (labels ((accept-loop (element reader)
                 (cond ((eq element :EOF) :EOF)
                       ((XMLDecl-P element) ;skip XMLDecl
                        (funcall reader #'(lambda (elm rdr) (accept-loop elm rdr))))
                       ((doctypedecl-p element) ; skip doctypedecl
                        (funcall reader #'(lambda (elm rdr) (accept-loop elm rdr))))
                       ((comment-p element) (comment->nt element stream)
                        (funcall reader #'(lambda (elm rdr) (accept-loop elm rdr))))
                       ((Description-P element) (description->nt element stream)
                        (funcall reader #'(lambda (elm rdr) (accept-loop elm rdr))))
                       (t (funcall reader #'(lambda (elm rdr) (accept-loop elm rdr)))))))
        (accept-loop element reader))))

(defun parse->nt (in-stream out-stream)
  (let ((producer (make-producer in-stream))    ; stream = in-stream
        (consumer (make-consumer out-stream)))  ; stream = out-stream
    (flush-buf)
    (setq *line-number* 0)
    ;; element = cl:nil reader = producer
    (funcall consumer cl:nil producer)))

;;; End of module
;;; --------------------------------------------------------------------

(provide :rdf2nt)
