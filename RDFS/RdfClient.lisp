;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;
;; RDF Client
;;
;; IT Program Project in Japan: 
;;    Building Operation-Support System for Large-scale System using IT
;;
;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;; for the realization of the MEXT IT Program in Japan,
;;
;; Copyright © 2006 by Galaxy Express Corporation, Japan.
;;
;; History
;; -------
;; 2007.12.05    make-function-input-stream is found 
;; 2006.01.23    File created

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :gx
    (:export read-plane-from-http read-nt-from-http read-rdf-from-http read-from-http *default-proxy*)
    )
  (require :aserve)
  (require :rdfparser)
  )
(in-package :gx)

;;
;; "kuuhon:onto;dir1;dir2;dir3;file.rdf" ->
;; "C:\\Program Files\\Apache Software Foundation\\Apache2.2\\htdocs\\ontologies\\dir1\\dir2\\dir3\\file.rdf"
;;

(defconstant BUFLEN 10240 "10KB, the number of character in buffer")
(defvar *default-proxy* "150.73.3.41" "default proxy, this should be set if the organization has a proxy.")

(setf (logical-pathname-translations "kuuhon")
  '(("onto;**;*.*" "C:/Program Files/Apache Software Foundation/Apache2.2/htdocs/ontologies/*.*")))

(defvar *rdf-repository* (parse-namestring "kuuhon:onto;"))

(defun uri2repository-path (uri &optional (repository *rdf-repository*))
  (setq uri (uri uri))
  (let* ((path (net.uri:uri-path uri))
         (file (pathname-name path))
         (type (pathname-type path))
         (dir (cons :relative (cdr (pathname-directory path))))
         (relative-path (make-pathname :directory dir :name file :type type))
         (abs-path (merge-pathnames relative-path repository)))
    abs-path))
#|
(defun cached-alive-p (uri path &key (proxy *default-proxy*) (encoding :latin1-base))
  "has this uri cached file as path in the rdf repository?"
  (and (prove-file path)
       (let* ((client-request (net.aserve.client:make-http-client-request uri
                                :accept "text/*"
                                :user-agent "SWCLOS"
                                :proxy proxy
                                :external-format encoding))
              (header (net.aserve.client:client-request-headers client-request)))
         

(defun rdf-fetch (uri)
  (setq uri (uri uri))
  (case (uri-scheme uri)
    (:http (let ((path (uri2repository-path uri)))
             (cond ((cached-alive-p uri path)))))
    (otherwise (error "Not Yet!"))))
|#


(defun pipe-for-http (ostream httpuri proxy encoding)
  "This function creates a pipe for http stream with the combination of excl:make-function-input-stream.
   <httpuri> must be a uri object. The scheme of the uri must be nil or 'http'.
   The first arg is the output stream that will be input stream in a partner input process."
  (let ((client-request (net.aserve.client:make-http-client-request httpuri
                          :accept "text/*"
                          :user-agent "SWCLOS"
                          :proxy proxy
                          :external-format encoding))
        (status-code cl:nil))
    (net.aserve.client:read-client-response-headers client-request)
    (case (setq status-code (net.aserve.client:client-request-response-code client-request))
      (200
       (let ((headers (net.aserve.client:client-request-headers client-request))
             (bytes-left (net.aserve.client::client-request-bytes-left client-request))
             (buffer (make-array BUFLEN :element-type 'cl:character)))
         (loop until (zerop (setq length 
                                  (net.aserve.client:client-request-read-sequence buffer client-request)))
             do (cond ((< bytes-left BUFLEN)
                       (princ (subseq buffer 0 bytes-left) ostream))
                      (t (princ buffer ostream)))
               (decf bytes-left length))
         (net.aserve.client:client-request-close client-request))
       (values))
      (otherwise
       ;; this procedure must be processed by another thread.
       (net.aserve.client:client-request-close client-request)
       (error "Status Code:~S ~S" status-code
         (net.aserve.client:client-request-response-comment client-request))))))

(defun read-plane-from-http (httpuri &optional (proxy *default-proxy*) (encoding :latin1-base))
  "just reads the content of <httpuri> and print out to *standard-output* .
   <httpuri> can be a uri object or a string. The scheme of the uri must be nil or 'http'."
  (unless httpuri (return-from read-plane-from-http cl:nil))
  (setq httpuri (uri httpuri))
  #|
  (excl:with-function-input-stream (http-stream #'pipe-for-http httpuri proxy encoding)
    (loop as line = (read-line http-stream cl:nil :EOF)
        until (eq line :EOF)
        do (write-line line))) |#
  (let ((http-stream (excl:make-function-input-stream #'pipe-for-http httpuri proxy encoding)))
    (unwind-protect
        (progn (loop as line = (read-line http-stream common-lisp:nil :EOF)
                   until (eq line :EOF)
                   do (write-line line)))
      (close http-stream)))
  cl:nil)

(defun read-nt-from-http (accepter-fn httpuri &optional (proxy *default-proxy*) (encoding :latin1-base))
  "<httpuri> can be a uri object or a string. The scheme of the uri must be nil or 'http'."
  (unless httpuri (return-from read-nt-from-http cl:nil))
  (setq httpuri (uri httpuri))
  #|
  (excl:with-function-input-stream (http-stream #'pipe-for-http httpuri proxy encoding)
    (loop as line = (read-line http-stream cl:nil :EOF)
        until (eq line :EOF)
        do (apply accepter-fn (parse-triple-line line)))) |#
  (let ((http-stream (excl:make-function-input-stream #'pipe-for-http httpuri proxy encoding)))
    (unwind-protect
        (progn (loop as line = (read-line http-stream common-lisp:nil :EOF)
                   until (eq line :EOF)
                   do (apply accepter-fn (parse-triple-line line))))
      (close http-stream)))
  )

(defun read-rdf-from-http (accepter-fun httpuri &optional (proxy *default-proxy*) (encoding :latin1-base))
  "<httpuri> can be a uri object or a string. The scheme of the uri must be nil or 'http'."
  (unless httpuri (return-from read-rdf-from-http cl:nil))
  (setq httpuri (uri httpuri))
  (setq *referenced-resources* cl:nil)
  (let ((http-stream (excl:make-function-input-stream #'pipe-for-http httpuri proxy :latin1-base)))
    (unwind-protect (progn (setq encoding (or (%peep-XMLDecl-code http-stream) encoding)))
      (close http-stream)))
  (let ((http-stream (excl:make-function-input-stream #'pipe-for-http httpuri proxy encoding)))
    (unwind-protect (progn (let ((*line-number* 1)
                                 (*default-namespace* httpuri)
                                 (*base-uri* httpuri)
				 (cl::*readtable* |rdf|:*standard-readtable*)
                                 (reader (make-rdfxml-reader http-stream))
                                 (accept (make-accepter-for-rdf accepter-fun)))
                             (flush-buf)
                             (catch 'RDF-completed
                                    (loop (funcall accept reader)))))
      (close http-stream)))
  (let ((refered (set-difference *referenced-resources* *defined-resources* :key #'car)))
    ;(warn "Defined resources: ~{~S ~}" *defined-resources*)
    ;(warn "Referenced resources: ~{~S ~}" *referenced-resources*)
    (when refered
      (warn "REFERENCED BUT NOT DEFINED RESOURCES: ~{~S ~}" refered)))
  :done)

(defun read-from-http (httpuri &optional file-type (proxy *default-proxy*) (encoding :latin1-base))
  "if <file-type> is t, then this function reads the contents of <httpuri> and classifies it.
   if <file-type> is nil, then this function just reads the contents and print out it.
   otherwise, <file-type> must match the type in <httpuri> and classifies it, if matched."
  (unless httpuri (return-from read-from-http cl:nil))
  (setq httpuri (uri httpuri))
  (let ((type (pathname-type (merge-pathnames (uri-path (uri httpuri)) ""))))
    (cond ((null file-type)
           (read-plane-from-http httpuri proxy encoding))
          ((or (eq file-type t) (string= (string file-type) type))
           (cond ((string= type "nt")
                  (read-nt-from-http #'addTriple-from-file httpuri proxy encoding))
                 ((string= type "rdf")
                  (read-rdf-from-http #'addRdfXml httpuri proxy encoding))
                 ((string= type "owl")
                  (read-rdf-from-http #'addRdfXml httpuri proxy encoding))
                 ((error "Illegal type designated: ~A" type))))
          ((error "file type mismatch: ~A for ~A" type httpuri)))))

#|
(in-package :gx-user)
(setq *default-proxy* "150.73.3.41")
(read-nt-from-http #'(lambda (s p o)
                       (cond ((and (null s) (null p) (null o)) (terpri))
                             ((and (null p) (null o)) ;(format t "~%#~A" s))
                             (t ;(format t "~%~A ~A ~A ." s p o))))
                   "http://www.w3.org/2000/10/rdf-tests/rdfcore/rdfs-domain-and-range/test001.nt")
(read-nt-from-http #'addTriple-from-file
                   "http://www.w3.org/2000/10/rdf-tests/rdfcore/rdfs-domain-and-range/test001.nt")

(read-plane-from-http "http://www.w3.org/2000/10/rdf-tests/rdfcore/horst-01/test001.rdf")
(read-rdf-from-http #'addRdfXml "http://www.w3.org/2000/10/rdf-tests/rdfcore/horst-01/test001.rdf")
(read-rdf-from-http #'addRdfXml "http://www.w3.org/TR/2003/PR-owl-guide-20031215/food.rdf")
;; Caution! OpenCyc is very huge.
(read-from-http "http://www.cyc.com/2004/06/04/cyc")
|#

;; End of module
;; --------------------------------------------------------------------

(provide :rdfclient)
