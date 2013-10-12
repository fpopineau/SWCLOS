;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;
;; RDF Browser
;;
;; Copyright (c) 2007 by Seiji Koide.
;;
;; History
;; -------
;; 2007.12.07    File created

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :gx
    (:export )
    )
  (require :aserve)
  (require :rdfparser)
  (require :rdfclient)
  )
(in-package :gx)

;;
;;
;;

(net.aserve:start :port 8000 :external-format (excl::crlf-base-ef :utf-8))
(net.aserve:publish
    :path "/"
    :content-type "text/html; charset=utf-8"
    :function 'welcome-page)

(defun welcome-page (req ent)
  (net.aserve:with-http-response (req ent)
    (net.aserve:with-http-body (req ent)
      (net.html.generator:html
       (:html
        (:head  (:title "RDF Browser"))
        (:body
         (:p (:h1 "Welcome to RDF Browser Page!"))
         "Enter URL: "
         ((:form :action "/rbrowser")
          ((:input :type "text" :name "url" :size 80))
          ((:input :type "submit")))))))))

(net.aserve:publish
 :path "/rbrowser"
 :content-type "text/html; charset=utf-8"
 :function 'rbrowser)
(defun rbrowser (req ent)
  (net.aserve:with-http-response (req ent)
    (net.aserve:with-http-body (req ent)
      (let ((urlstr (net.aserve:request-query-value "url" req)))
        (net.html.generator:html
         (:html
          (:head  (:title "RDF Browser1"))
          (:body
           (cond ((= (length urlstr) 0)
                  (net.html.generator:html
                   "Please input URL for RDF/XML or N-triple contents: "
                   ((:form :action "/rbrowser")
                    ((:input :type "text" :name "url" :size 80))
                    ((:input :type "submit")))))
                 (t (let ((*standard-output* *standard-output*)
                          (*error-output* *error-output*)
                          (type (pathname-type (merge-pathnames (uri-path (uri urlstr)) ""))))
                      (setq *subjects-defined* ())
                      (with-open-file (ostream "outfile4rbrowser" :direction :output :if-exists :overwrite)
                        (with-open-file (estream "errorfile4rbrowser" :direction :output :if-exists :overwrite)
                          (setq *standard-output* ostream)
                          (setq *error-output* estream)
                          (cond ((string= type "nt")
                                 (read-nt-from-http #'addTriple-from-file httpuri proxy encoding))
                                ((string= type "rdf")
                                 (read-rdf-from-http #'addRdfXml httpuri proxy encoding))
                                ((string= type "owl")
                                 (read-rdf-from-http #'addRdfXml httpuri proxy encoding))
                                ((error "Illegal type designated: ~A" type))))))
                    (nreverse *subjects-defined*)
                    (cond ((null *subjects-defined*)
                           (net.html.generator:html
                            (:html
                             (:body
                              (:p (format cl:nil "No subjects in ~A: " urlstr))
                              "Please input URL for RDF/XML or N-triple contents: "
                              ((:form :action "/rbrowser")
                               ((:input :type "text" :name "url"))
                               ((:input :type "submit")))))))
                          (t (net.html.generator:html
                              (:html
                               (:body
                                ((:iframe (:name "output") (:src "outfile4rbrowser") (:height "400")))
                                ((:iframe (:name "error") (:src "errorfile4rbrowser") (:height "400")))
                                (loop for subj in *subjects-defined*
                                    collect
                                      (net.html.generator:html
                                       (:span
                                        (format nil "~A" (or (name subj) "Anonomous"))
                                        :br
                                        (loop for (role filler) in (get-slots subj)
                                            collect
                                              (net.html.generator:html
                                               (:span
                                                (format nil "   ~8A : ~A" role (or (name filler)
                                                                                   (slot-value filler '|rdf|:|about|)
                                                                                   (get-form filler)))
                                                :br))))))))))))))))))))

#|
(in-package :gx-user)
(setq *default-proxy* "150.73.3.41")
;; access to http://www.w3.org/2000/10/rdf-tests/rdfcore/rdfs-domain-and-range/test001.nt
|#
;; End of module
;; --------------------------------------------------------------------

(provide :rdfio)
