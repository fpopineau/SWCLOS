(in-package :gx)

(defun get-token (delimiter string start)
  (let ((pos (position delimiter string :start start)))
    (cond ((null pos)
           (cond ((= start (length string)) (values "" start))
                 (t (values (string-trim '(#\Space #\Tab #\Newline) (subseq string start)) (length string)))))
          ((= start pos) (values "" pos))
          (t (values (string-trim '(#\Space #\Tab #\Newline) (subseq string start pos)) pos)))))

(defun read-csv-line (&optional input-stream eof-error-p eof-value recursive-p)
  (let ((line (read-line input-stream eof-error-p eof-value recursive-p)))
    (if (eq line eof-value) (return-from read-csv-line eof-value))
    (loop with len = (length line) and pos = 0 and token
        do (multiple-value-setq (token pos) (get-token #\, line pos))
          (incf pos)
        collect token
        while (< pos len))))

(defun read-csv (input-stream &optional eof-error-p eof-value)
  (when (not (eq eof-value (peek-char t input-stream eof-error-p eof-value)))
    (loop for lst = (read-csv-line input-stream eof-error-p eof-value)
        while (not (eq lst eof-value))
        collect lst)))

(defun build-ontology-from-csv (file item-namespace-string data-namespace-string this-property
                                &optional (filter #'all-digits-in-first-column-p))
  (let ((llstr (with-open-file (stream file) (read-csv stream cl:nil :eof))))
    (let ((items (car llstr))
          (datalist (cdr llstr))
          (itempkg (or (find-package item-namespace-string) (make-package item-namespace-string)))
          (datapkg (or (find-package data-namespace-string) (make-package data-namespace-string))))
      (setq items (mapcar #'(lambda (str) (intern str itempkg)) items))
      (loop for data in datalist
          when (funcall filter data)
          do (setq data (loop for datum in data
                              for item in items
                            when (and (not (string= datum ""))
                                      (class? item))
                            collect (addInstance (symbol-value item) (intern datum datapkg))))
            (loop for subj in data
                when (class-of subj)
                do (loop for obj in data with prop

                       when (and (not (eq subj obj))
                                  (setq prop (find-if #'(lambda (p)
                                                         (some #'(lambda (c) (typep obj c)) (mklist (get-range p))))
                                                     (remove-if-not #'(lambda (p) (typep p this-property))
                                                                    (mapcar #'symbol-value
                                                                      (collect-prop-names-from (class-of subj)))))))
                       do (addTriple subj prop obj)
                         ))
            )))
  :done)

(defun all-digits-in-first-column-p (data)
  (and (not (= (length (car data)) 0)) (every #'digit-char-p (car data))))

(defun sretrieve (namestr class)
  "retlieve instances of <class> whose name matches to <namestr>."
  (let* ((firstChar (char namestr 0))
         (instances (remove-if-not #'(lambda (ins)
                                       (and (name ins)
                                            (char= firstChar (char (string (name ins)) 0))))
                                   (collect-all-instances class))))
    (when instances
      (%smatches namestr 1 (mapcar #'(lambda (ins) (cons 1 (name ins))) instances)))))
(defun %smatches (name pos pnames &optional (len (length name)))
  (cond ((< pos len)
         (let ((charpos (char name pos)))
           (cond ((string= charpos #\Space) (%smatches name (1+ pos) pnames len)) ; skip blank
                 ((string= charpos #\�@) (%smatches name (1+ pos) pnames len))    ; skip Japanese space
                 (t (let ((founds (mappend #'(lambda (pname) (%same-char charpos (cdr pname) (car pname))) pnames)))
                      (cond (founds (%smatches name (1+ pos) founds len))
                            (t ;; results
                             (mapcar #'(lambda (pname) (cdr pname)) pnames))))))))
        (t (mapcar #'(lambda (pname) (cdr pname)) pnames))))
(defun %same-char (char1 name pos)
  (let ((char2 (char (string name) pos)))
    (cond ((string= char2 #\Space) (%same-char char1 name (1+ pos)))
          ((string= char2 #\�@) (%same-char char1 name (1+ pos)))
          ((char= char1 char2) (cl:list (cons (1+ pos) name))))))



(defpackage ihi)
(defResource ihi::Property (|rdf|:|type| |rdfs|:|Class|)
  (|rdfs|:|subClassOf| |rdf|:|Property|))

;;PC�g�p��
(defProperty ihi::���Ɩ{���F (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::�g�p��)
  (|rdfs|:|range| ihi::���Ɩ{��))
(defProperty ihi::����F (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::�g�p��)
  (|rdfs|:|range| ihi::����))
(defProperty ihi::���[������F (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::�g�p��)
  (|rdfs|:|range| ihi::���[������))
(defProperty ihi::�n��F (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::�g�p��)
  (|rdfs|:|range| ihi::�n��))
(defProperty ihi::�l�R�[�h�F (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::�g�p��)
  (|rdfs|:|range| ihi::�l�R�[�h))
(defProperty ihi::�g�p�o�b�F (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::�g�p��)
  (|rdfs|:|range| ihi::�VνĖ�))

(defProperty ihi::�l���F (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::�l�R�[�h)
  (|rdfs|:|range| ihi::�g�p��))

;;PC�VνĖ�
(defProperty ihi::�g�p�ҁF (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::�VνĖ�)
  (|rdfs|:|range| ihi::�g�p��))
(defProperty ihi::�Ǘ��ԍ��F (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::�VνĖ�)
  (|rdfs|:|range| ihi::�Ǘ��ԍ�))

;; PC�Ǘ��ԍ�
(defProperty ihi::�VνĖ��F (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::�Ǘ��ԍ�)
  (|rdfs|:|range| ihi::�VνĖ�))
(defProperty ihi::�o�b�^�C�v�F (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::�Ǘ��ԍ�)
  (|rdfs|:|range| ihi::�o�b�^�C�v))
(defProperty ihi::�ݒu�ꏊ�F (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::�Ǘ��ԍ�)
  (|rdfs|:|range| ihi::�ݒu�ꏊ))
(defProperty ihi::��νĖ��F (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::�Ǘ��ԍ�)
  (|rdfs|:|range| ihi::��νĖ�))
(defProperty ihi::��IP���ڽ�F (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::�Ǘ��ԍ�)
  (|rdfs|:|range| ihi::��IP���ڽ))

(defun load-csv ()
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\�f�X�N�g�b�v\\���V�i�{�Ёj�f�B�X�N.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\�f�X�N�g�b�v\\���V�i�{�Ёj�m�[�g.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\�f�X�N�g�b�v\\���V�i���n�j�f�X�N.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\�f�X�N�g�b�v\\���V�i���n�j�m�[�g.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\�f�X�N�g�b�v\\���V�i�x���j�f�X�N.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\�f�X�N�g�b�v\\���V�i�x���j�m�[�g.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\�f�X�N�g�b�v\\���V�i����j�f�X�N.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\�f�X�N�g�b�v\\���V�i����j�m�[�g.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\�f�X�N�g�b�v\\���V�i�c���j�f�X�N.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\�f�X�N�g�b�v\\���V�i�c���j�m�[�g.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\�f�X�N�g�b�v\\���V�i�����j�f�X�N.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\�f�X�N�g�b�v\\���V�i�����j�m�[�g.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\�f�X�N�g�b�v\\���V�i���Q�j�f�X�N.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\�f�X�N�g�b�v\\���V�i���Q�j�m�[�g.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\�f�X�N�g�b�v\\���V�i�����j�f�X�N.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\�f�X�N�g�b�v\\���V�i�����j�m�[�g.csv"
                           "ihi" "ihi" ihi::Property)
  )

#|
(defProperty |rdf|:|Property| (|rdf|:|type| |rdfs|:|Class|)
  (|rdfs|:|subClassOf| |rdfs|:|Class|))
(read-rdf-file #'addRdfXml "C:\\allegro-projects\\RDFS\\vcard30.rdf")
|#


