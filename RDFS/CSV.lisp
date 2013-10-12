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
                 ((string= charpos #\　) (%smatches name (1+ pos) pnames len))    ; skip Japanese space
                 (t (let ((founds (mappend #'(lambda (pname) (%same-char charpos (cdr pname) (car pname))) pnames)))
                      (cond (founds (%smatches name (1+ pos) founds len))
                            (t ;; results
                             (mapcar #'(lambda (pname) (cdr pname)) pnames))))))))
        (t (mapcar #'(lambda (pname) (cdr pname)) pnames))))
(defun %same-char (char1 name pos)
  (let ((char2 (char (string name) pos)))
    (cond ((string= char2 #\Space) (%same-char char1 name (1+ pos)))
          ((string= char2 #\　) (%same-char char1 name (1+ pos)))
          ((char= char1 char2) (cl:list (cons (1+ pos) name))))))



(defpackage ihi)
(defResource ihi::Property (|rdf|:|type| |rdfs|:|Class|)
  (|rdfs|:|subClassOf| |rdf|:|Property|))

;;PC使用者
(defProperty ihi::事業本部： (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::使用者)
  (|rdfs|:|range| ihi::事業本部))
(defProperty ihi::部門： (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::使用者)
  (|rdfs|:|range| ihi::部門))
(defProperty ihi::メール略語： (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::使用者)
  (|rdfs|:|range| ihi::メール略語))
(defProperty ihi::地区： (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::使用者)
  (|rdfs|:|range| ihi::地区))
(defProperty ihi::個人コード： (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::使用者)
  (|rdfs|:|range| ihi::個人コード))
(defProperty ihi::使用ＰＣ： (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::使用者)
  (|rdfs|:|range| ihi::新ﾎｽﾄ名))

(defProperty ihi::個人名： (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::個人コード)
  (|rdfs|:|range| ihi::使用者))

;;PC新ﾎｽﾄ名
(defProperty ihi::使用者： (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::新ﾎｽﾄ名)
  (|rdfs|:|range| ihi::使用者))
(defProperty ihi::管理番号： (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::新ﾎｽﾄ名)
  (|rdfs|:|range| ihi::管理番号))

;; PC管理番号
(defProperty ihi::新ﾎｽﾄ名： (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::管理番号)
  (|rdfs|:|range| ihi::新ﾎｽﾄ名))
(defProperty ihi::ＰＣタイプ： (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::管理番号)
  (|rdfs|:|range| ihi::ＰＣタイプ))
(defProperty ihi::設置場所： (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::管理番号)
  (|rdfs|:|range| ihi::設置場所))
(defProperty ihi::旧ﾎｽﾄ名： (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::管理番号)
  (|rdfs|:|range| ihi::旧ﾎｽﾄ名))
(defProperty ihi::旧IPｱﾄﾞﾚｽ： (|rdf|:|type| ihi::Property)
  (|rdfs|:|domain| ihi::管理番号)
  (|rdfs|:|range| ihi::旧IPｱﾄﾞﾚｽ))

(defun load-csv ()
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\デスクトップ\\空情シ（本社）ディスク.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\デスクトップ\\空情シ（本社）ノート.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\デスクトップ\\空情シ（相馬）デスク.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\デスクトップ\\空情シ（相馬）ノート.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\デスクトップ\\空情シ（富岡）デスク.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\デスクトップ\\空情シ（富岡）ノート.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\デスクトップ\\空情シ（瑞穂）デスク.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\デスクトップ\\空情シ（瑞穂）ノート.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\デスクトップ\\空情シ（田無）デスク.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\デスクトップ\\空情シ（田無）ノート.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\デスクトップ\\空情シ（昭島）デスク.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\デスクトップ\\空情シ（昭島）ノート.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\デスクトップ\\空情シ（呉２）デスク.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\デスクトップ\\空情シ（呉２）ノート.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\デスクトップ\\空情シ（相生）デスク.csv"
                           "ihi" "ihi" ihi::Property)
  (build-ontology-from-csv "C:\\Documents and Settings\\HT0005\\デスクトップ\\空情シ（相生）ノート.csv"
                           "ihi" "ihi" ihi::Property)
  )

#|
(defProperty |rdf|:|Property| (|rdf|:|type| |rdfs|:|Class|)
  (|rdfs|:|subClassOf| |rdfs|:|Class|))
(read-rdf-file #'addRdfXml "C:\\allegro-projects\\RDFS\\vcard30.rdf")
|#


