(in-package gx-user)

;(defpackage :ub)

(defun q1 ()
  (loop for student in (collect-all-instances-of |ub|::|GraduateStudent|) with results
      when (cl:member |ub|::|Department0.University0.GraduateCourse0|
                      (gx::mklist (get-value student |ub|::|takesCourse|)))
      do (push student results)
      finally (return results)))

(defun q2 ()
  (let ((students (collect-all-instances-of |ub|::|GraduateStudent|))
        (universities (collect-all-instances-of |ub|::|University|)))
    (loop for student in students with results
        do
          (loop for department in (gx::mklist (get-value student |ub|::|memberOf|))
              when (typep department |ub|::|Department|)
              do
                (loop for university in universities
                    when (and (cl:member university (gx::mklist (get-value student |ub|::|undergraduateDegreeFrom|)))
                              (cl:member university (gx::mklist (get-value department |ub|::|subOrganizationOf|))))
                    do (push (cl:list student university department) results)))
        finally (return results))))

(defun q3 ()
  (loop for publication in (collect-all-instances-of |ub|::|Publication|) with results
      when (cl:member |ub|::|Department0.University0.AssistantProfessor0| 
                      (gx::mklist (get-value publication |ub|::|publicationAuthor|)))
      do (push publication results)
      finally (return results)))

(defun q4 ()
  (loop for professor in (collect-all-instances-of |ub|::|Professor|) with results
      when (cl:member |ub|::|Department0.University0.| (gx::mklist (get-value professor |ub|::|worksFor|)))
      do (loop for name in (gx::mklist (get-value professor |ub|::|name|))
             do 
               (loop for email in (gx::mklist (get-value professor |ub|::|emailAddress|))
                   do 
                     (loop for tel in (gx::mklist (get-value professor |ub|::|telephone|))
                         do (push (cl:list professor name email tel)
                                  results))))
      finally (return results)))

(defun q5 ()
  (loop for person in (collect-all-instances-of |ub|::|Person|) with results
      when (cl:member |ub|::|Department0.University0.| (gx::mklist (get-value person |ub|::|memberOf|)))
      do (push person results)
      finally (return results)))

(defun q6 ()
  (collect-all-instances-of |ub|::|Student|))

(defun q7 ()
  (loop for student in (collect-all-instances-of |ub|::|Student|) with results
      and courses = (gx::mklist (get-value |ub|::|Department0.University0.AssociateProfessor0| |ub|::|teacherOf|))
      do 
        (loop for course in courses 
            when (cl:member course (gx::mklist (get-value student |ub|::|takesCourse|)))
            do (push (cl:list student course) results))
      finally (return results)))

(defun q8 ()
  (let ((departments (collect-all-instances-of |ub|::|Department|))
        (students (collect-all-instances-of |ub|::|Student|)))
    (loop for department in departments with results
        when (cl:member |ub|::|University0.| (gx::mklist (get-value department |ub|::|subOrganizationOf|)))
        do
          (loop for student in students
              when (cl:member department (gx::mklist (get-value student |ub|::|memberOf|)))
              do
                (loop for email in (gx::mklist (get-value student |ub|::|emailAddress|))
                    do (push (cl:list student department email)
                             results)))
        finally (return results))))

(defun q9 ()
  (loop for student in (collect-all-instances-of |ub|::|Student|) with results
      do 
        (loop for faculty in (gx::mklist (get-value student |ub|:|advisor|))
            when (typep faculty |ub|:|Faculty|)
            do
              (loop for course in (gx::mklist (get-value faculty |ub|:|teacherOf|))
                  when (and (typep course |ub|:|Course|)
                            (cl:member course (gx::mklist (get-value student |ub|:|takesCourse|))))
                  do (push (cl:list student faculty course)
                           results)))
      finally (return results)))

(defun q10 ()
  (loop for student in (collect-all-instances-of |ub|::|Student|)
      when (cl:member |ub|::|Department0.University0.GraduateCourse0|
                      (gx::mklist (get-value student |ub|::|takesCourse|)))
      collect student))

(defun q11 ()
  (loop for research-group in (collect-all-instances-of |ub|::|ResearchGroup|)
      when (some #'(lambda (x) (subsumed-p x |ub|::|University0.|))
                 (gx::mklist (get-value research-group |ub|::|subOrganizationOf|)))
      collect research-group))
#|
(defun q11 ()
  (loop for rgroup in (collect-all-instances-of |ub|::|ResearchGroup|)
      when (cl:member |ub|::|University0.|
                      (gx::mklist (get-value rgroup |ub|::|subOrganizationOf|)))
      collect rgroup))
|#
(defun q12 ()
  (loop for chair in (collect-all-instances-of |ub|::|Chair|) with results
      do
        (loop for department in (gx::mklist (get-value chair |ub|::|worksFor|))
            when (and (typep department |ub|::|Department|)
                      (cl:member |ub|::|University0.| (gx::mklist (get-value department |ub|::|subOrganizationOf|))))
            do (push (cl:list chair department) results))
      finally (return results)))
        
(defun q13 ()
  (loop for person in (collect-all-instances-of |ub|:|Person|) with results
      and values = (gx::mklist (get-value |ub|::|University0.| |ub|::|hasAlumnus|))
      do 
        (loop for val in values
            when (rdf-equalp person val)
            do (push person results))
      finally (return results)))

(defun q14 ()
  (collect-all-instances-of |ub|::|UndergraduateStudent|))

#+lispworks
(defmacro excl::gc (&rest args)
       	`(hcl::gc-generation ,@args))

(defun query()
  (format t "~%=========================================================================")
  (format t "~%-- query 1 ------------------")
  (excl::gc t)
  (format t "~%Number of Answer:~S" (length (time (q1))))
  (format t "~%-- query 2 ------------------")
  (excl::gc t)
  (format t "~%Number of Answer:~S" (length (time (q2))))
  (format t "~%-- query 3 ------------------")
  (excl::gc t)
  (format t "~%Number of Answer:~S" (length (time (q3))))
  (format t "~%-- query 4 ------------------")
  (excl::gc t)
  (format t "~%Number of Answer:~S" (length (time (q4))))
  (format t "~%-- query 5 ------------------")
  (excl::gc t)
  (format t "~%Number of Answer:~S" (length (time (q5))))
  (format t "~%-- query 6 ------------------")
  (excl::gc t)
  (format t "~%Number of Answer:~S" (length (time (q6))))
  (format t "~%-- query 7 ------------------")
  (excl::gc t)
  (format t "~%Number of Answer:~S" (length (time (q7))))
  (format t "~%-- query 8 ------------------")
  (excl::gc t)
  (format t "~%Number of Answer:~S" (length (time (q8))))
  (format t "~%-- query 9 ------------------")
  (excl::gc t)
  (format t "~%Number of Answer:~S" (length (time (q9))))
  (format t "~%-- query 10 -----------------")
  (excl::gc t)
  (format t "~%Number of Answer:~S" (length (time (q10))))
  (format t "~%-- query 11 -----------------")
  (excl::gc t)
  (format t "~%Number of Answer:~S" (length (time (q11))))
  (format t "~%-- query 12 -----------------")
  (excl::gc t)
  (format t "~%Number of Answer:~S" (length (time (q12))))
  (format t "~%-- query 13 -----------------")
  (excl::gc t)
  (format t "~%Number of Answer:~S" (length (time (q13))))
  (format t "~%-- query 14 -----------------")
  (excl::gc t)
  (format t "~%Number of Answer:~S" (length (time (q14))))
  (format t "~%=========================================================================")
  )

(eval-when (:execute :load-toplevel)

  )