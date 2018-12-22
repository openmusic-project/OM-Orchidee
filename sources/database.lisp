(in-package :om)

(defvar *orchidee-db-path* nil)
(defparameter *db-names* nil)

;;; set the db path and load the .mat if the db has already been analysed...
(defun set-db-path (path)
  (when *orchidee-app-process*
    (orchidee-msg 5 "/acknowledge" "/closesession" 0))
  (if (and path (probe-file path))
      (let ()
        ;(unless (and *orchidee-db-PATH*
        ;         (string-equal (namestring path) (namestring *orchidee-db-PATH*)))
        (setf *orchidee-db-PATH* path)
        (when *orchidee-app-process*
          (let ((dbmat-file (make-pathname :directory (pathname-directory *orchidee-db-path*)
                                           :name "om-orchidee-db" :type "mat")))
            (if (and (probe-file dbmat-file) *orchidee-app-process*)
                (progn
                  (print (string+ "LOADING DATA BASE CONTENTS: " (namestring dbmat-file)))
                  (orchidee-msg 'progress-bar "/ready" "/dbload" 102
                                (namestring (make-pathname :directory (pathname-directory *orchidee-db-path*)
                                                           :name "om-orchidee-db" :type "mat"))))
              (om-message-dialog "Warning: The current Orchidée session knowledge does probably not correspond with the Data Base contents. Use 'Update Database' to synchronize.")
              )))
        )
    (when *orchidee-app-process* (orchidee-msg 5 "/ready" "/dbreset" 102))
    )
  (when *orchidee-app-process* 
    (orchidee-msg 5 "/acknowledge" "/newsession" 0))
  )


; (orchidee-msg 5 "/dbqueryfields" "/dbgetqueryfields" 444 "message")
; (orchidee-msg 5 "/dbfieldvaluelist" "/dbgetfieldvaluelist" 444 "message" "instrument")

; (update-database)

(defmethod! db-names ()
   :icon 899
   (let ((rep (orchidee-msg 5 "/dbfieldvaluelist" "/dbgetfieldvaluelist" 321 "message" "dbname")))
     (when rep
       (nthcdr 3 rep))))



(defun update-database (&optional tmppath)
  (let ((path (or tmppath *orchidee-db-path*)))
    (if (null (probe-file path))
        (om-message-dialog "No valid database path has been set. Please select a database folder before to upgrade.")
      (when (om-y-or-n-dialog "Warning: Updating Data Base requires closing the current Orchidee. All current settings and knowledge will be reset. Do you want to proceed ?")
        
                
        ;;; close session
        (orchidee-msg 5 "/acknowledge" "/closesession" 0)

        (setf *db-names* (mapcar #'(lambda (path) (car (last (pathname-directory path))))
                                 (om-directory (make-pathname :directory (append (pathname-directory path) '("sounds")))
                                               :directories t :files nil)))
        ;;; remove unused xml
        (mapcar #'(lambda (path) (unless (member (car (last (pathname-directory path))) *db-names* :test 'string-equal)
                                   (om-delete-directory path)))
                       (om-directory (make-pathname :directory (append (pathname-directory path) '("xml")))
                                     :directories t :files nil))

        ;;; genere les xml pour chaque dossier
        ;(orchidee-msg 'progress-bar "/ready" "/dbanalyzesamples" 101 
        ;              (namestring (make-pathname :directory (append (pathname-directory path) (list "sounds"))))
        ;              (namestring (make-pathname :directory (append (pathname-directory path) (list "xml")))))
        
        (mapcar #'(lambda (db) 
                    (orchidee-msg 'progress-bar "/ready" "/dbanalyzesamples" 101 
                                  (namestring (make-pathname :directory (append (pathname-directory path) (list "sounds" db))))
                                  (namestring (make-pathname :directory (append (pathname-directory path) (list "xml")))))
                    )
                *db-names*)
        
        ;;; update la base
        ;;; 1) ORCHIDEE + new sounds
        (cond ((member "orchidee-forum" *db-names* :test 'string-equal)
               (orchidee-msg 5 "/ready" "/dbreset" 102)
               (orchidee-msg 'progress-bar "/ready" "/dbupdate" 102
                             (namestring (make-pathname :directory (append (pathname-directory path) (list "xml"))))))
              (t 
               (orchidee-msg 'progress-bar "/ready" "/dbmake" 103
                             (namestring (make-pathname :directory (append (pathname-directory path) (list "xml")))))
               ))
  
        ;;; sauve
        (let ((db-file (make-pathname :directory (pathname-directory path)
                                      :name "om-orchidee-db" :type "mat")))
          (when (probe-file db-file)
            (om-delete-file db-file))
          (orchidee-msg 'progress-bar "/ready" "/dbsave" 102 (namestring db-file)))
        
        ;;; open session
        (setf *orchidee-db-path* path)
        (orchidee-msg 5 "/acknowledge" "/newsession" 0)
    
    ))))



