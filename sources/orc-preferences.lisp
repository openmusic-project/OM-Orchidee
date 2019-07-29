(in-package :om)

;;; to be set in preferences or with set-orchidee-path


(defparameter *orchidee-PATH* "Orchidee")
(defparameter *orchidee-HOST* "127.0.0.1")
(defparameter *orchidee-OUT* 9998)
(defparameter *orchidee-IN* 9999)
(defparameter *print-osc-msg* t)

;;;=====================
;;; SVP PREFS


(add-external-pref-module 'orchidee)

(defmethod get-external-name ((module (eql 'orchidee))) "Orchidee")

(defmethod get-external-module-vals ((module (eql 'orchidee)) modulepref) (get-pref modulepref :orchidee-options))
(defmethod get-external-module-path ((module (eql 'orchidee)) modulepref) (get-pref modulepref :orchidee-path))
(defmethod set-external-module-vals ((module (eql 'orchidee)) modulepref vals) (set-pref modulepref :orchidee-options vals))
(defmethod set-external-module-path ((module (eql 'orchidee)) modulepref path) (set-pref modulepref :orchidee-path path))



;;; host outport inport
(defun def-orchidee-options ()
  (list "127.0.0.1" 9998 9999 nil t))

(defmethod get-external-def-vals ((module (eql 'orchidee)))
  (list :orchidee-path (om-default-application-path '("Orchidee-Forum") "Orchidee-0.3.3")
        :orchidee-options (def-orchidee-options)))

(defmethod save-external-prefs ((module (eql 'orchidee))) 
  `(:orchidee-path ,(om-save-pathname *orchidee-PATH*) 
    :orchidee-options (list ,*orchidee-HOST* ,*orchidee-OUT* ,*orchidee-IN* ,(om-save-pathname *orchidee-db-PATH*) ,*print-osc-msg*)))

; (nth 3 (get-pref (get-pref-by-icon 922) :orchidee-options))

(defmethod put-external-preferences ((module (eql 'orchidee)) moduleprefs)
  (let ((list-prefs (get-pref moduleprefs :orchidee-options)))
    (when list-prefs 
       (setf *orchidee-HOST* (nth 0 list-prefs))
       (setf *orchidee-OUT* (nth 1 list-prefs))
       (setf *orchidee-IN* (nth 2 list-prefs))
       (set-db-path (nth 3 list-prefs))
       (setf *print-osc-msg* (nth 4 list-prefs)))
    (when (get-pref moduleprefs :orchidee-path)
      (setf *orchidee-PATH* (get-pref moduleprefs :orchidee-path)))
    ))

;; eval when loading the lib
(when (find-pref-module :externals)
  (put-external-pref-values 'orchidee))


(defvar *tmp-db-path* nil)

(defmethod show-external-prefs-dialog ((module (eql 'orchidee)) prefvals)
  (let* ((rep-list (copy-list prefvals))
         (dialog (om-make-window 'om-dialog
                                 :window-title "Orchidee Options"
                                 :size (om-make-point 300 450)
                                 :position :centered
                                 :resizable nil :maximize nil :close nil))
         hosttxt outtxt intxt run stop dbpath printmsg
         (pos 0))
    (om-add-subviews dialog
                     
                     (om-make-dialog-item 'om-static-text (om-make-point 20 (incf pos 10)) (om-make-point 200 24) "Orchidee Server" :font *om-default-font3b*)

                     (om-make-dialog-item 'om-static-text (om-make-point 20 (incf pos 40)) (om-make-point 150 20) "Host:" :font *om-default-font1b*)
                     
                     (setf hosttxt (om-make-dialog-item 'om-editable-text (om-make-point 165 pos) (om-make-point 100 16) (nth 0 prefvals)
                                          :after-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item)))
                                                            (unless (string= "" text)
                                                              (setf (nth 0 rep-list) text)
                                                              )))
                                          :font *om-default-font1*))
                     
                     (om-make-dialog-item 'om-static-text (om-make-point 20 (incf pos 30)) (om-make-point 120 24) "Ports:" :font *om-default-font1b*)
                     (om-make-dialog-item 'om-static-text (om-make-point 80 pos) (om-make-point 120 24) "Receive" :font *om-default-font1*)
                     
                     (setf outtxt (om-make-dialog-item 'om-editable-text (om-make-point 220 pos) (om-make-point 40 18)
                                          (format nil "~D" (nth 1 prefvals)) 
                                          :after-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (read-from-string text))
                                                              (if (and (integerp number) (>= number 0)) ; (not (= number (nth 2 rep-list)))
                                                                  (setf (nth 1 rep-list) number)
                                                                (progn 
                                                                  (om-beep-msg "OSC port must be a positive integer")
                                                                  (om-set-dialog-item-text item (format nil "~D" (nth 1 rep-list))))
                                                                ))))
                                          
                                          :di-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (read-from-string text))
                                                              (if (and (integerp number) (>= number 0)) ; (not (= number (nth 2 rep-list)))
                                                                  (setf (nth 1 rep-list) number)
                                                                (progn 
                                                                  (om-beep-msg "OSC port must be an positive integer")
                                                                  (om-set-dialog-item-text item (format nil "~D" (nth 1 rep-list))))
                                                                ))))
                                          :font *om-default-font1*))
                     
                     (om-make-dialog-item 'om-static-text (om-make-point 80 (incf pos 30)) (om-make-point 120 24) "Send" :font *om-default-font1*)
                     
                     (setf intxt (om-make-dialog-item 'om-editable-text (om-make-point 220 pos) (om-make-point 40 13)
                                          (format nil "~D" (nth 2 prefvals)) 
                                          :after-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (read-from-string text))
                                                              (if (and (integerp number) (>= number 0)) ; (not (= number (nth 1 rep-list)))
                                                                  (setf (nth 2 rep-list) number)
                                                                (progn 
                                                                  (om-beep-msg "OSC port must be an integer")
                                                                  (om-set-dialog-item-text item (format nil "~D" (nth 2 rep-list))))
                                                                ))))
                                          :di-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (read-from-string text))
                                                              (if (and (integerp number) (>= number 0)) ; (not (= number (nth 1 rep-list)))
                                                                  (setf (nth 2 rep-list) number)
                                                                (progn 
                                                                  (om-beep-msg "OSC port must be an integer")
                                                                  (om-set-dialog-item-text item (format nil "~D" (nth 2 rep-list))))
                                                                ))))
                                          :font *om-default-font1*))

                     (setf run (om-make-dialog-item 'om-button (om-make-point 70 (incf pos 40)) (om-make-point 80 20) "Run"
                                          :enable t
                                          :di-action (om-dialog-item-act item
                                                       (run-orchidee)
                                                       ;(om-enable-dialog-item item nil)
                                                       ;(om-enable-dialog-item stop t)
                                                       )))

                     (setf stop (om-make-dialog-item 'om-button (om-make-point 150 pos) (om-make-point 80 20) "Stop"
                                          :enable t
                                          :di-action (om-dialog-item-act item
                                                      (quit-orchidee)
                                                      ;(om-enable-dialog-item item nil)
                                                      ;(om-enable-dialog-item run t)
                                                      )))
                     

                    (setf printmsg (om-make-dialog-item 'om-check-box (om-make-point 60 (incf pos 40)) (om-make-point 180 15) " Print OSC Messages" 
                                          :di-action (om-dialog-item-act item 
                                                       (setf (nth 4 rep-list) (om-checked-p item)))
                                          :font *om-default-font1*
                                          :checked-p (nth 4 prefvals)))
                     

                     (om-make-dialog-item 'om-static-text (om-make-point 20 (incf pos 40)) (om-make-point 200 24) "Sound DataBase" :font *om-default-font1b*)
                     (let ((path (or *tmp-db-path* *orchidee-db-PATH*)))
                       (setf dbpath (om-make-dialog-item 'om-static-text (om-make-point 20 (incf pos 20)) (om-make-point 200 40) 
                                                         (if path (namestring path) "unspecified")
                                                         :fg-color (if (and path (probe-file path)) *om-black-color* *om-red-color*)
                                                         :font *om-default-font1*)))

                     (om-make-dialog-item 'om-button (om-make-point 230 pos) (om-make-point 50 24) "..."
                                                      :di-action (om-dialog-item-act button
                                                                   (declare (ignore button))
                                                                   (let* ((path (om-choose-directory-dialog)))
                                                                     (when path 
                                                                       (if (probe-file path)
                                                                           (progn 
                                                                             (setf *tmp-db-path* path)
                                                                             (om-set-dialog-item-text dbpath (namestring path))
                                                                             (om-set-fg-color dbpath *om-black-color*)
                                                                             )
                                                                         (om-beep-msg (string+ "Bad path for " 
                                                                                               (get-external-name mod))))))))
                     
                     
                     
                     (setf run (om-make-dialog-item 'om-button (om-make-point 10 (incf pos 40)) (om-make-point 150 20) "Update DataBase..."
                                          :enable t
                                          :di-action (om-dialog-item-act item
                                                       (om-eval-enqueue '(update-database *tmp-db-path*))
                                                       )))
                     (om-make-dialog-item 'om-static-text (om-make-point 170 pos) (om-make-point 130 80) 
                                          "[synchronizes orchidée session's internal knowledge with the current DB contents]"
                                          :font *om-default-font1*
                                          )

                     ;;; boutons
                     (om-make-dialog-item 'om-button (om-make-point 20 (incf pos 110)) (om-make-point 100 20) "Restore..."
                                          :di-action (om-dialog-item-act item
                                                       (om-set-dialog-item-text hosttxt (nth 0 (def-orchidee-options)))
                                                       (om-set-dialog-item-text outtxt (format nil "~D" (nth 1 (def-orchidee-options))))
                                                       (om-set-dialog-item-text intxt (format nil "~D" (nth 2 (def-orchidee-options))))
                                                       (om-set-dialog-item-text dbpath (format nil "~D" (if (nth 3 (def-orchidee-options))
                                                                                                            (namestring (nth 3 (def-orchidee-options)))
                                                                                                          "unspecified")))
                                                       (om-set-fg-color dbpath (if (and (nth 3 (def-orchidee-options)) (probe-file (nth 3 (def-orchidee-options))))
                                                                                   *om-black-color* *om-red-color*))
                                                       (setf *tmp-db-path* nil)
                                                       ))
      
      (om-make-dialog-item 'om-button (om-make-point 180 (- pos 30)) (om-make-point 100 20) "Cancel"
                           :di-action (om-dialog-item-act item
                                        (setf *tmp-db-path* nil)
                                        (om-return-from-modal-dialog dialog nil)))
      
      (om-make-dialog-item 'om-button (om-make-point 180 pos) (om-make-point 100 20) "OK"
                           :di-action (om-dialog-item-act item
                                        (let ((argerror nil)
                                              (host (om-dialog-item-text hosttxt))
                                              (out (read-from-string (om-dialog-item-text outtxt)))
                                              (in (read-from-string (om-dialog-item-text intxt)))
                                              (db (om-dialog-item-text dbpath))
                                              (pmsg (om-checked-p printmsg)))
                                          
                                          (setf rep-list (list host out in db pmsg))
                                          (if argerror
                                              (om-message-dialog (format nil "Error in an Orchidee option.~% Preferences could not be recorded.")) 
                                            (om-return-from-modal-dialog dialog rep-list))
                                          ))
                           :default-button t :focus t)
                     
      )
    (om-modal-dialog dialog)))


;(show-external-prefs-dialog 'orchidee (def-orchidee-options))
