


(defun change-text (win text)
  (setf (capi::title-pane-text (car (capi::layout-description (capi::pane-layout win)))) text))


(let* ((win1 (make-instance 'capi::interface 
                           :layout (make-instance 'capi::pinboard-layout
                                                  :description (list 
                                                                (make-instance 'capi::title-pane :text "init")))))
      (win2 (make-instance 'capi::interface 
                           :layout (make-instance 'capi::pinboard-layout
                                                  :description (list 
                                                                (make-instance 'capi::button :text "go"
                                                                               :callback-type :none
                                                                               :callback #'(lambda ()
                                                                                             (change-text win1 "go")
                                                                                             (loop for i from 0 to 20 do
                                                                                                   (change-text win1 (format nil "~D" i))
                                                                                                   (sleep 0.1)))
                                                                               ))))))
  (capi::display win1)
  (capi::display win2))



  
