;;; 
;;; OpenMusic Orchestration Tool
;;; 
;;; Orchestra object/editor
;;;
;;; J. Bresson

(in-package :om)


(defparameter *instruments-table* '(
                                    ("ClBb" "Bb Clarinet") 
                                    ("ClEb" "Eb Clarinet") 
                                    ("BClBb" "Bass Bb Clarinet") 
                                    ("CbClBb" "Contrabass Bb Clarinet") 
                                    ("Bn" "Bassoon") 
                                    ("Fl" "Flute")
                                    ("BFl" "Bass Flute")
                                    ("CbFl" "Contrabass Flute")
                                    ("Picc" "Piccolo")
                                    ("Ob" "Oboe")
                                    ("ASax" "Alto Sax")
                                    ("TpC" "C Trumpet") 
                                    ("Hn" "Horn")
                                    ("EH" "English Horn")
                                    ("Tbn" "Trombone")
                                    ("TTbn" "Tenor Trombone")
                                    ("BTbn" "Bass Trombone")
                                    ("BTb" "Bass Tuba")
                                    ("CbTb" "Contrabass Tuba")
                                    
                                    ("Va" "Viola")
                                    ("Vas" "Violas")
                                    ("Vc" "Violoncello")
                                    ("Vcs" "Violoncellos")
                                    ("Cb" "Contrabass")
                                    ("Cbs" "Contrabasses")
                                    ("Vn" "Violin")
                                    ("Vns" "Violins")
                                    
                                    ("P" "Piano")
                                    ("Ma" "Marimba")
                                    ("Mc" "Maracas")
                                    
                                    ("-" "-")))

(defun get-instr-name (str)
  (or (cadr (find str *instruments-table* :key 'car :test 'string-equal))
      str))
(defun get-instr-symb (str)
  (or (car (find str *instruments-table* :key 'cadr :test 'string-equal))
      str))

(defparameter *om-instruments* '("Vn" "Va" "Vc" "Cb" "ClBb" "Bn" "TpC" "Tbn" "Fl" "Hn" "Ob" "Ma" "Mc"))

(defparameter *emptyinstr* "---")

(defparameter *nb-instruments* '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10"))

(defvar *orchidee-instruments* nil)

; (orchidee-msg 5 "/dbfields" "/dbgetfields"  321 "message")
; (orchidee-msg 5 "/dbqueryfields" "/dbgetqueryfields"  321 "message")

(defun set-orchestra ()
  (let ;((instruments (orchidee-msg 5 "/dbinstlist" "/getdbinstlist" 990)))
      ((instruments (orchidee-msg 5 "/dbfieldvaluelist" "/dbgetfieldvaluelist" 321 "message" "instrument")))
    (setf *orchidee-instruments* (nthcdr 3 instruments))))

; (set-orchestra)

(defclass! orchestra () 
  ((instr-list :accessor instr-list :initform (or *orchidee-instruments* *om-instruments*))
   (orc-contents :accessor orc-contents :initarg :orc-contents :initform nil)
   (microtones :accessor microtones :initarg :microtones :initform 2))
  (:icon 806))

(defmethod initialize-instance :after ((self orchestra) &rest args)
  (unless (member (microtones self) '(2 4 8 16))
    (om-beep-msg "Warning: orchestra microtone resolution must be either 2, 4, 8 or 16.")
    (setf (microtones self) 2)))

(defun format-character (inst-list)
  (reduce #'(lambda (a b) (string+ a "/" b)) inst-list))

(defmethod format-orchestra ((self orchestra))
  (let ((replist nil)
        (i 0))
    (loop for elt in (orc-contents self) do
          (when (and (cadr elt) (numberp (cadr elt)) (plusp (cadr elt))   ;;; n = number > 0
                     (remove nil (car elt)))   ;;; instruments != null
            (setf replist (append replist 
                                  (loop for n from 1 to (cadr elt) 
                                        do (setf i (+ i 1))
                                        collect (format-character (remove nil (remove-if #'(lambda (item) (string-equal item "")) 
                                                                                         (remove-duplicates (car elt) :test 'string-equal)))))
                                  ))))
    replist))



(defmethod spec-obj-icon-size ((self orchestra)) '(70 55))
(defmethod spec-obj-icon-size ((self soundtarget)) '(32 32))

(defmethod Class-has-editor-p ((self orchestra)) t)

(defmethod get-fonde-pict ((self orchestra)) (or *orcobject-pict* *boxedit-pict*))

(defmethod get-initval ((self orchestra)) (make-instance 'orchestra))

(defmethod get-boxsize ((self orchestra)) (om-make-point 60 60))


(defmethod print-object ((self orchestra) x) 
  (call-next-method)
  (format x "~%")
  (loop for in in (orc-contents self) do
        (format x "    ~s x ~D ~%" (cadr in) (car in)))
  self)

(defclass orchestraEditor (EditorView) 
              ((items :accessor items :initform nil)))

(defmethod get-win-ed-pos ((self orchestra)) (om-make-point 200 200))
(defmethod get-win-ed-size ((self orchestra)) (om-make-point 380 140))

(defmethod get-editor-class ((self orchestra)) 'orchestraEditor)

(defclass OrchestraPanel (om-scroller) ())

(defmethod metaobj-scrollbars-params ((self orchestraEditor))  '(:v t))


(defmethod get-panel-class ((self orchestraEditor)) 'OrchestraPanel)
(defmethod editor ((self OrchestraPanel)) (om-view-container self))

(defmethod update-subviews ((Self orchestraeditor))
   (om-set-view-size (panel self) (om-make-point (w self) (- (h self) 70))))

(defparameter *wind-head-h* 100)

(defmethod make-editor-window ((class (eql 'orchestraEditor)) object name ref &key 
                                 winsize winpos (close-p t) (winshow t) 
                                 (resize nil) (maximize nil))
   (let ((win (call-next-method class object name ref :winsize winsize :winpos winpos :resize nil 
                                                      :close-p t :winshow t
                                                      )))
     (om-set-view-size win (om-make-point 380 (min 800 (+ (get-new-pos (editor win)) *wind-head-h* 10))))
     win))

(defvar *player-pict* nil)
(defvar *inst-pict* nil)

(setf *player-pict* (om-load-and-store-picture 
                        "player" 'full 
                        (make-pathname :directory (append (pathname-directory *soinitfile*)
                                                          (list "resources" "pict")))))
(setf *inst-pict* (om-load-and-store-picture 
                        "instruments" 'full 
                        (make-pathname :directory (append (pathname-directory *soinitfile*)
                                                          (list "resources" "pict")))))



(defmethod initialize-instance :after ((self orchestraEditor) &rest l)
  (declare (ignore l))
  (let* ((ed-view (om-make-view (get-panel-class self) 
                                :owner self
                                :scrollbars (first (metaobj-scrollbars-params self))
                                :retain-scrollbars (second (metaobj-scrollbars-params self))
                                :field-size  (om-make-point 330 (+ 20 (* (length (orc-contents (object self))) 30)))
                                :position (om-make-point 0 *wind-head-h*) 
                                :size (om-make-point (- (w self) 15) (- (h self) *wind-head-h*))
                                :bg-color *om-light-gray-color*)))
    
    
    (setf (panel self) ed-view)
    
    (om-set-bg-color self *om-light-gray-color*)
    
    (om-add-subviews self
                     (om-make-dialog-item 'om-static-text 
                                          (om-make-point 20 2)
                                          (om-make-point 160 20)
                                          "Orchestra Editor"
                                          :font *om-default-font2b*
                                          )
                     (om-make-view 'om-icon-button 
                                          :position (om-make-point 60 25)
                                          :size (om-make-point 20 20)
                                          :icon1 "+" :icon2 "+-pushed"
                                          ;:font (om-make-font "Verdana" 14 :style '(:bold))
                                          :action (om-dialog-item-act item
                                                       (add-item self '(nil 1))
                                                       )
                                          )
                     (om-make-view 'picture-view 
                                   :position (om-make-point 20 25)
                                   :size (om-make-point 30 40)
                                   :pict *player-pict* 
                                   )

                     (om-make-view 'picture-view 
                                   :position (om-make-point 250 5)
                                   :size (om-make-point 100 35)
                                   :pict *inst-pict* 
                                   )
                     (om-make-dialog-item 'om-static-text (om-make-point 60 52) (om-make-point 80 60)
                                          "Add character" :fg-color *om-gray-color*)
                     (om-make-dialog-item 'om-static-text (om-make-point 160 40) (om-make-point 90 60)
                                          "Get Orchidee instruments" :fg-color *om-gray-color*)

                     (om-make-dialog-item 'om-button (om-make-point 244 40)
                                          (om-make-point 115 20) "Load from DB"
                                          :di-action (om-dialog-item-act item 
                                                       (set-orchestra)
                                                       (if *orchidee-instruments*
                                                           (progn
                                                             (setf (instr-list (object self)) *orchidee-instruments*)
                                                             (put-items self))
                                                         (om-message-dialog "Instrument list could not be loaded. Please check that Orchidee is running and ready."))))
                     
                     (om-make-dialog-item 'om-static-text (om-make-point 160 70) (om-make-point 80 60)
                                          "Microtone resolution" :fg-color *om-gray-color*)
                     
                     (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 244 74)
                                          (om-make-point 70 20) ""
                                          :range '("1/2" "1/4" "1/8" "1/16")
                                          :value (string+ "1/" (integer-to-string (microtones (object self))))
                                          :di-action (om-dialog-item-act item
                                                       (setf (microtones (object self)) (nth (om-get-selected-item-index item) '(2 4 8 16)))))
                     
                     
                     )
    (put-items self)
    ))

(defmethod om-draw-contents ((self orchestraEditor))
  (om-with-focused-view self
    (om-with-fg-color self *om-gray-color* 
      (om-draw-line 232 55 250 55)
      (om-draw-line 69 30 69 52)
      )))



;;; ONE ITEM IN ORC LIST
  
(defclass instr-item (om-view) 
              ((ins-names :accessor ins-names :initarg :ins-names :initform '("---"))
               (nb :accessor nb :initarg :nb :initform 0)
               (ilist :accessor ilist :initarg :ilist :initform nil)))
               
(defun make-instr-item (name nb posy &optional (ilist *om-instruments*))
  (let ((item-h (* 26 (+ 1 (max 1 (length name))))))
    (om-make-view 'instr-item :position (om-make-point 20 posy) :size (om-make-point 340 item-h)
                  :ins-names name :nb nb :bg-color *om-light-gray-color*
                  :ilist ilist)))

(defparameter *orc-red-color* (om-make-color 0.58 0.51 0.49))

(defmethod initialize-instance :after ((self instr-item) &rest args)
  (let ((item-h (h self)))
    (unless (ins-names self) (setf (ins-names self) (list nil)))
    (om-add-subviews self 
                     (om-make-view 'om-view :position (om-make-point 0 0) :size (om-make-point 30 item-h) :bg-color *orc-red-color*)
                     (om-make-dialog-item 'om-pop-up-dialog-item 
                                          (om-make-point 36 3) 
                                          (om-make-point 160 26)
                                          ""
                                          :di-action (om-dialog-item-act item
                                                       (if (ins-names self)
                                                           (setf (car (ins-names self)) (get-instr-symb (om-get-selected-item item)))
                                                         (setf (ins-names self) (list (get-instr-symb (om-get-selected-item item)))))
                                                       (update-orchestre-editor (editor (om-view-container (om-view-container item)))))
                                          :font *controls-font* 
                                          :range (mapcar 'get-instr-name (append (list "-") (ilist self)))
                                          :value (get-instr-name (if (ins-names self) (car (ins-names self)) "-"))
                                          )
                   (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 200 3) 
                                       (om-make-point 60 26)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                    (setf (nb self) (om-get-selected-item-index item))
                                                    (update-orchestre-editor (editor (om-view-container (om-view-container item)))))
                                       :font *controls-font* 
                                       :range *nb-instruments*
                                       :value (integer-to-string (nb self))
                                       )
                   (om-make-view 'om-icon-button 
                                          :position (om-make-point 270 5)
                                          :size (om-make-point 20 20)
                                          :icon1 "-" :icon2 "--pushed"
                                          ;:font (om-make-font "Verdana" 14 :style '(:bold))
                                          :action (om-dialog-item-act item
                                                       (remove-item (editor (om-view-container (om-view-container self))) self))
                                          )
                   (om-make-view 'om-icon-button 
                                          :position (om-make-point 40 (- item-h 20))
                                          :size (om-make-point 16 16)
                                          :icon1 "+" :icon2 "+-pushed"
                                          ;:font (om-make-font "Verdana" 14 :style '(:bold))
                                          :action (om-dialog-item-act item
                                                    (let ((ed (editor (om-view-container (om-view-container item)))))
                                                      (setf (ins-names self) (append (list! (ins-names self)) (list nil)))
                                                      (update-orchestre-editor ed)
                                                      (put-items ed)   
                                                      )))
                   (om-make-dialog-item 'om-static-text (om-make-point 60 (- item-h 20)) (om-make-point 260 16)
                                        "Additional instruments for this player..."
                                        :fg-color *om-gray-color*
                                        )
                   
                   )
    
    (loop for add-inst in (cdr (ins-names self))
          for i = 1 then (+ i 1) do
          (let ((ind i))
            (om-add-subviews self 
                           (om-make-dialog-item 'om-pop-up-dialog-item 
                                          (om-make-point 36 (+ 4 (* i 26)))
                                          (om-make-point 160 26)
                                          ""
                                          :di-action (om-dialog-item-act item
                                                       (setf (nth ind (ins-names self)) (get-instr-symb (om-get-selected-item item)))
                                                       (update-orchestre-editor (editor (om-view-container (om-view-container item)))))
                                          :font *controls-font* 
                                          :range (mapcar 'get-instr-name (append (list "---") (ilist self)))
                                          :value (get-instr-name (nth i (ins-names self)))
                                          )
                           (om-make-view 'om-icon-button 
                                         :position (om-make-point 200 (+ 8 (* i 26)))
                                         :size (om-make-point 16 16)
                                         :icon1 "-" :icon2 "--pushed"
                                         :action #'(lambda (item)
                                                     (setf (ins-names self) (append (subseq (ins-names self) 0 ind)
                                                                                    (subseq (ins-names self) (+ ind 1))))
                                                     (update-orchestre-editor (editor (om-view-container (om-view-container item))))
                                                     (put-items (editor (om-view-container (om-view-container item)))))
                                         ))
            
            ))
    ))


(defmethod om-draw-contents ((self instr-item))
  (om-with-fg-color self *orc-red-color*
    (om-draw-view-outline self)))
  

(defmethod put-items ((self orchestraeditor))
  ;;; cleanup
  (loop for di in (items self) do (om-remove-subviews (panel self) di))
  (setf (items self) nil)
  ;;; set new items
  (loop for in in (orc-contents (object self))
        for i = 0 then (+ i 1) do
        (let ((item (make-instr-item (car in) (cadr in) (get-new-pos self) (instr-list (object self)))))
          (om-add-subviews (panel self) item)
          (setf (items self) (append (items self) (list item)))))
  (when (> (om-point-v (om-field-size (panel self))) (get-new-pos self))
    (om-set-field-size (panel self) (om-make-point 330 (+ 0 (get-new-pos self))))
    (om-set-view-size (om-view-window self) (om-make-point 380 (get-orcwin-size self))))
  (when (< (om-point-v (om-field-size (panel self))) (get-new-pos self))
    (om-set-view-size (om-view-window self) (om-make-point 380 (get-orcwin-size self)))
    (om-set-field-size (panel self) (om-make-point 330 (+ 0 (get-new-pos self))))))

(defun get-new-pos (editor)
  (+ 10 (loop for it in (items editor) sum (+ 8 (* 26 (+ 1 (max 1 (length (ins-names it)))))))))

(defun get-orcwin-size (editor)
  (min 800 (+ (get-new-pos editor) (+ *wind-head-h* 10))))

(defmethod add-item ((self orchestraeditor) instr)
  (let ((item (make-instr-item (car instr) (cadr instr) (get-new-pos self) (instr-list (object self)))))
    (om-add-subviews (panel self) item)
    (setf (items self) (append (items self) (list item)))
    (update-orchestre-editor self)
    
    (om-set-view-size (om-view-window self) (om-make-point 380 (get-orcwin-size self)))
    (om-set-field-size (panel self) (om-make-point 330 (get-new-pos self)))
    ))

(defmethod remove-item ((self orchestraeditor) instr)
  (setf (items self) (remove instr (items self)))
  (om-remove-subviews (panel self) instr)
  (update-orchestre-editor self)
  (put-items self))


;;; updates object according to the editor contents
(defmethod update-orchestre-editor ((self orchestraeditor))
  (setf (orc-contents (object self))
        (remove nil (loop for di in (items self) collect (list (remove-if #'(lambda (str) (member str '("-" "---" "") :test 'string-equal))
                                                                                 (ins-names di)) (nb di)))))
  (report-modifications self))


(defmethod update-editor-after-eval ((self orchestraeditor) val)
  (setf (object self) val)
  (loop for di in (items self) do (om-remove-subviews (panel self) di))
  (setf (items self) nil)
  (put-items self)
  (om-set-field-size (panel self) (om-make-point 330 (get-new-pos self))))

(defmethod close-editor-before ((self orchestraeditor))
  (call-next-method)
  (update-orchestre-editor self))


