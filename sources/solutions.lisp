(in-package :om)


(defclass! orc-solution ()
           ((id :initform nil :initarg :id :accessor id)
            (name :initform "" :initarg :name :accessor name)
            (size :initform nil :initarg :size :accessor size)
            (features :initform nil :initarg :features :accessor features)
            (vals :initform nil :initarg :vals :accessor vals)
            (criteria :initform nil :initarg :criteria :accessor criteria)
            (composition :initform nil :initarg :composition :accessor composition)
            (solution-mixer :initform nil :accessor solution-mixer))
           (:icon 801))

(defclass! sol-component ()
  ((name :initform nil :initarg :name :accessor name)
   (instrument :initform nil :initarg :instrument :accessor instrument)
   (note :initform nil :initarg :note :accessor note)
   (comp-pitch :initform nil :initarg :comp-pitch :accessor comp-pitch)
   (mode :initform nil :initarg :mode :accessor mode)
   (dyn :initform nil :initarg :dyn :accessor dyn)
   (mute :initform nil :initarg :mute :accessor mute)
   (string-n :initform nil :initarg :string-n :accessor string-n)
   (sfname :initform nil :initarg :sfname :accessor sfname)
   (path :initform nil :initarg :path :accessor path)
   (strsp :initform nil :initarg :strsp :accessor strsp)
   (sgain :initform nil :initarg :sgain :accessor sgain)
   
   (edits :initform '(1.0 0.5) :initarg :edits :accessor edits))
  (:icon 808))

(defmethod print-object ((self sol-component) stream)
  (format stream "comp:~a" (string-until-char (string (name self)) ".")))

(defclass! orc-solutionset () 
           ((solutions :initform nil :initarg :solutions :accessor solutions)
            (selection :initform nil :initarg :selection :accessor selection))
           (:icon 808))

(defmethod get-fonde-pict ((self orc-solution)) (or *orcobject-pict* *boxedit-pict*))
(defmethod get-fonde-pict ((self sol-component)) (or *orcobject-pict* *boxedit-pict*))
(defmethod get-fonde-pict ((self orc-solutionset)) (or *orcobject-pict* *boxedit-pict*))

(defmethod spec-obj-icon-size ((self orc-solution)) '(50 38))

(defmethod objfromobjs ((self pathname) (type orc-solutionset))
  (parse-solution-file self))

(defun restore-case (str)
  (or (car (find str *instruments-table* :test 'string-equal :key 'car))
      str))

(defun parse-solution-file (path)
  (let ((solutionset (make-instance 'orc-solutionset))
        (nbfeatures 0)
        (features nil) (nbsol 0) (size 0)
        (currline nil) (eof nil))
    (with-open-file (file path :direction :input)
      ;;; nb solutions
      (when (setf currline (om-read-line file))
        (setf nbsol (read-from-string currline nil nil)))
      ;;; orc size
      (when (setf currline (om-read-line file))
        (setf size (read-from-string currline nil nil)))
      ;; features
      (when (setf currline (om-read-line file))
        (setf nbfeatures (read-from-string currline nil nil)))
      (loop for featn from 1 to nbfeatures do 
            (when (setf currline (om-read-line file))
              (pushr currline features)))
      ;(list nbsol size features)
      (loop for i from 1 to nbsol 
            while (not eof) do
            ;;; ONE SOLUTION
            (let ((sol (make-instance 'orc-solution)))
              (setf (size sol) size)
              (setf (features sol) features)
              (setf (criteria sol) (make-list (length features)))
              (setf (vals sol) (make-list (length features)))
              (setf (composition sol) (make-list size))
              ;;; ID
              (when (setf currline (om-read-line file))
                (setf (id sol) (read-from-string currline nil nil))
                (setf (name sol) (format nil "solution~A" (id sol))))
              (setf eof nil)
              (unless eof
                ;;; COMPOSITION OF THE SOLUTION
                (loop for comp from 0 to (- size 1) do
                      (let ((sc (make-instance 'sol-component))
                            val n)
                        ;; INSTR NAME OR <empty>
                        (when (setf currline (om-read-line file))
                          (multiple-value-bind (val n) (read-from-string currline nil nil)
                            (setf currline (subseq currline n))
                            (setf (name sc) (restore-case (string val)))
                            (unless (equal val '<empty>)
                              (setf (instrument sc) (name sc))
                              ;; NOTE
                              (multiple-value-bind (val n) (read-from-string currline nil nil)
                                (setf currline (subseq currline n))
                                (setf (note sc) val)
                                (setf (name sc) (string+ (string (name sc)) " " (string val)))
                                (setf (comp-pitch sc) (orchidee->mc (string val))))
                              ;;; mode
                              (multiple-value-bind (val n) (read-from-string currline nil nil)
                                (setf currline (subseq currline n))
                                (setf (mode sc) val)
                                (unless (equal val 'ord)
                                  (setf (name sc) (string+ (name sc) " " (string val))))
                                )
                              ;; dyn
                              (multiple-value-bind (val n) (read-from-string currline nil nil)
                                  (setf currline (subseq currline n))
                                  (setf (dyn sc) val)
                                  (setf (name sc) (string+ (name sc) " " (string val))))
                              ;; mute
                              (multiple-value-bind (val n) (read-from-string currline nil nil)
                                (setf currline (subseq currline n))
                                (setf (mute sc) val)
                                (unless (or (equal val 'NA) (equal val 'n))
                                  (setf (name sc) (string+ (name sc) " " (string val)))))
                              ;; string
                              (multiple-value-bind (val n) (read-from-string currline nil nil)
                                  (setf currline (subseq currline n))
                                  (setf (string-n sc) val)
                                  ;(unless (= val 0)
                                  ;  (setf (name sc) (string+ (name sc) " " (integer-to-string val))))
                                  )
                              ;;; path
                              (multiple-value-bind (val n) (read-from-string currline nil nil)
                                (let ((sfpath (pathname (subseq currline 0 (1- n))))) ; (string val))))
                                  (setf (path sc) (pathname-directory sfpath))
                                  (setf (sfname sc) (string+ (pathname-name sfpath) "." (pathname-type sfpath))))
                                (setf currline (subseq currline n)))
                              ;; transpose
                              (multiple-value-bind (val n) (read-from-string currline nil nil)
                                (setf currline (subseq currline n))
                                (setf (strsp sc) val))
                              ;; gain
                              (multiple-value-bind (val n) (read-from-string currline nil nil)
                                (setf currline (subseq currline n))
                                (setf (sgain sc) (if (numberp val) val 1.0)))
                              
                              (setf (edits sc) (list 1.0 0.5))
                              
                              )))
                        
                        (setf (nth comp (composition sol)) sc)))
                
                ;;; FEATURE VALUES
                (loop for v from 0 to (- (length features) 1) do
                      (when (setf currline (om-read-line file))
                        (loop while currline do 
                              (multiple-value-bind (val n) (read-from-string currline nil nil)
                                (if val 
                                    (progn 
                                      (setf currline (subseq currline n))
                                      (pushr val (nth v (vals sol))))
                                  (setf currline nil))
                                ))
                        ))

                ;;; CRITTERIA VALUES
                (loop for c from 0 to (- (length features) 1) do
                      (when (setf currline (om-read-line file))
                        (setf (nth c (criteria sol)) (read-from-string currline nil nil))))
                
                ;;;(compute-def-edits sol)
                (pushr sol (solutions solutionset))
                )))
        solutionset)))

        
;;; PLAY
   
(defun restore-db-path (sol-component)
  (make-pathname :directory (let ((pos 1)) ;(position "SOUNDDB" path :test 'string-equal)))
                              (if (and pos *orchidee-db-path*)
                                  (append (pathname-directory *orchidee-db-path*) 
                                          (list "sounds") ; (dbname sol-component)) 
                                          (subseq (path sol-component) pos))
                                (path sol-component)))
                 :name (sfname sol-component))
  )


(defmethod play-obj? ((self orc-solution)) t)
(defmethod play-obj? ((self sol-component)) t)
(defmethod get-obj-dur ((self orc-solution)) 4000)
(defmethod get-obj-dur ((self sol-component)) 4000)

(defmethod get-snd-data ((self sol-component) &optional track)
  (when (path self)
    (let ((sndfile (restore-db-path self)))
      (if (probe-file sndfile)
        (sound-vol 
         (get-om-sound-data sndfile track)
         (sgain self))
        (om-beep-msg (string+ "File " (namestring sndfile) " not found"))
        ))))

(defmethod get-snd-data ((self orc-solution)  &optional track)
  (remove nil (loop for sol in (composition self)
                    for i = 0 then (+ i 1) 
                    collect (get-om-sound-data sol i))))

                                   

(defmethod get-snd-data ((self orc-solution)  &optional track)
  (let ((i 0))
    ;(reduce 'sound-mix 
             (remove nil (mapcar 
                         #'(lambda (sol) (when (path sol)
                                           (let ((sndfile (restore-db-path sol)))
                                             (if (probe-file (namestring sndfile))
                                                 (let ((sndptr (oa::om-read-sound (las::MakeReadSound (namestring sndfile)) 0.5 2.0)))
                                                   (if sndptr
                                                       (sound-effect (get-om-sound-data sndfile (incf i))
                                                                                    :ptr sndptr
                                                                                    :tracknum )
                                                             ;(list (vol-effect (car (edits sol))) (pan-effect (cadr (edits sol))))
                                                                     (list (vol-effect (sgain sol)))
                                                                     )
                                                     (om-beep-msg (string+ "Problem with file " (namestring sndfile)))
                                                     ))
                                               (om-beep-msg (string+ "File " (namestring sndfile) " not found"))))))
                         (composition self)))
    ;        )
    )


(defmethod Preparetoplay ((player t) (s sol-component) at &key approx port interval voice)
  (when (path s)
    (let ((ptr (get-snd-data s)))
      (om-set-track-volume *audio-player* 0 (car (edits s)))
      (om-set-track-pan *audio-player* 0 (cadr (edits s)))
      (Preparetoplay *audio-player* ptr at :approx approx :port port :interval interval :voice voice)
      )))

(defmethod Preparetoplay ((player t) (s orc-solution) at &key approx port interval voice)
    (let ((ptrlist (get-snd-data s)))
      (build-mixer-object s)
      (Preparetoplay *audio-player* (cons (solution-mixer s) ptrlist) at :approx approx :port port :interval interval :voice voice)
      ))



(defmethod objfromobjs ((self sol-component) (type sound))
  (objfromobjs (get-snd-data self) type))

(defmethod objfromobjs ((self orc-solution) (type sound))
  (let ((data (get-snd-data self)))
    (when data 
      (objfromobjs (reduce 'sound-mix data) type))))



;(defmethod compute-def-edits ((self orc-solution))
;  (let ((glist (mapcar 'sgain (composition self))))
;    (mapcar #'(lambda (sc gain)
;                (when gain 
;                  (setf (edits sc) (list gain 0.5))))
;            (composition self)
;            (om-scale glist 0.0 1.0 0.0 (list-max glist)))
;    ))
                


;;; SOLUTION EDITOR
(defclass SolutionEditor (audiocontrollerEditor) 
              ((items :accessor items :initform nil)))
(defclass SolutionPanel (audiocontrollerpanel) ())

(defmethod Class-has-editor-p ((self orc-solution)) t)
(defmethod get-win-ed-pos ((self orc-solution)) (om-make-point 400 200))
(defmethod get-win-ed-size ((self orc-solution)) (om-make-point (or (and (size self) (* 80 (size self))) 80) 260))
(defmethod get-editor-class ((self orc-solution)) 'SolutionEditor)
(defmethod metaobj-scrollbars-params ((self SolutionEditor))  '(nil nil))
(defmethod get-panel-class ((self SolutionEditor)) 'SolutionPanel)
(defmethod editor ((self SolutionPanel)) (om-view-container self))

(defmethod update-editor-after-eval ((self SolutionEditor) val)
  (build-mixer-object val)
  (call-next-method self (solution-mixer val)))

(defmethod report-modifications ((self SolutionEditor))
  (loop for solc in (composition (ref-solution (object self)))
        for ch in (channels-ctrl (object self)) do
        (setf (edits solc) (list (omvol2float (vol-ctrl ch)) (ompan2float (pan-ctrl ch))))))

(defclass solution-mix-console (audio-mix-console) 
  ((ref-solution :initarg :ref-solution :accessor ref-solution)))

(defmethod build-mixer-object ((self orc-solution))
  (let ((mixer (or (solution-mixer self) 
                   (setf (solution-mixer self) (make-instance 'solution-mix-console :nbtracks (size self)
                                                     :ref-solution self)))))
    (loop for solc in (composition self)
          for ch in (channels-ctrl mixer) do
          (setf (vol-ctrl ch) (if (edits solc) (float2omvol (car (edits solc))) 0)
                (pan-ctrl ch) (if (edits solc) (float2ompan (cadr (edits solc))) 0))
          )
    mixer))

(defun ompan2float (value) (float (/ (+ 0 100) 200)))
(defun float2ompan (value)  (round (* (- value 0.5) 200)))
(defun omvol2float (value) (float (/ value 100)))
(defun float2omvol (value) (round (* value 100)))

(defmethod om-draw-contents ((self solutioneditor))
  (call-next-method)
  (om-with-focused-view self
    (om-with-font *om-default-font1b*
                  (om-draw-string 20 (- (h self) 10)
                                   (if (and (name (ref-solution (object self)))
                                            (not (string-equal (name (ref-solution (object self))) "")))
                                       (name (ref-solution (object self)))
                                     "solution editor"))
                  )))
  
(defmethod make-editor-window ((class (eql 'SolutionEditor)) object name ref &key 
                                 winsize winpos (close-p t) (winshow t) 
                                 (resize nil) (maximize nil))
  (unless (solution-mixer object) (build-mixer-object object))
  (let* ((win (call-next-method class (solution-mixer object) name ref :winsize (get-win-ed-size object) :winpos winpos :resize nil 
                               :close-p t :winshow t
                               ))
         (ed (editor win)))
    
    (om-set-bg-color ed *orc-red-color*)

    (om-add-subviews ed
              ;       (om-make-dialog-item 'om-static-text
              ;                            (om-make-point 20 (- (h ed) 25)) (om-make-point 200 20)
              ;                            (if (and (name (ref-solution (object ed)))
              ;                                     (not (string-equal (name (ref-solution (object ed))) "")))
              ;                                (name (ref-solution (object ed)))
              ;                              "solution editor")
              ;                            :font *om-default-font1b*)
                     (om-make-view 'om-icon-button 
                                   :icon1 "play" :icon2 "play-pushed"
                                   :size (om-make-point 20 20)
                                   :position (om-make-point (- (w ed) 54) (- (h ed) 25))
                                   :action (lambda (arg)
                                             (when (ref-solution (object ed))
                                               ;(send-audio-settings (object ed))
                                               (setf (loop-play *general-player*) t)
                                               (loop-play-on-tracks (object ed))))
                                   )
                     
                     (om-make-view 'om-icon-button 
                                   :icon1 "stop" :icon2 "stop-pushed"
                                   :size (om-make-point 20 20)
                                   :position (om-make-point (- (w ed) 30) (- (h ed) 25))
                                   :action (lambda (arg) 
                                             (stop t)
                                             )
                                   ))
    win))



(defmethod loop-play-on-tracks ((self solution-mix-console))
  (play  
   (append ; (list self)
           (remove nil (loop for elt in (composition (ref-solution self))
                             for i = 1 then (+ i 1) collect
                             (when (path elt)
                               (let ((sndfile (restore-db-path elt)))
                                 (if (probe-file sndfile)
                                     (sound-fade (sound-vol 
                                                  (get-om-sound-data sndfile :tracknum i)
                                                  (sgain elt))
                                                 100 200)
                                   (om-beep-msg (string+ "File " (namestring sndfile) " not found"))
                                   )))))
           )
   ))
  

(defmethod update-subviews ((Self SolutionEditor))
  (om-set-view-size (panel self) (om-make-point (w self) (- (h self) 30))))


(defclass solutionchannelpanel (audiochannelPanelview) ())
(defmethod get-channelpanel-class ((self SolutionPanel)) 'solutionchannelpanel)
(defmethod editor ((self solutionchannelpanel)) (editor (om-view-container self)))


(defmethod make-track-title ((self solutionchannelpanel) &optional (ypos 0))
  (om-make-dialog-item 'om-static-text
                       (om-make-point 0 ypos) 
                       (om-make-point 80 40) 
                       (string-downcase (string-until-char (string 
                                                            (name (nth (1- (track (channelctr self))) (composition (ref-solution (object (editor self)))))))
                                                           "."))
                       :font *om-default-font1b*))

;;; SOLUTION SET EDITOR

(defclass SolutionSetEditor (editorview) 
              ((mainlist :accessor mainlist :initform nil)
               (selectionlist :accessor selectionlist :initform nil)
               (curname :accessor curname :initform nil)
               (cursoltxt :accessor cursoltxt :initform nil)
               (currentsolution :accessor currentsolution :initform nil)
               ))

;(defclass SolutionSetPanel (om-view) ())

(defmethod Class-has-editor-p ((self orc-solutionset)) t)
(defmethod get-win-ed-pos ((self orc-solutionset)) (om-make-point 600 400))
(defmethod get-win-ed-size ((self orc-solutionset)) (om-make-point 600 300))
(defmethod get-editor-class ((self orc-solutionset)) 'SolutionSetEditor)
(defmethod metaobj-scrollbars-params ((self SolutionSetEditor))  '(nil nil))
;(defmethod get-panel-class ((self SolutionSetEditor)) 'SolutionSetPanel)
;(defmethod editor ((self SolutionSetPanel)) (om-view-container self))

(defmethod update-editor-after-eval ((self SolutionSetEditor) val)
  (call-next-method)
  (update-editor-lists self))

(defmethod report-modifications ((self SolutionSetEditor))
  (call-next-method))


(defmethod make-editor-window ((class (eql 'solutionseteditor)) object name ref &key 
                                 winsize winpos (close-p t) (winshow t) 
                                 (resize nil) (maximize nil))
  (call-next-method class object name ref :winsize winsize :winpos winpos :resize nil 
                    :close-p t :winshow t
                    ))
    

(defmethod handle-key-event ((self solutionseteditor) char)
  (when (and (equal char #\Space) (currentsolution self))
    (if (Idle-p *general-player*)
        (play (currentsolution self))
      (stop t))))

(defmethod initialize-instance :after ((self solutionseteditor) &rest initargs) 
  (om-set-bg-color self (om-make-color 0.6 0.5 0.5))  
  (om-add-subviews self 
                   (om-make-dialog-item 'om-static-text (om-make-point 20 8)
                                        (om-make-point 150 20)
                                        "ALL SOLUTIONS"
                                        :fg-color *om-dark-gray-color*
                                        :font *targeteditor-font*)
                   (setf (mainlist self)
                              (om-make-dialog-item 'om-multi-item-list
                                                   (om-make-point 20 35)
                                                   (om-make-point 180 220)
                                                   ""
                                                   :range (mapcar 'name (solutions (object self)))
                                                   :scrollbars t
                                                   :di-action (om-dialog-item-act item
                                                                (let ((i (om-get-selected-item-index item)))
                                                                  (if (or (integerp i) (and (listp i) (= 1 (length i)) (setf i (car i))))
                                                                      (set-selection self (nth i (solutions (object self))))
                                                                    (set-selection self nil))
                                                                  ))
                                                   ))
                   (om-make-dialog-item 'om-static-text (om-make-point 20 260)
                                        (om-make-point 150 20)
                                        "Add to selection..."
                                        :fg-color *om-dark-gray-color*
                                        :font *targeteditor-font*)
                   (om-make-dialog-item 'om-button (om-make-point 150 255) (om-make-point 50 20) "-->"
                                        :di-action (om-dialog-item-act item
                                                     (loop for ind in (om-get-selected-item-index (mainlist self)) do
                                                           (unless (member (nth ind (solutions (object self))) (selection (object self)))
                                                             (setf (selection (object self)) (append (selection (object self)) 
                                                                                                     (list (nth ind (solutions (object self))))))))
                                                     (update-editor-lists self)
                                                     (report-modifications self)))
                   (om-make-dialog-item 'om-static-text (om-make-point 400 8)
                                        (om-make-point 150 20)
                                        "USER SELECTION"
                                        :fg-color *om-dark-gray-color*
                                        :font *targeteditor-font*)
                   (setf (selectionlist self)
                              (om-make-dialog-item 'om-multi-item-list
                                                   (om-make-point 400 35)
                                                   (om-make-point 180 220)
                                                   ""
                                                   :range (mapcar 'name (selection (object self)))
                                                   :scrollbars t
                                                   :di-action (om-dialog-item-act item
                                                                (let ((i (om-get-selected-item-index item)))
                                                                  (if (or (integerp i) (and (listp i) (= 1 (length i)) (setf i (car i))))
                                                                      (set-selection self (nth i (selection (object self))))
                                                                    (set-selection self nil))
                                                                  ))))
                   (om-make-dialog-item 'om-static-text (om-make-point 400 260)
                                        (om-make-point 150 20)
                                        "Delete selection..."
                                        :fg-color *om-dark-gray-color*
                                        :font *targeteditor-font*)
                   (om-make-dialog-item 'om-button (om-make-point 535 255) (om-make-point 50 20) "x"
                                        :di-action (om-dialog-item-act item
                                                     (setf (selection (object self)) 
                                                           (remove nil (subs-posn (selection (object self))
                                                                                  (om-get-selected-item-index (selectionlist self))
                                                                                  nil)))
                                                     (update-editor-lists self)
                                                     (report-modifications self)))
                   (setf (curname self) (om-make-dialog-item 'om-editable-text (om-make-point 225 34)
                                                            (om-make-point 150 20)
                                                            ""
                                                            :enable nil
                                                            :font *om-default-font1*
                                                            :modify-action (om-dialog-item-act item
                                                                             (when (currentsolution self)
                                                                               (setf (name (currentsolution self)) 
                                                                                     (om-dialog-item-text item))
                                                                               (update-editor-lists self)
                                                                               ))))
                   (setf (cursoltxt self) (om-make-dialog-item 'om-static-text (om-make-point 224 70)
                                                            (om-make-point 160 185)
                                                            "..."
                                                            :fg-color *om-light-gray-color*
                                                            :font *om-default-font1*
                                                            ))
                   (om-make-view 'om-icon-button 
                                 :icon1 "play" :icon2 "play-pushed"
                                 :size (om-make-point 20 20)
                                 :position (om-make-point 280 (- (h self) 40))
                                 :action (lambda (arg) 
                                           (Stop-Player *general-player*)
                                           (when (currentsolution self)
                                             (playany t (currentsolution self))))
                                 )
                   
                   (om-make-view 'om-icon-button 
                                 :icon1 "stop" :icon2 "stop-pushed"
                                 :size (om-make-point 20 20)
                                 :position (om-make-point 304 (- (h self) 40))
                                 :action (lambda (arg) 
                                           (Stop-Player *general-player*))
                                 )
                   ))
                                                   
(defmethod update-editor-lists ((self solutionseteditor))
  (om-set-item-list (mainlist self) (mapcar 'name (solutions (object self))))
  (om-set-item-list (selectionlist self) (mapcar 'name (selection (object self)))))
  
(defmethod set-selection ((self solutionseteditor) sol)
  (setf (currentsolution self) sol)
  (if sol
      (progn 
        (om-set-dialog-item-text (cursoltxt self) (format-solution sol))
        (om-set-dialog-item-text (curname self) (name sol))
        (om-enable-dialog-item (curname self) t)
        (om-set-fg-color (cursoltxt self) *om-black-color*)
        )
    (progn 
      
      (om-set-dialog-item-text (cursoltxt self) "...")
      (om-enable-dialog-item (curname self) nil)
      (om-set-dialog-item-text (curname self) "")
      (om-set-fg-color (cursoltxt self) *om-light-gray-color*)
      )
    ))
      
(defun format-solution (sol)
  (let ((str ""))
    (loop for item in (composition sol) do (setf str (string+ str (format nil "~A~%" (name item)))))
    str))

;;; POLY CONVERSION

(defun vel-conversion (symb)
  (case symb
    ('fff (dyn-fff))
    ('ff (dyn-ff))
    ('f (dyn-f))
    ('mf (dyn-mf)) 
    ('mp (dyn-mp)) 
    ('ppp (dyn-ppp))
    ('pp (dyn-pp)) 
    ('p (dyn-p))
    (otherwise nil)))

;(vel-conversion 'ff)

(defmethod objfromobjs ((self orc-solution) (type poly))
  (let* ((channels nil)
        (p (make-instance 'poly
                 :voices (loop for item in (composition self) collect
                               
                               (let* ((c (make-instance 'chord :lmidic (list (if (string-equal (name item) "<empty>") 
                                                                                 6000
                                                                               (comp-pitch item)))
                                                        :lvel (if (string-equal (name item) "<empty>") '(0)
                                                                (list (get-vel-from-dyn (dyn item))))
                                                        :lchan (if (string-equal (name item) "<empty>") '(10)
                                                                 (list 
                                                                  (let ((found (find (string (instrument item)) channels 
                                                                                            :test 'string-equal :key 'car)))
                                                                    (if found (cadr found)
                                                                      (progn
                                                                        (pushr (list (string (instrument item))
                                                                                     (+ (length channels) 1))
                                                                               channels)
                                                                        (length channels))))))
                                                               
                                                        ))
                                      (onevoice nil))
                                 (add-extra c
                                            (make-instance 'vel-extra :thechar (vel-conversion (dyn item))
                                                           :deltax 0 :deltay 0.9)
                                            nil)
                                 (setf onevoice (make-instance 'voice :tree (if (string-equal (name item) "<empty>") 
                                                                         '(1 (((1 4) (-1))))
                                                                       '(1 (((1 4) (1)))))
                                                        :chords (list c)
                                                        :tempo 120))
                                 (if (string-equal (name item) "<empty>")
                                     (setf (name onevoice) "")
                                   (setf (name onevoice) (string (instrument item))))
                                 onevoice
                                 )))))
  (set-midi-table channels)
  p))

;;; REDEF
(defmethod draw-object ((self grap-poly) view x y zoom minx maxx miny maxy slot size linear? staff grille-p chnote)
   (let* ( (posy y) 
            positions
            (meas-list (cdr (get-aligne-measures (reference self))))
            )
      (loop for item in (inside self)
            for i = 0 then (+ i 1) 
            for system in staff do
            (push posy positions)
            
            (draw-object item view x (- posy (round (* (posy (car (staff-list system))) (/ size 4))))
                         zoom minx maxx miny maxy slot size linear? (nth i staff) grille-p chnote)
            (when (and (name (reference item)) (stringp (name (reference item))))
              (om-with-font (get-font-to-draw 6)
                            (om-draw-string x (+ posy (round (* (length (staff-list system)) size 1.9))) 
                                            (name (reference item)))))
            (setf posy (+ posy (get-delta-system system size view i)))
            )
      (setf positions (reverse positions))
      (collect-rectangles self)
      (last-aling-measures self  minx maxx zoom)
      (draw-aligned-measures self meas-list staff size  positions)
      (draw-extras self view size staff)
      ))

(defmethod initialize-instance :after ((self multiseqPanel) &rest initargs)
  (declare (ignore initargs))
  (loop for object in (inside (object (om-view-container self)))
        for i = 1 then (+ i 1)
        do (unless (name object) (setf (name object) ""))))

;;; a mettre dans scoretools
(defun get-vel-from-dyn (dyn)
  (let ((pos (position dyn '(p pp ppp mp mf f ff fff))))
    (if pos (nth (+ pos 1) *cur-dynamic-list*)
      100)))


(defmethod objfromobjs ((self orc-solutionset) (type poly))
  (reduce 'concat 
          (mapcar #'(lambda (sol) (objfromobjs sol (make-instance 'poly))) 
                  (solutions self))))



;;; MIDI SETUP 

(defvar *instr-2-gm-table* nil)
(setf *instr-2-gm-table* '(("BbCl" "71 - Clarinet") 
                           ("BClBb" "71 - Clarinet") 
                           ("CbClBb" "71 - Clarinet") 
                           ("Bn" "70 - Bassoon") 
                           ("Fl" "73 - Flute")
                           ("BFl" "73 - Flute")
                           ("CbFl" "73 - Flute")
                           ("Picc" "72 - Piccolo")
                           ("Ob" "68 - Oboe")
                           ("ASax" "65 - Alto Sax")
                           ("CTp" "56 - Trumpet") 
                           ("Hn" "60 - French Horn")
                           ("EH" "69 - English Horn")
                           ("Tbn" "57 - Trombone")
                           ("TTbn" "57 - Trombone")
                           ("BTbn" "57 - Trombone")
                           ("BTb" "58 - Tuba")
                           ("CbTb" "58 - Tuba")
                           
                           ("Va" "41 - Viola")
                           ("Vc" "42 - Cello")
                           ("Cb" "43 - Contrabass")
                           ("Vn" "40 - Violin")
                           
                           ("P" "0 - Acoustic Grand Piano")
                           ("Ma" "12 - Marimba")
                           ("Mc" "Percu")
                           
                           ))

;; GM closest program...
(defun find-midi-program (instr)
  (let ((name (cadr (find instr *instr-2-gm-table*  :test 'string-equal :key 'car))))
    (or (cadr (find name *midi-programs* :test 'string-equal :key 'car)) 0)))
   
;; SET MIDI CHANNELS ACCORDING TO THE INSTRUMENTS
(defun set-midi-table (chan-list)
  (loop for ins in chan-list do
        (pgmout (find-midi-program (car ins)) (cadr ins))))


; (find-midi-program "Vc")