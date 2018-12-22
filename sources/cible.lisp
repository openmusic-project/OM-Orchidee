;;; 
;;; OpenMusic Orchestration Tool
;;; 
;;; Target object/editor
;;;
;;; J. Bresson

(in-package :om)


(defclass! soundtarget () 
  ((chord :accessor chord :initarg :chord :initform (make-instance 'chord) :documentation "initial chord (symbolic pitches)")
   (params :accessor params :initarg :params :initform nil :documentation "attributes of the pitch spectral processing operators")
   (operateurs :accessor operateurs :initarg :operateurs :initform nil :documentation "additional pitch processing operators")
   (freq-filter :accessor freq-filter :initarg :freq-filter :initform (simple-bpf-from-list '(0 1 1000 2000 3000 4000 5000) 
                                                                             '(-60 0 0 0 0 0 0) 'bpf 1)
                :documentation "a filter for the resulting spectrum")
   (amp-profile :accessor amp-profile :initform (simple-bpf-from-list '(0 200 800 1000) 
                                                                      '(0 100 100 0) 'bpf 0)
                :documentation "dynamic profile for the sound target")
   (spectra :accessor spectra :initform (make-instance 'vps-list))
   (synth-class :accessor synth-class :initarg :synth-class :initform 'add-1 :documentation "the OMChroma class used for synthesis")
   (snd :accessor snd :initarg :snd :initform nil :documentation "the sound used as a target for orchestration")
   (analysisparams :accessor analysisparams :initform (list 80 25 "ChordSeq+ERB"))

   ;;; TARGETCHORD = the search domain chord
   (targetchord :accessor targetchord :initform (make-instance 'chord))
   ;;; CHORDMODE : how to build targetchord
   ;;; 0 => targetchord = initchord
   ;;; 1 => targetchord = manual
   ;;; 2 => no targetchord
   (chordmode :accessor chordmode :initform 0)
   ;;; CHORDINFO : how to consider targetchord in search domain
   ;;; 0 => ignore
   ;;; 1 => include in serach domain
   ;;; 2 => force search domain
   (chordinfo :accessor chordinfo :initform 2)
   
   )
  (:icon 807)
  (:documentation "
SOUNDTARGET represents the data allowing to Orchidée to initiate an orchestration process.

Most of this data is accessible from the editor, and some part of it can be set algorithmically from OM programs via the visible inputs of the class.

The higher-level slot is <chord>, which allows to specify a set of pitches as the basis of the target sound. <chord> can be specified in the OM patch or in the SoundTarget editor.

Each note of the CHORD can be selected individually and transformed in the spectral domain in the 'Selected Note' frame of the editor.
Harmonics or user-defined (cmd+click) partials can be added, and amplitudes can be edited in the left-hand part of the note spectrum view.

Such additions can also be performed beforehand in the OM patch using the <params> slot. 
Connect a list of the form:
 '((:nb-harm 10) (:amps (1 0.5)) (:add-partiels ((400 1.0) (500 0.2))) (:nth-remove (4))) 
in order, for instance, to add automatically 10 harmonics, set the two first amplitudes to 1 and 0.5 respectively, remove the fourth partial and add two partials at 400 and 500Hz with amplitudes 1.0 and 0.2.

Additional operators can also be connected to <operateurs> and controlled the same way (in the <params> list, using the operator's name as keyword, or in the 'Selected Note' frame of the editor). 
These operators must be functions transforming structures of type VPS (see OMChroma) given as first argument.

The FULL-SPECTRUM frame of the editor is only for display and can not be edited. The parameters of its conversion to a sound target are accessible in the SOUND frame. They principally consist of a filter, a dynamic envelope and a synthesis process, which can be set in the editor or in the OM patch using the <freq-filter>, <amp-profile> and <synth-class> slots.

Finally, the TARGET frame contains the actual data sent to Orchidée: the target sound, some analysis parameters and a pitch search domain determined (optionally) depending on the initial symbolic pitches.

The SOUNDTARGET window can actually be displayed in a 'reduced' mode and show only this part of the editor. 
It is also possible to ignore the full target building steps and specify it directly as a sound file via the <snd> slot.


See the OM-Orchidée tutorial patches for additional info and use of the SOUNDTARGET object. 

"))


(defmethod make-one-instance ((self soundtarget) &rest slots)
  (let ((rep (make-instance 'soundtarget
                            :chord (change-class (clone (nth 0 slots)) 'chord-spectra)
                            :params (nth 1 slots)
                            :operateurs (nth 2 slots)
                            :freq-filter (clone (nth 3 slots))
                            :synth-class (nth 4 slots)
                            :snd (clone (nth 5 slots)))))
    
    ;(setf (spectra rep) (make-new-spectra (chord rep)))

    rep))

(defmethod initialize-instance :after ((self soundtarget) &rest args)
  
  (when (not (params self))
    (setf (params self) (clone *default-note-params*))) 
  (unless (listp (caar (params self))) (setf (params self) (list (params self))))
  (when (> (length (params self)) (length (inside (chord self))))
    (setf (params self) (first-n (params self) (length (inside (chord self))))))
  (when (< (length (params self)) (length (inside (chord self))))
    (let ((diff (- (length (inside (chord self))) (length (params self)))))
      (setf (params self) (append (params self) (repeat-n (clone (car (last (params self)))) diff)))))

  (setf (operateurs self) (remove nil (mapcar #'(lambda (f) (when (fboundp (vps-op-get-fun f)) (vps-op-get-fun f)))
                                              (list! (operateurs self)))))
  (setf (inside (chord self)) 
        (loop for note in (inside (chord self)) 
              for i = 0 then (+ i 1) 
              collect (let ((n (make-instance 'note-spectra :midic (midic note) :vel (vel note))))
                        (when (and (params self) (nth i (params self)))
                          (set-vps-params n (nth i (params self))))
                        (setf (ops n) (operateurs self))
                        (update-vps n)
                        n)))

  (setf (spectra self) (make-new-spectra (chord self)))

  ;(setf (synth-class self) (mapcar (lambda (c) (if (symbolp c) c (type-of c))) (list! (synth-class self))))
  (setf (synth-class self) (if (symbolp (synth-class self)) (synth-class self) (type-of (synth-class self))))
  )



(defclass chord-spectra (chord) ())

(defmethod set-defaul-value ((self chord))
   (setf (inside self) nil))

(defclass note-spectra (note)
  ((ops :accessor ops :initarg :ops :initform nil)
   (vps-params :accessor vps-params :initarg :vps-params :initform (clone *default-note-params*))
   (vps :accessor vps :initarg :vps :initform nil)))


(defvar *default-note-params*
  '((:nb-harm 1) (:stretch-fact (100))))


;(get-note-param (default-note-params) :nb-harm)


(defmethod get-note-param ((self note-spectra) key)
  (cadr (find key (vps-params self) :key 'car)))

(defmethod set-vps-params ((self note-spectra) params)
  (loop for p in params do
        (set-note-param self (car p) (cadr p))))

(defmethod set-note-param ((self note-spectra) key val)
  (let ((p (position key (vps-params self) :key 'car)))
    (if p
        (setf (nth p (vps-params self)) (list key val))
      (setf (vps-params self) (append (vps-params self) (list (list key val)))))))


(defmethod initialize-instance :after ((self note-spectra) &rest args)
  (setf (vps self) (make-instance 'cr::fql 
                                  :the-list (list (mc->f (midic self))) 
                                  :amplitudes (list (om-scale (vel self) 0.0 1.0 0 127)))))


(defmethod update-vps ((self note-spectra))
  (let* ((base (mc->f (midic self)))
         (listf (om* (arithm-ser 1 (get-note-param self :nb-harm) 1) base))
         (lista (get-note-param self :amps))
         freqs amps)
    
    
    
    (when (get-note-param self :nth-remove)
      (setf listf (position-remove (get-note-param self :nth-remove) listf)))
    
    (loop while (< (length lista) (length listf)) do
          (pushr (om-scale (vel self) 0.0 0.9 0 127) lista))

    (loop for addp in (get-note-param self :add-partiels) do
          ;(pushr (+ (car addp) base) listf)
          (pushr (car addp) listf)
          (pushr (cadr addp) lista))
    
    (multiple-value-setq (listf lista) (om-sort-partiels listf lista))

    ;(print listf)

    (loop for op in (ops self) do 
          (when (fboundp op)
            (let ((tmpvps ;(make-instance 'cr::fql :the-list listf :amplitudes lista)))
                          (apply op 
                                 (append (list (make-instance 'cr::fql :the-list listf :amplitudes lista)) 
                                         (list! (or (get-note-param self (vps-op-keyword op)) (vps-op-defvals op)))))))
              (setf listf (get-vps-freqs tmpvps)
                    lista (get-vps-amps tmpvps))
              (multiple-value-setq (listf lista) (om-sort-partiels listf lista)))))
   
    (initialize-instance (vps self) :the-list listf :amplitudes lista)))


(defmethod transpose-a :after ((self note-spectra) trans)
  (update-vps self))



;; OPERATEURS = SYMBOLS 
(defmethod vps-op-get-fun ((self t)) self)
(defmethod vps-op-get-fun ((self standard-generic-function)) (function-name self))
(defmethod vps-op-get-fun ((self OMLispFun)) (funname self))

;;; methodes à définir pour les différents opérateurs
(defmethod vps-op-name ((self t)) (or (string self) "*@$*% !!"))
(defmethod vps-op-keyword ((self t)) (intern self))
(defmethod vps-op-defvals ((self t)) (repeat-n 20 (length (cdr (function-lambda-list self)))))
(defmethod vps-op-ranges ((self t)) (repeat-n '(0 1000) (length (cdr (function-lambda-list self)))))

(defmethod vps-op-types ((self symbol)) (vps-op-types (fdefinition self)))
(defmethod vps-op-types ((self t)) (repeat-n 't (length (function-lambda-list self))))
(defmethod vps-op-types ((self standard-generic-function)) 
  (loop for arg in (method-specializers (car (generic-function-methods self))) collect
        (class-name arg)))


; special for OMGenericFunctions
(defmethod vps-op-name ((self symbol))
  (let ((fun (fdefinition self)))
    (if (omgenfun-p fun) (name fun))
    (call-next-method)))

(defmethod vps-op-defvals ((self symbol))
  (let ((fun (fdefinition self))) 
    (or (and (omgenfun-p fun) (cdr (inputs-default fun)))
        (call-next-method))))

(defmethod update-model-component ((self soundtarget) n vals)
  (let ((note (nth n (inside (chord self)))))
    (setf (vps-params note) vals)
    (update-vps note)
    (setf (params self) (loop for n in (inside (chord self)) collect (vps-params n)))
    ))

(defmethod make-new-spectra ((self chord))
  (merge-vps 
   (make-instance 'vps-list :elements (loop for note in (inside self) collect (vps note)))))

(defmethod update-spectra ((self soundtarget))
  (setf (spectra self) (make-new-spectra (chord self)))
  ;(filter-vps (spectra self) (freq-filter self))
  (when (and (/= 0 (chordinfo self)) (= (chordmode self) 0))
    (when (and (chord self) (targetchord self))
      (setf (inside (targetchord self)) (sort 
                                         (mapcar #'(lambda (m) (make-instance 'note :midic m)) (lmidic (chord self)))
                                         '< :key 'midic))
      )))

(defmethod filter-vps ((self cr::vps) (filter bpf))
   (let ((newfilter (clone filter)))
     (when (> (length (get-vps-freqs self)) 0)
       (when (< (list-min (get-vps-freqs self)) (list-min (x-points filter)))
         (setf newfilter (simple-bpf-from-list (append (list 0 (list-min (get-vps-freqs self))) (x-points filter))
                                               (append (list (car (y-points filter)) (car (y-points filter))) (y-points filter))
                                               'bpf 2))
         )
       
       (when (> (list-max (get-vps-freqs self)) (list-max (x-points newfilter)))
         (setf newfilter (simple-bpf-from-list (append (x-points newfilter) (om+ (list-max (get-vps-freqs self)) '(0 10)))
                                               (append (y-points newfilter) (list (car (last (y-points newfilter))) 
                                                                               (car (last (y-points newfilter)))))
                                               'bpf 2))
         )
       (let ((filter-fact (mapcar #'(lambda (f) (transfer (if (< (length (y-points newfilter)) 200)
                                                              (get-spline-obj newfilter 200 3)
                                                            newfilter)
                                                          f)) (get-vps-freqs self))))
         ;; filter-fact est en dB
         (setf (cr::amplitudes self) (om* (cr::amplitudes self) (db->lin filter-fact)))
         (initialize-instance self)
         self)
       )
     ))

(defmethod synthesize-soundtarget ((self cr::vps) (ampbpf bpf) &optional (class 'add-1)) 
  (let ((meta-vps (clone self))
        (snd nil))

    (when (or (not (point-list ampbpf))
              (< (length (point-list ampbpf)) 2))
      (setf ampbpf (simple-bpf-from-list '(0 1000) '(50 50))))

    (if (> (length (get-vps-freqs meta-vps)) 0)
        (let ((matrix (apply 'mk-array 
                             (append 
                              (list class (length (get-vps-freqs meta-vps))
                                    (list 0 nil)
                                    :durs (/ (list-max (x-points ampbpf)) 1000.0)
                                    :amp (om-scale (get-vps-amps meta-vps) 0.0 500.0)
                                    :freq (get-vps-freqs meta-vps)
                                    )
                              (when (find 'aenv (mapcar 'slot-definition-name (class-slots (find-class class))))
                                (list :aenv ampbpf))
                              )
                             )))

          (let ((xx (synthesize matrix)))
             (setf snd (objfromobjs xx (make-instance 'sound))))
          )
      (om-message-dialog "The sound target is empty !"))
    
    snd))

(defmethod synthesize-sound ((self soundtarget))
  (let ((newsound 
         (synthesize-soundtarget 
          (filter-vps (clone (spectra self)) (get-current-bpf (freq-filter self)))
          (amp-profile self)
          (synth-class self))))
    (when newsound (setf (snd self) newsound))))

;;;================================
;;;
;;; OM INTERFACE
;;;
;;;================================ 

(defmethod omNG-save ((self soundtarget) &optional (values? nil))
  "Cons a Lisp expression that retunr a copy of self when it is valuated."
  `(let ((rep ,(call-next-method)))
     (setf (analysisparams rep) ,(omng-save (analysisparams self)))
     (setf (chordinfo rep) ,(omng-save (chordinfo self)))
     (setf (chordmode rep) ,(omng-save (chordmode self)))
     (setf (targetchord rep) ,(omng-save (targetchord self)))
     rep
     ))

(defmethod omNG-copy ((self soundtarget))
  "Cons a Lisp expression that retunr a copy of self when it is valuated."
  `(let ((rep ,(call-next-method)))
     (setf (analysisparams rep) ,(omng-copy (analysisparams self)))
     (setf (chordinfo rep) ,(omng-copy (chordinfo self)))
     (setf (chordmode rep) ,(omng-copy (chordmode self)))
     (setf (targetchord rep) ,(omng-copy (targetchord self)))
     rep
     ))

(defmethod thesound ((self soundtarget)) (snd self))

(defmethod Class-has-editor-p ((self soundtarget)) t)

(defmethod get-fonde-pict ((self soundtarget)) (or *orcobject-pict* *boxedit-pict*))

(defmethod get-initval ((self soundtarget)) (make-instance 'soundtarget :chord (make-instance 'chord-spectra)))

(defmethod get-boxsize ((self soundtarget)) (om-make-point 60 60))

;;;================================
;;;
;;; EDITOR
;;;
;;;================================

(defmethod get-win-ed-pos ((self soundtarget)) (om-make-point 600 200))
(defmethod get-win-ed-size ((self soundtarget)) (om-make-point 1060 600))
(defmethod get-win-ed-size2 ((self soundtarget)) (om-make-point 700 300))


(defmethod default-edition-params ((self soundtarget)) 
  (pairlis '(winsize winpos mode) 
           (list (om-make-point 700 300) (om-make-point 600 200) 0)))

(defmethod get-editor-class ((self soundtarget)) 'targetEditor)


(defclass targetEditor (EditorView)
              ((targetp :accessor targetp :initform nil)
               (synthp :accessor synthp :initform nil)
               (chordp :accessor chordp :initform nil)
               (spectp :accessor spectp :initform nil)
               (notep :accessor notep :initform nil)              
               (focus :accessor focus :initform nil)
               (selected-component :accessor selected-component :initarg :selected-component :initform nil)
               (display-mode :accessor display-mode :initarg :display-mode :initform 0)
               ))


(defmethod make-editor-window ((class (eql 'targetEditor)) object name ref &key 
                                 winsize winpos (close-p t) (winshow t) 
                                 (resize nil) (maximize nil))
  (let* ((size (if (= (get-edit-param ref 'mode) 0) (om-make-point 700 300) (om-make-point 1100 600)))
        (win (call-next-method class object name ref :winsize size :winpos winpos :resize nil 
                               :close-p t :winshow t
                               )))
    win))

(defvar *targeteditor-font* nil)
(setf *targeteditor-font* (om-make-font "Verdana" 12 :style '(:bold)))

(defmethod close-editor-after ((self targetEditor))  
  (call-next-method)
  (setf (selected-component self) nil))



(defmethod metaobj-scrollbars-params ((self targetEditor))  '(nil nil))

#|
(defmethod update-subviews ((self targeteditor))
   (if (= 0 (display-mode self))
       (progn 
          (om-set-view-position (chordp self) (om-make-point -100 -100))
          (om-set-view-size (chordp self) (om-make-point 80 80))
          
          (om-set-view-position (notep self) (om-make-point -100 -100))
          (om-set-view-size (notep self) (om-make-point 80 80))
          
          (om-set-view-position (spectp self) (om-make-point -100 -100))
          (om-set-view-size (spectp self) (om-make-point 80 80))
          
          (om-set-view-position (synthp self) (om-make-point -100 -100))
          (om-set-view-size (synthp self) (om-make-point 80 80))
          
          (om-set-view-position (targetp self) (om-make-point 0 0))
          (om-set-view-size (targetp self) (om-view-size self))
         )
(progn
  (om-set-view-position (chordp self) (om-make-point 0 0))
  (om-set-view-size (chordp self) (om-make-point (round (w self) 4) (round (h self) 2)))
  
  (om-set-view-position (notep self) (om-make-point (round (w self) 4) 0))
  (om-set-view-size (notep self) (om-make-point (round (w self) 2) (round (h self) 2)))
  
  (om-set-view-position (spectp self) (om-make-point (round (* 3 (w self)) 4) 0))
  (om-set-view-size (spectp self) (om-make-point (round (w self) 4) (round (h self) 2)))
  
  (om-set-view-position (synthp self) (om-make-point 0 (round (h self) 2)))
  (om-set-view-size (synthp self) (om-make-point (round (w self) 3) (round (h self) 2)))
  
  (om-set-view-position (targetp self) (om-make-point (round (w self) 3) (round (h self) 2)))
  (om-set-view-size (targetp self) (om-make-point (round (* 2 (w self)) 3) (round (h self) 2))))
))
|#

(defmethod update-subviews ((self targeteditor))
  
  
  (om-set-view-size (targetp self) (om-make-point 700 300))
  (om-set-view-position (targetp self) (om-make-point (- (w self) (w (targetp self))) (- (h self) (h (targetp self)))))

  (om-set-view-position (chordp self) (om-make-point 0 0))
  (om-set-view-size (chordp self) (om-make-point (round (w self) 4) (- (h self) (h (targetp self)))))
  
  (om-set-view-position (notep self) (om-make-point (round (w self) 4) 0))
  (om-set-view-size (notep self) (om-make-point (round (w self) 2) (- (h self) (h (targetp self)))))
  
  (om-set-view-position (spectp self) (om-make-point (round (* 3 (w self)) 4) 0))
  (om-set-view-size (spectp self) (om-make-point (round (w self) 4) (- (h self) (h (targetp self)))))
  
  (om-set-view-position (synthp self) (om-make-point 0 (y (targetp self))))
  (om-set-view-size (synthp self) (om-make-point (- (w self) (w (targetp self))) (h (targetp self))))
  

)

(defmethod open-target-edit ((self targeteditor))
  
  (set-edit-param (ref self) 'mode 1)
  ;(om-set-view-size (window self) (om-make-point (om-point-h (get-win-ed-size (object self))) (om-point-v (om-view-size (window self)))))
  (om-set-view-size (window self) (om-make-point (om-point-h (om-view-size (window self))) (om-point-v (get-win-ed-size (object self)))))
  (setf (display-mode self) 1)
  (om-set-view-size (window self) (get-win-ed-size (object self)))
  
  (update-subviews self)
)

(defmethod close-target-edit ((self targeteditor))
  
  (set-edit-param (ref self) 'mode 0)
  ;(om-set-view-size (window self) (om-make-point (om-point-h (om-view-size (window self))) (om-point-v (get-win-ed-size2 (object self)))))
  (om-set-view-size (window self) (om-make-point (om-point-h (get-win-ed-size2 (object self))) (om-point-v (om-view-size (window self)))))
  (setf (display-mode self) 0)
  (om-set-view-size (window self) (get-win-ed-size2 (object self)))

  (update-subviews self)
)

(defparameter *ste-color-1* (om-make-color 0.38 0.24 0.22))
(defparameter *ste-color-2* (om-make-color 0.47 0.4 0.38))

;(let ((c (om-choose-color-dialog :color *ste-color-1*)))
;  (list (om-color-r c) (om-color-g c) (om-color-b c)))

(defmethod initialize-instance :after ((self targetEditor) &rest l)
  (declare (ignore l))
  (om-set-bg-color self *om-dark-gray-color*)
  (om-add-subviews self 
                   (setf (chordp self) (om-make-view 'targetchordeditor 
                                                     :owner self
                                                     :object (chord (object self))
                                                     :bg-color *ste-color-2*
                                                     :c++ *om-black-color*
                                                     :c+ *om-dark-gray-color*
                                                     :c-- *om-black-color*
                                                     :c- *om-dark-gray-color*))
                   (setf (spectp self) (om-make-view 'targetspeceditor 
                                                     :amp-unit :db
                                                     :owner self
                                                     :object (spectra (object self))
                                                     :bg-color *ste-color-2*
                                                     :c++ *om-black-color*
                                                     :c+ *om-dark-gray-color*
                                                     :c-- *om-black-color*
                                                     :c- *om-dark-gray-color*))
                   (setf (synthp self) (om-make-view 'targetsyntheditor 
                                                     :owner self
                                                     :bg-color *ste-color-2*
                                                     :c++ *om-black-color*
                                                     :c+ *om-dark-gray-color*
                                                     :c-- *om-black-color*
                                                     :c- *om-dark-gray-color*))
                   (setf (notep self) (om-make-view 'notespeceditor 
                                                    :owner self
                                                    :amp-unit :db
                                                    :bg-color *ste-color-2*
                                                    :c++ *om-black-color*
                                                    :c+ *om-dark-gray-color*
                                                    :c-- *om-black-color*
                                                    :c- *om-dark-gray-color*))
                   (setf (targetp self) (om-make-view 'targetdataeditor
                                                   :object (amp-profile (object self))
                                                   :owner self
                                                   :bg-color *ste-color-1*
                                                   :c++ *om-black-color*
                                                   :c+ *om-dark-gray-color*
                                                   :c-- *om-black-color*
                                                   :c- *om-dark-gray-color*)))
 
    ;(update-subviews self)
  (setf (display-mode self) (get-edit-param (ref self) 'mode))
  (update-object-spectra self)
  )




(defmethod handle-key-event ((self targetEditor) char)
  (if (and (equal :om-key-tab char)
           (equal (focus self) (panel (filtereditor (synthp self)))))
      (handle-key-event (focus self) char)
    (if (equal :om-key-tab char)
        (if (selected-component self)
            (setf (selected-component self) (mod (+ (selected-component self) 1) (length (inside (chord (object self))))))
          (setf (selected-component self) 0))
      (if (focus self)
          (handle-key-event (focus self) char)))))

(defmethod editor-null-event-handler ((self om-view)) nil)
(defmethod editor-null-event-handler ((self targetEditor))
  (when (and (focus self) (editor (focus self))) (editor-null-event-handler (editor (focus self)))))

(defmethod (setf selected-component) :after (n (self targetEditor))
 
  (off-selection (panel (chordp self)))

  (when (integerp n)
    (setf (selection? (panel (chordp self))) 
          (list (nth n (inside (object (chordp self)))))))
  
  (om-invalidate-view (panel (chordp self)))
  
  
  (setf (object (notep self)) (if (integerp n)
                                  (vps (nth n (inside (chord (object self)))))
                                nil))
  
  (update-params (noteparamsp (notep self)))
  
  (om-invalidate-view (notep self))
  )


(defmethod editor-select-all ((Self targetEditor))
  (if (focus self)
    (editor-select-all (focus self))))

(defmethod record-undo ((self targetEditor)) nil)

(defmethod update-editor-after-eval ((self targetEditor) val)
  (call-next-method)
  (update-editor-after-eval (chordp self) (chord val))
  (om-close-window (om-view-window self))
  ;(when (selected-component self) 
  ;  (update-editor-after-eval (notep self) (vps (nth (selected-component self) (inside (chord val))))))
  )

;;;===========================
;;; General Editor Operations

(defmethod update-object-spectra ((self targeteditor))
  (update-spectra (object self))
  (om-invalidate-view (notep self))
  (setf (object (spectp self)) (spectra (object self)))
  ;(setf (selectedvps (spectp self)) :all)
  (om-invalidate-view (spectp self))
  (setf (chord (chordp (targetp self))) (targetchord (object self)))
  (update-panel (chordp (targetp self))))

(defmethod synthesize-sound ((self targeteditor))
  (synthesize-sound (object self))
  (om-invalidate-view (targetp self)))



;;;====================================
;;; ACCORD
;;;====================================

(defclass targetchordeditor (chordeditor 3dborder-view) ())
(defclass targetchordpanel (chordpanel) ())
(defmethod get-score-class-panel ((self targetchordeditor)) 'targetchordpanel)
(defmethod metaobj-scrollbars-params ((self targetchordeditor)) nil nil)

(defmethod om-get-menu-context ((self targetchordeditor)) nil)
(defmethod get-inspector ((self targetchordeditor)) nil)
(defmethod update-inspector ((self targetchordeditor) num) nil)

(defmethod object ((self targetchordeditor))
  (if (om-view-container self)
    (chord (object (om-view-container self)))
    (make-instance 'chord)
    ))

(defmethod om-h-scroll-position ((self targetchordpanel)) -7)

(defmethod initialize-instance :after ((self targetchordeditor) &rest L) 
   (declare (ignore l))
   (om-remove-subviews self (ctr-view self) (title-bar self))
   (om-add-subviews self 
                    (om-make-dialog-item 'om-static-text (om-make-point 30 15) 
                                         (om-make-point 200 20) "CHORD"
                                         :font *targeteditor-font*
                                         :fg-color *om-white-color*))
   (setf (staff-tone (panel self)) 4)
   (setf (obj-mode (panel self)) "note")
   (setf (staff-size (panel self)) 24)
   (setf (mode self) 7)
   (score-top-margin (panel self) 1)
   (setf (staff-sys (panel self)) (get-staff-system 'ggff))
   (om-set-field-size (panel self) (om-make-point 160 140))
   (update-panel (panel self))
   (update-subviews self))

(defmethod update-subviews ((self targetchordeditor))
   (om-set-view-size (panel self) (om-make-point (- (w self) 60) (- (h self) 80)))
   (om-set-view-position (panel self) (om-make-point 30 50))
   (om-invalidate-view self))


(defmethod handle-key-event ((self targetchordpanel) char)
  (let* ((targeteditor (om-view-container (editor self)))
         (target (object targeteditor))
         (tmplist nil))
  (case char 
    (:om-key-up 
     (call-next-method)
     (update-object-spectra targeteditor))
    (:om-key-down 
     (call-next-method)
     (update-object-spectra targeteditor))
    (:om-key-delete 
     (call-next-method)
     (setf (selected-component targeteditor) nil)
     (update-object-spectra targeteditor)
     )
    (otherwise nil))
  ))

(defmethod om-view-doubleclick-handler ((self targetchordpanel) where) nil)

(defmethod om-view-click-handler ((self targetchordpanel) where)
  (setf (focus (om-view-container (editor self))) self)
  (let* ((mode-obj (grap-class-from-type (obj-mode self)))
         (graph-obj (get-click-in-obj self (graphic-obj self) mode-obj where)))
    (cond 
     ((om-add-key-p)
      (add-new-object self mode-obj where graph-obj)
      (update-object-spectra (om-view-container (editor self)))
      (setf (selected-component (om-view-container (editor self))) nil))
     (graph-obj 
      (if (om-shift-key-p) (omselect-with-shift self graph-obj)
        (when (not (member (reference graph-obj) (selection? self) :test 'equal))
          (off-selection self)
          (select-note self graph-obj))))
     (t 
      (setf (selected-component (om-view-container (editor self))) nil)
      (om-with-focused-view self (control-actives self where))))
    (om-invalidate-view self)))

(defmethod transpose-drag :after ((self targetchordpanel) list first-mouse)
  (update-object-spectra (om-view-container (editor self))))

(defmethod do-select-items-in-rect :after ((self targetchordpanel) rect) 
  (when (= 1 (length (selection? self)))
    (setf (selected-component (om-view-container (editor self))) 
          (position (car (selection? self)) (inside (object (editor self)))))))

(defmethod add-new-object ((self targetchordpanel) obj where graph-obj)
  (let* ((up (round (* (score-top-margin self) (staff-size self))))
         (midic (delta-to-name (staff-size self)  
                               (- (* -1 (-  (om-point-v where) up))
                                  (round (* (posy (car (staff-list (staff-sys self)))) (/ (staff-size self) 4))))
                               (* 100 (- (top-in-midi (staff-sys self)) 3))))
         (thechord (object (om-view-container self))))
    (setf (inside thechord) (sort 
                             (concatenate 'list (inside thechord) 
                                          (list (make-instance 'note-spectra :midic midic
                                                               :ops (operateurs (object (om-view-container (editor self)))))))
                             '< :key 'midic))
    (update-panel self t)))

(defmethod off-selection ((self targetchordpanel))
  (call-next-method)
  (loop for item in (inside (object (editor self))) do
        (set-mus-color item *om-black-color*)))

(defmethod (setf selection?) (selection (self targetchordpanel))
  (call-next-method)
  (loop for item in (selection? self) do
        (set-mus-color item *om-red2-color*)))

(defmethod select-note ((self targetchordpanel) g-obj)
  (call-next-method)
  (setf (selected-component (om-view-container (editor self))) (position (reference g-obj) (inside (object (editor self))))))

;(defmethod report-modifications ((self targetchordeditor))
  ;(update-model (editor (om-view-container self)))
  ;(call-next-method))

(defmethod om-draw-contents :after ((self targetchordeditor))
  (om-with-focused-view self
    (om-with-fg-color self *om-white-color*
      
      (om-with-line-size 4
        (om-draw-line (- (w self) 24) 120 (- (w self) 4) 120)
        
        (om-draw-line 160 (- (h self) 12) (- (w self) 4) (- (h self) 12))
        (om-draw-line 160 (- (h self) 4) 160 (- (h self) 12))
        )
      )))


;;;====================================
;;; SELECTED NOTE
;;;====================================

(defclass notespeceditor (3Dborder-view vpseditor) 
  ((noteparamsp :accessor noteparamsp :initarg :noteparamsp :initform nil)))

(defclass notespecpanel (vpspanel) ())
(defmethod get-panel-class ((self notespeceditor)) 'notespecpanel)

(defmethod get-env-view ((self notespeceditor)) 'notespecenv-panel)
(defclass notespecenv-panel (specenv-panel) ())


(defmethod initialize-instance :after ((Self notespeceditor) &rest L) 
   (declare (ignore l))
   ;(om-remove-subviews self (title-bar self))
   (om-add-subviews self 
                    (om-make-dialog-item 'om-static-text (om-make-point 30 15) 
                                         (om-make-point 300 20) "Selected Note (Spectral Edit)"
                                         :font *targeteditor-font*
                                         :fg-color *om-white-color*)
                    (setf (noteparamsp self) (om-make-view 'noteparamsview))
                    )
   (setf (editable-p self) t)
   )

(defmethod init-titlebar ((self notespeceditor))
  nil)

(defmethod update-subviews ((self notespeceditor))

  (om-set-view-position  (title-bar self) (om-make-point 30 50))
  (om-set-view-size  (title-bar self) (om-make-point 190 20))

  (om-set-view-position  (panel self) (om-make-point 130 70))
  (om-set-view-size  (panel self) (om-make-point 90 (- (h self) 100)))
  
  (om-set-view-position  (envpanel (panel self)) (om-make-point 70 70))
  (om-set-view-size (envpanel (panel self)) (om-make-point 60 (- (h self) 100)))
  
  (om-set-view-position (rulery (panel self)) (om-make-point 30 70))
  (om-set-view-size (rulery (panel self)) (om-make-point 40 (- (h self) 100)))
  
  (om-set-view-position (noteparamsp self) (om-make-point 260 50))
  (om-set-view-size (noteparamsp self) (om-make-point 240 (- (h self) 80)))
  
  (om-invalidate-view self))

(defmethod update-titlebar-info ((self notespeceditor) point &key (freq t) (amp t))
  (om-with-focused-view (title-bar self)
    (om-with-fg-color (title-bar self) *editor-bar-color*
      (om-fill-rect 2 2 (- (w (title-bar self)) 4) (- (h (title-bar self)) 4)))
    (when freq
      (om-draw-string 110 15 
                      (format () "f = ~5f Hz" (point2freq self (om-point-v point)))))
    (when amp
      (om-draw-string 6 15 
                      (format () "amp = ~5f ~A" (point2amp self (om-point-h point)) (if (equal (amp-unit self) :lin) "(lin)" "dB"))))
    ))


(defmethod om-view-click-handler :before ((self notespecenv-panel) pos)
  (setf (focus (om-view-container (om-view-container self))) (panel (om-view-container self))))

(defmethod om-view-click-handler ((self notespecpanel) pos)
  (setf (focus (om-view-container (editor self))) self)
  (when (selected-component (om-view-container (editor self)))
    (call-next-method)))

(defmethod add-partiel-to-pane :after ((self notespecpanel) where)
  (let* ((note (nth (selected-component (om-view-container (editor self))) 
                   (inside (chord (object (om-view-container (editor self)))))))
        ;(newf (- (point2freq (editor self) (om-point-v where)) (mc->f (midic note))))
        (newf (point2freq (editor self) (om-point-v where)))
        (newa 0.9))
    (set-note-param note :add-partiels (sort (append (get-note-param note :add-partiels) (list (list newf newa))) '< :key 'car))
    (update-object-spectra (om-view-container (editor self)))
    (update-params (noteparamsp (editor self)))))

(defmethod delete-partiels-from-pane :before ((self notespecpanel))
  (let ((vps (selected-object (om-view-container self)))
        (note (nth (selected-component (om-view-container (editor self))) 
                   (inside (chord (object (om-view-container (editor self))))))))
    (loop for pos in (selection? self) do
          (let* ((f (nth pos (get-vps-freqs vps)))
                 (p (position f (get-note-param note :add-partiels) :key 'car)))
            ;;; notes can e deleted only if they are "hand-drawn" notes
            (if p
              (set-note-param note :add-partiels (position-remove p (get-note-param note :add-partiels)))
              (progn (om-beep) (abort)))
            ))))

(defmethod delete-partiels-from-pane :after ((self notespecpanel))
  (update-object-spectra (om-view-container (editor self)))
  (update-params (noteparamsp (editor self))))


(defmethod release-change-amp ((self notespecenv-panel) pos)
  (call-next-method)
  (when (selected-component (om-view-container (om-view-container self)))
  (let* ((vps (selected-object (om-view-container self)))
         (note (nth (selected-component (om-view-container (om-view-container self))) 
                    (inside (chord (object (om-view-container (om-view-container self)))))))
         (params (get-note-param note :add-partiels))
         (amps nil))
    (loop for f in (get-vps-freqs vps) 
          for a in (get-vps-amps vps) do
          (let ((p (position f params :key 'car)))
            (if p
                (progn 
                  (setf (nth p params) (list f a))
                  (set-note-param note :add-partiels params))
              (pushr a amps))))
    (set-note-param note :amps amps)
    )
  (update-object-spectra (om-view-container (om-view-container self)))))



;;;=========
;;; PARAMS
;;;=========

(defclass noteparamsview (om-view) 
  ((op :accessor op :initarg :op :initform nil)
   (vals :accessor vals :initarg :vals :initform nil)))

(defmethod update-params ((self noteparamsview))
  (let* ((editor (editor (om-view-window self)))
         (targetobj (object editor)))
    (setf (op self) (operateurs targetobj))
    ;(if (selected-component editor)
    ;  (setf (vals self) (copy-list (vps-params (nth (selected-component editor) (inside (chord targetobj))))))
      (setf (vals self) nil)
    ;)
    (build-di-components self)))

(defmethod initialize-instance :after ((self noteparamsview) &rest args)
  (om-add-subviews self
                   (om-make-dialog-item 'om-static-text (om-make-point 5 5)
                                        (om-make-point 130 20) "No Selection"
                                        :fg-color *om-gray-color*
                                        :font *targeteditor-font*)))
  

(defmethod build-di-components ((self noteparamsview))
  (let* ((editor (editor (om-view-window self)))
        (targetobj (object editor))
        (note (when (selected-component editor) 
                (nth (selected-component editor) (inside (chord targetobj))))))
    ;(loop for n in (inside (chord targetobj)) do (print (vps-params n)))
    (when (om-subviews self)
      (eval `(om-remove-subviews ,self ,@(om-subviews self))))
    (when note
      (setf (vals self) (copy-list (vps-params note))))
    (if (vals self)
      (let ((i 5)
            (vals (copy-list (vals self))))
      (om-add-subviews self
                   (om-make-dialog-item 'om-static-text (om-make-point 5 5)
                                        (om-make-point 130 20) "Nb Harmonics"
                                        :fg-color *om-dark-gray-color*
                                        :font *targeteditor-font*)
                   (om-make-dialog-item 'numbox (om-make-point 150 (+ i 1))
                                        (om-make-point 36 22) (format nil " ~D" (cadr (find :nb-harm vals :key 'car)))
                                        :bg-color *om-white-color* :font *targeteditor-font*
                                        :min-val 1
                                        :max-val 100
                                        :value (nth 1 (find :nb-harm vals :key 'car))
                                        :afterfun (lambda (item) 
                                                    (let ((p (position :nb-harm (vals self) :key 'car)))
                                                      (setf (cadr (nth p vals)) (value item))
                                                      (update-model-component targetobj (selected-component editor) vals)
                                                      (update-object-spectra editor)
                                                      ))))
      
      (setf i (+ i 25))
      (om-add-subviews self
                   (om-make-dialog-item 'om-static-text (om-make-point 5 i)
                                        (om-make-point 150 20) 
                                        "Add. Partials"
                                        :fg-color *om-dark-gray-color*
                                        :font *targeteditor-font*)
                   (om-make-dialog-item 'om-static-text (om-make-point 160 i)
                                        (om-make-point 40 20) 
                                        (integer-to-string (length (cadr (find :add-partiels vals :key 'car))))
                                        :font *targeteditor-font*)
                   (om-make-view 'om-icon-button :position (om-make-point 200 (+ i 2))
                                        :size (om-make-point 20 20) 
                                        :icon1 "x" :icon2 "x-pushed"
                                        :action #'(lambda (item) 
                                                     (let ((p (position :add-partiels vals :key 'car)))
                                                       (when p
                                                         (setf (cadr (nth p vals)) nil)
                                                         (update-model-component targetobj (selected-component editor) vals)
                                                         (update-object-spectra editor)
                                                         (update-params self)
                                                         ))))
                   )
      (setf i (+ i 45))
      (loop for o in (op self) do
            (om-add-subviews self 
                             (om-make-dialog-item 'om-static-text (om-make-point 5 (+ i 3))
                                                  (om-make-point 100 20) (vps-op-name o)
                                                  :fg-color *om-dark-gray-color*
                                                  :font (om-make-font "verdana" 12 :style '(:bold))))
            (let ((j -1) key defvals ranges)
              (setf key (vps-op-keyword o)
                    defvals (clone (vps-op-defvals o))
                    ranges (vps-op-ranges o))
              (loop for arg in (cdr (function-lambda-list o)) 
                    for narg = 1 then (+ narg 1) do
                    (setf j (+ j 1))
                    (let ((v (nth j (list! (or (nth 1 (find key vals :key 'car))
                                               defvals))))
                          (type (nth narg (vps-op-types o)))
                           pos)
                      (setf pos j)
                      (if (subtypep type 'bpf)
                          (om-add-subviews self 
                                          (om-make-view 'bpf-drop-area :size (om-make-point 50 25)
                                                        :position (om-make-point (+ 150 (* 60 j)) (- i 2))
                                                        :value v
                                                        :afterfun (lambda (item) 
                                                                        (unless (find key vals :key 'car)
                                                                          (pushr (list key defvals) vals))
                                                                          (let ((p (position key vals :key 'car)))
                                                                            (if p                                                     
                                                                                (setf (nth pos (cadr (nth p vals))) (value item))
                                                                              (print (string+ "problem: values not found for " (string o))))
                                                                        (update-model-component targetobj (selected-component editor) vals)
                                                                        (update-object-spectra editor)
                                                                        )
                                                                          )))
                          (om-add-subviews self 
                                       (om-make-dialog-item 'edit-numbox (om-make-point (+ 110 (* 60 j)) (+ i 1))
                                                            (om-make-point 56 20) (format nil " ~D" v)
                                                            :bg-color *om-white-color* :font *controls-font*
                                                            :min-val (car (nth pos ranges))
                                                            :max-val (cadr (nth pos ranges))
                                                            :value v 
                                                            :afterfun (lambda (item) 
                                                                        (unless (find key vals :key 'car)
                                                                          (pushr (list key defvals) vals))
                                                                          (let ((p (position key vals :key 'car)))
                                                                            (if p                                                     
                                                                                (setf (nth pos (cadr (nth p vals))) (value item))
                                                                              (print (string+ "problem: values not found for " (string o))))
                                                                        ;(print vals)
                                                                        (update-model-component targetobj (selected-component editor) vals)
                                                                        (update-object-spectra editor)
                                                                        )
                                                            )
                                                            ))
                          ))
                    )
              (setf i (+ i 25))
              ))
            )
      (om-add-subviews self
                       (om-make-dialog-item 'om-static-text (om-make-point 5 5)
                                            (om-make-point 130 20) "No Selection"
                                            :fg-color *om-gray-color*
                                            :font *targeteditor-font*)))))

(defclass bpf-drop-area (drop-area) 
              ((value :initarg :value :accessor value :initform nil)
               (afterfun :initarg :afterfun :accessor afterfun :initform nil)
               (object :accessor object :initarg :object :initform nil)))


(defmethod om-drag-receive  ((view bpf-drop-area) 
                             (dragged-view t) position 
                             &optional (effect nil))
    (when (subtypep (class-name (class-of (value (object (get-drag-object dragged-view))))) 'bpf)
      (setf (value view) (clone (value (object (get-drag-object dragged-view)))))
      (om-invalidate-view view)
      (when (afterfun view)
        (funcall (afterfun view) view))
      t))

(defmethod om-draw-contents ((self bpf-drop-area))
  (om-draw-view-outline self)
  (when (value self)
    (draw-obj-in-rect (value self) 0 (w self) 0 (h self) nil self)))

(defmethod om-draw-contents :after ((self notespeceditor))
  (om-with-focused-view self
    (om-with-fg-color self *om-white-color*
    
      (om-with-dashline '(8 8)
        (om-with-line-size 4
          (om-draw-line 4 (- (h self) 12) (- (w self) 4) (- (h self) 12))
          (om-draw-line 4 120 (- (w self) 4) 120)
          )
      ))))

;;;===================
;;; FULL SPECTRUM
;;;===================

(defclass targetspeceditor (vpseditor 3Dborder-view) ())

(defclass targetspecpanel (vpspanel) ())
(defmethod get-panel-class ((self targetspeceditor)) 'targetspecpanel)

(defclass fullspecenv-panel (specenv-panel) ())
(defmethod get-env-view ((self targetspeceditor)) 'fullspecenv-panel)

(defmethod initialize-instance :after ((Self targetspeceditor) &rest L) 
   (declare (ignore l))
   (om-add-subviews self 
                    (om-make-dialog-item 'om-static-text (om-make-point 30 15) 
                                         (om-make-point 200 24) "FULL SPECTRUM"
                                         :fg-color *om-white-color*
                                         :font *targeteditor-font*))  
   (update-subviews self))

(defmethod init-titlebar ((self targetspeceditor)) nil)

(defmethod update-titlebar-info ((self targetspeceditor) point &key (freq t) (amp t))
  (om-with-focused-view (title-bar self)
    (om-with-fg-color (title-bar self) *editor-bar-color*
      (om-fill-rect 2 2 (- (w (title-bar self)) 4) (- (h (title-bar self)) 4)))
    (when freq
      (om-draw-string 110 15 
                      (format () "f = ~5f Hz" (point2freq self (om-point-v point)))))
    (when amp
      (om-draw-string 6 15 
                      (format () "amp = ~5f ~A" (point2amp self (om-point-h point)) (if (equal (amp-unit self) :lin) "(lin)" "dB"))))
    ))

(defmethod update-subviews ((self targetspeceditor))
  (om-set-view-position (panel self) (om-make-point 120 70))
  (om-set-view-size (panel self) (om-make-point 100 (- (h self) 100)))

  (om-set-view-position (title-bar self) (om-make-point 30 50))
  (om-set-view-size (title-bar self) (om-make-point 190 20))
  
  (om-set-view-position (envpanel (panel self)) (om-make-point 70 70))
  (om-set-view-size (envpanel (panel self)) (om-make-point 50 (- (h self) 100)))
  
  (om-set-view-position (rulery (panel self)) (om-make-point 30 70))
  (om-set-view-size (rulery (panel self)) (om-make-point 40 (- (h self) 100)))

  (om-invalidate-view self))

(defmethod om-view-click-handler ((self targetspecpanel) pos)
  (setf (focus (om-view-container (editor self))) self)
  (call-next-method))

(defmethod om-view-click-handler ((self fullspecenv-panel) pos)
  (setf (focus (om-view-container (om-view-container self))) self))
  
(defmethod update-object ((self targetspeceditor))
  (update-object-spectra (om-view-container self))
  (om-invalidate-view self))

(defmethod add-partiel-to-pane ((self targetspecpanel) where) nil)
(defmethod delete-partiels-from-pane ((self targetspecpanel)) nil)

(defmethod om-draw-contents :after ((self targetspeceditor))
  (om-with-focused-view self
    (om-with-fg-color self *om-white-color*
      (om-with-line-size 4
        (om-draw-line 4 (- (h self) 12) 130 (- (h self) 12))
        (om-draw-line 130 (- (h self) 12) 130 (- (h self) 30))
        (om-draw-line 4 120 30 120)
          
      ))))

;;;====================================
;;; BPF PANEL
;;;====================================
(defclass targetbpfeditor (bpfeditor) 
              ((y-range :accessor y-range :initarg :y-range :initform '(0 10))))
(defclass targetbpfpanel (bpfpanel) ())

(defmethod get-panel-class ((self targetbpfeditor)) 'targetbpfpanel)

(defmethod initialize-instance :after ((Self targetbpfeditor) &rest L) 
   (declare (ignore l))
   (om-remove-subviews self (title-bar self) (control self) (rulery (panel self)))
   (set-ranges (panel self)  (list 0 (* (last-elem (x-points (currentbpf (panel self)))) (expt 10 (decimals (currentbpf (panel self))))))
               ;(list 
               ; (- (* (list-min (y-points (object self))) (expt 10 (decimals (object self)))) 10)
               ; (+ (* (list-max (y-points (object self))) (expt 10 (decimals (object self)))) 10))
               (y-range self)
               )
   (setf (grille-p (panel self)) t)
   (setf (show-back-p (panel self)) nil)
   
   )



(defmethod update-subviews ((self targetbpfeditor))
  (let ((rulerssize 15))
   (om-set-view-position (panel self) (om-make-point 0 0))
   (om-set-view-size  (panel self) (om-make-point (w self) (- (h self) rulerssize)))
   (om-set-view-position (rulerx (panel self)) (om-make-point 0 (- (h self) rulerssize)))
   (om-set-view-size (rulerx (panel self)) (om-make-point (w self) rulerssize))
   (om-invalidate-view self)))


(defmethod om-draw-view-outline ((self targetbpfpanel) &optional pensize) nil)

(defmethod ruleroffsety-from-editor ((self targetbpfeditor)) -8)

(defun set-filter-spline (panel)
  (set-spline-preview (spline (om-view-container panel)) (< (length (y-points (currentbpf panel))) 100)))

(defmethod handle-key-event ((Self targetbpfpanel) Char)
  (let ((myobj (currentbpf self)))
     (case char
       (:om-key-delete 
        (if (listp (selection? self)) 
          (if (<= (length (point-list Myobj)) 2)
            (om-beep)
            (call-next-method))
          (if (consp (selection? self))
            (call-next-method)
            (om-beep)))
        )
       (:om-key-tab (call-next-method)
        (set-filter-spline self))
        
       (t (unless (characterp char) (call-next-method)
            ))
       )))

(defmethod om-view-click-handler ((self targetbpfpanel) pos)
  (setf (focus (editor (om-view-window self))) self)
  (call-next-method))

(defmethod om-draw-contents ((self targetbpfpanel))
;  (if (equal (focus (editor (om-view-window self))) self)
  (call-next-method))

(defmethod report-modifications ((self targetbpfeditor))
  (update-object (om-view-container self)))

(defmethod update-object ((self t)) nil)

(defmethod show-lines-p ((self targetbpfpanel)) (not (active (spline (editor self)))))

;;;====================================
;;; SYNTH PARAMS & FILTERS
;;;====================================

(defvar *target-synth-classes* nil)
(setf *target-synth-classes* '(add-1 fof-1 snare-1 pluck-1 ran-1))

(defun trans-synth-name (class)
  (case class 
    (add-1 "Additive")
    (fof-1 "FOF")
    (snare-1 "Snare")
    (pluck-1 "Pluck")
    (ran-1 "Randomize")
    (otherwise (string class))))


(defclass targetsyntheditor (3Dborder-view) 
              ((filtereditor :accessor filtereditor :initarg :filtereditor :initform nil)
               (ampview :accessor ampview :initarg :ampview :initform nil)
               (classlist :accessor classlist)
               (b1 :accessor b1 :initarg :b1 :initform nil)             
               ))


(defmethod initialize-instance :after ((Self targetsyntheditor) &rest L) 
   (declare (ignore l))
   (om-add-subviews self 
                    
                    (om-make-dialog-item 'om-static-text (om-make-point 40 10) 
                                         (om-make-point 200 20) "SOUND"
                                         :fg-color *om-white-color*
                                         :font *targeteditor-font*)
                    (om-make-dialog-item 'om-static-text (om-make-point 40 40) 
                                         (om-make-point 100 20) "Filters"
                                         :fg-color *om-white-color*
                                         :font *targeteditor-font*)
                    (om-make-dialog-item 'om-static-text (om-make-point 40 260) 
                                         (om-make-point 200 20) "Synthesis"
                                         :fg-color *om-white-color*
                                         :font *targeteditor-font*)

                    (setf (filtereditor self) 
                          (om-make-view 'targetbpfeditor 
                                        :object (freq-filter (object (om-view-container self)))
                                        :y-range (om* (expt 10 (decimals (freq-filter (object (om-view-container self))))) 
                                                      '(-110 10))
                                        ))
                    
                    (setf (ampview self) 
                          (om-make-view 'targetbpfeditor 
                                        :object (amp-profile (object (om-view-container self)))
                                        :y-range (om* (expt 10 (decimals (amp-profile (object (om-view-container self))))) 
                                                      '(0 120))
                                        ))

                    (setf (classlist self) (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 100 34) (om-make-point 120 20)
                                                            ""
                                                            :range (mapcar 'trans-synth-name *target-synth-classes*)
                                                            :value (trans-synth-name (synth-class (object (om-view-container self))))
                                                            :di-action (om-dialog-item-act item 
                                                                         (setf (synth-class (object (om-view-container self)))
                                                                               (nth (om-get-selected-item-index item) *target-synth-classes*)))
                                                                                 
                                                   ))

                    
                    (setf (b1 self) (om-make-view 'om-icon-button 
                                                   :icon1 "snd" :icon2 "snd-pushed"
                                                   :size (om-make-point 26 26)
                                                   :action (lambda (arg) (synthesize-sound (om-view-container self)))
                                                   ))
                   
        
                    )
   (when (multibpf? (filtereditor self)) 
     (om-add-subviews self                     
                      (om-make-view 'om-icon-button
                                    :size (om-make-point 18 18)
                                    :position (om-make-point 222 40)
                                    :icon1 "prev" :icon2 "prev-pushed"
                                    :action #'(lambda (item) 
                                                (when (multibpf? (filtereditor self))
                                                  (change-current-bpf (panel (filtereditor self)) 
                                                                      (get-bpf-obj (panel (filtereditor self)))  -1)
                                                  (set-filter-spline (panel (filtereditor self))))
                                                ))
                    (om-make-view 'om-icon-button
                                  :size (om-make-point 18 18)
                                  :position (om-make-point 242 40)
                                  :icon1 "next" :icon2 "next-pushed"
                                  :action #'(lambda (item) 
                                              (when (multibpf? (filtereditor self))
                                              (change-current-bpf (panel (filtereditor self)) 
                                                                  (get-bpf-obj (panel (filtereditor self))) 1)
                                              (set-filter-spline (panel (filtereditor self)))
                                              )
                                              ))))
   (setf (s-resolution (spline (filtereditor self))) 50)
   (setf (degre (spline (filtereditor self))) 4)   
   (set-spline-preview (spline (filtereditor self)) (< (length (y-points (currentbpf (panel (filtereditor self))))) 100))

   (update-subviews self))


(defmethod update-subviews ((self targetsyntheditor))

  (om-set-view-position  (filtereditor self) (om-make-point 40 60))
  (om-set-view-size (filtereditor self) (om-make-point 220 90))

  (om-set-view-position (ampview self) (om-make-point 40 165))
  (om-set-view-size  (ampview self) (om-make-point 220 80))


   (om-set-view-position (b1 self) (om-make-point 260 258))
   
   (om-set-view-position (classlist self) (om-make-point 120 260))

  
  (om-invalidate-view self))

(defmethod om-draw-contents :after ((self targetsyntheditor))
  (let ((y-axis-pos (- (om-point-h (om-view-position (filtereditor self))) 24)))
  (om-with-focused-view self
    (om-with-fg-color self *om-white-color*
      (om-draw-string y-axis-pos (+ (om-point-v (om-view-position (filtereditor self))) 16) "  0")
      (om-draw-string y-axis-pos (+ (om-point-v (om-view-position (filtereditor self))) 40) " dB")
      (om-draw-string y-axis-pos (+ (om-point-v (om-view-position (filtereditor self))) 
                                    (om-point-v (om-view-size (filtereditor self)))
                                    -18) "-100")
      
      (om-draw-string (+ (om-point-h (om-view-position (filtereditor self)))
                         (om-point-h (om-view-size (filtereditor self))))
                      (+ (om-point-v (om-view-position (filtereditor self)))
                         (om-point-v (om-view-size (filtereditor self)))
                         -5)
                          "f(Hz)")
      
      (om-draw-string y-axis-pos (+ (om-point-v (om-view-position (ampview self))) 16) "  1")
      (om-draw-string y-axis-pos (+ (om-point-v (om-view-position (ampview self))) 
                                    (om-point-v (om-view-size (ampview self)))
                                    -18) "  0")
      
      (om-draw-string (+ (om-point-h (om-view-position (ampview self)))
                         (om-point-h (om-view-size (ampview self))))
                      (+ (om-point-v (om-view-position (ampview self)))
                         (om-point-v (om-view-size (ampview self)))
                         -5)
                          "t(s)")

      (om-with-line-size 4
        (om-draw-line (- (w self) 65) (- (h self) 30) (- (w self) 30) (- (h self) 30))
        (om-draw-line (- (w self) 30) (- (h self) 30) (- (w self) 30) 80)
        (om-draw-line (- (w self) 30) 80 (- (w self) 4) 80)

        (om-draw-line 160 2 160 40)
        )
      ))))


      
;;;====================================
;;; TARGET PANEL
;;;====================================


    
(defclass targetdataeditor (3Dborder-view) 
              ((chordinfodi :accessor chordinfodi)
               (chordp :accessor chordp)
               (sendb :accessor sendb :initarg :sendb :initform nil)
               (sndview :accessor sndview :initarg :sndview :initform nil)
               (b2 :accessor b2 :initarg :b2 :initform nil)
               (b3 :accessor b3 :initarg :b3 :initform nil)
               (loadb :accessor loadb :initarg :loadb :initform nil)
               (editb :accessor editb :initarg :editb :initform nil)
               (manchoice :accessor manchoice :initarg :manchoice :initform nil)
               (orchichoice :accessor orchichoice :initarg :orchichoice :initform nil)
               (initchoice :accessor initchoice :initarg :initchoice :initform nil)
               ))

;(defmethod object ((self targetdataeditor)) nil)
;(defmethod (setf undo) (val (self targetdataeditor)) nil)

(defclass targetsoundpanel (3dborder-view view-with-ruler-x cursor-play-view-mixin) ())

(defclass minichordpanel (chordpanel) 
              ((chord :accessor chord :initarg :chord :initform (make-instance 'chord :lmidic nil))))

(defmethod initialize-instance :after ((self minichordpanel) &rest args) 
   (declare (ignore l))
   (setf (staff-tone self) 4)
   (setf (obj-mode self) "note")
   (setf (staff-size self) 20)
   (setf (staff-sys self) (get-staff-system 'ggff))
   (setf (deltax (staff-sys self)) 1.4)
   (om-set-field-size self (om-make-point 160 140))
   (update-panel self))

(defmethod editor ((self minichordpanel)) nil)
(defmethod object ((self minichordpanel)) (chord self))
(defmethod score-page-mode ((self minichordpanel)) nil)
(defmethod score-top-margin ((self minichordpanel) &optional val) 0.6)
(defmethod om-h-scroll-position ((self minichordpanel)) -7)
(defmethod score-system-space ((self minichordpanel) &optional val) '(1))
(defmethod init-music-patch ((self minichordpanel)) nil)
(defmethod set-editor-tonality ((self minichordpanel)) nil)
(defmethod objectfromeditor ((self minichordpanel)) (chord self))
(defmethod get-approx-scale ((self minichordpanel))  *current-1/4-scale*)
(defmethod draw-general-tonality ((self minichordpanel)) nil)
(defmethod window ((self minichordpanel)) (om-view-window self))
(defmethod update-slot-edit ((self minichordpanel)) nil)
(defmethod om-drag-start ((self minichordpanel)) nil)
(defmethod om-drag-selection-p ((self minichordpanel) pos) nil)

(defmethod manual-mode ((self minichordpanel))
  (let* ((panel (om-view-container self))
         (targeteditor (om-view-container panel))
         (target (object targeteditor)))
    (setf (chordmode target) 1)
    (setf (chordinfo target) (+ 1 (om-get-selected-item-index (chordinfodi panel))))
    (om-set-check-box (manchoice panel) t)
    (om-set-check-box (orchichoice panel) nil)
    (om-set-check-box (initchoice panel) nil)
    (om-enable-dialog-item (chordinfodi panel) t)
    (enable-chord self t)))

(defmethod handle-key-event ((self minichordpanel) char)
  (let* ((targeteditor (om-view-container (om-view-container self)))
         (target (object targeteditor))
         (tmplist nil))
  (case char 
    (:om-key-up 
     (call-next-method)
     (manual-mode self)
     )
    (:om-key-down 
     (call-next-method)
     (manual-mode self))
    (:om-key-delete 
     (call-next-method)
     (manual-mode self)
     )
    (otherwise nil))
  ))

(defmethod general-delete ((view minichordpanel) (self note))
   (deep-delete-obj self (chord view)))

(defmethod om-view-doubleclick-handler ((self minichordpanel) where) nil)

(defmethod om-view-click-handler ((self minichordpanel) where)
  (setf (focus (om-view-container (om-view-container self))) self)
  (let* ((mode-obj (grap-class-from-type (obj-mode self)))
         (graph-obj (get-click-in-obj self (graphic-obj self) mode-obj where)))
    (cond 
     ((om-add-key-p)
      (add-new-object self mode-obj where graph-obj)
      (manual-mode self))
     (graph-obj 
      (if (om-shift-key-p) (omselect-with-shift self graph-obj)
        (when (not (member (reference graph-obj) (selection? self) :test 'equal))
          (off-selection self)
          (select-note self graph-obj))))
     (t 
      (om-with-focused-view self (control-actives self where))))
    (om-invalidate-view self)))

(defmethod add-new-object ((self minichordpanel) obj where graph-obj)
  (let* ((up (round (* (score-top-margin self) (staff-size self))))
         (midic (delta-to-name (staff-size self)  
                               (- (* -1 (-  (om-point-v where) up))
                                  (round (* (posy (car (staff-list (staff-sys self)))) (/ (staff-size self) 4))))
                               (* 100 (- (top-in-midi (staff-sys self)) 3))))
         (thechord (chord self)))
    (setf (inside thechord) (sort 
                             (concatenate 'list (inside thechord) 
                                          (list (make-instance 'note :midic midic)))
                             '< :key 'midic))
    (update-panel self t)))

(defmethod transpose-drag :after ((self minichordpanel) list first-mouse)
  (manual-mode self))


(defmethod add-new-object ((self minichordpanel) obj where graph-obj)
  (call-next-method))

(defmethod off-selection ((self minichordpanel))
  (call-next-method)
  (loop for item in (inside (object self)) do
        (set-mus-color item (if (/= 0 (chordinfo (object (om-view-container (om-view-container self)))))
                                *om-black-color* *om-gray-color*)))
  )

(defmethod (setf selection?) (selection (self minichordpanel))
  (call-next-method)
  (loop for item in (selection? self) do
        (set-mus-color item *om-red2-color*)))

(defmethod enable-chord ((self minichordpanel) val)
  (loop for item in (inside (object self)) do
        (unless (member item (selection? self))
          (set-mus-color item (if val *om-black-color* *om-gray-color*))))
  (update-panel self))



(defmethod initialize-instance :after ((Self targetdataeditor) &rest L) 
   (declare (ignore l))
   (let ((deltaval nil))
   (om-add-subviews self 
                    (om-make-dialog-item 'om-static-text (om-make-point 30 10) 
                                         (om-make-point 200 20) "TARGET"
                                         :fg-color *om-white-color*
                                         :font *targeteditor-font*)
                    
                    
                    (setf (sndview self) (om-make-view 'targetsoundpanel 
                                                       :size (om-make-point 200 80)
                                                       :position (om-make-point 30 40)))
                    
                    (setf (b2 self) (om-make-view 'om-icon-button 
                                                   :icon1 "play" :icon2 "play-pushed"
                                                   :size (om-make-point 20 20)
                                                   :position (om-make-point 210 122)
                                                   :action (lambda (arg)
                                                                   (when (snd (object (om-view-container self))) 
                                                                      (play (snd (object (om-view-container self))))))
                                                   ))
                    (setf (b3 self) (om-make-view 'om-icon-button 
                                                  :icon1 "stop" :icon2 "stop-pushed"
                                                  :size (om-make-point 20 20)
                                                  :position (om-make-point 188 122)
                                                  :action (lambda (arg) 
                                                            ;;; TMP
                                                            ;(update-subviews (om-view-container self))
                                                            (om-invalidate-view (om-view-container self) t)
                                                            (stop t))
                                                  ))
                    (setf (loadb self) (om-make-view 'om-icon-button 
                                                  :icon1 "folder" :icon2 "folder-pushed"
                                                  :size (om-make-point 20 20)
                                                  :position (om-make-point 30 122)
                                                  :action (lambda (arg) 
                                                            (let ((snd (get-sound)))
                                                              (when snd (setf (snd (object (om-view-container self))) snd))
                                                              (om-invalidate-view (sndview self))))
                                                            ))
                    (setf (editb self) (om-make-view 'om-icon-button 
                                                  :icon1 "snd" :icon2 "snd-pushed"
                                                  :size (om-make-point 60 20)
                                                  :lock-push t
                                                  :position (om-make-point 52 122)
                                                  :selected-p (= 1 (display-mode (om-view-container self)))
                                                  :action (lambda (arg) 
                                                            (if (= 0 (display-mode (om-view-container self)))
                                                                (open-target-edit (om-view-container self))
                                                              (close-target-edit (om-view-container self)))
                                                            (setf (selected-p arg) (= 1 (display-mode (om-view-container self)))))
                                                            ))
                    (om-make-dialog-item 'om-static-text (om-make-point 30 160) 
                                         (om-make-point 200 20) "ANALYSIS PARAMS"
                                         :fg-color *om-white-color*
                                         :font *targeteditor-font*)
                                        
                    (om-make-dialog-item 'om-static-text (om-make-point 30 190) 
                                         (om-make-point 200 20) "f0 min (Hz)"
                                         :fg-color *om-white-color*
                                         :font *targeteditor-font*)
                    (om-make-dialog-item 'om-editable-text (om-make-point 160 190)
                                         (om-make-point 60 10) (integer-to-string (nth 0 (analysisparams (object (om-view-container self)))))
                                         :di-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (read-from-string text))
                                                              (if (and (integerp number) (>= number 0))
                                                                  (setf (nth 0 (analysisparams (object (om-view-container self)))) number)
                                                                (progn 
                                                                  (om-beep)
                                                                  (om-set-dialog-item-text item 
                                                                                           (format nil "~D" (nth 0 (analysisparams (object (om-view-container self)))))))
                                                                ))))
                                         :after-action  (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (read-from-string text))
                                                              (if (and (integerp number) (>= number 0))
                                                                  (setf (nth 0 (analysisparams (object (om-view-container self)))) number)
                                                                (progn 
                                                                  (om-beep)
                                                                  (om-set-dialog-item-text item 
                                                                                           (format nil "~D" (nth 0 (analysisparams (object (om-view-container self)))))))
                                                                ))))
                                         :font *om-default-font2*)
                                        
                    (om-make-dialog-item 'om-static-text (om-make-point 30 220) 
                                         (om-make-point 200 20) "nb partials"
                                         :fg-color *om-white-color*
                                         :font *targeteditor-font*)
                    
                    (om-make-dialog-item 'om-editable-text (om-make-point 160 220)
                                         (om-make-point 60 10) (integer-to-string (nth 1 (analysisparams (object (om-view-container self)))))
                                         :di-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (read-from-string text))
                                                              (if (and (integerp number) (>= number 0))
                                                                  (setf (nth 1 (analysisparams (object (om-view-container self)))) number)
                                                                (progn 
                                                                  (om-beep)
                                                                  (om-set-dialog-item-text item 
                                                                                           (format nil "~D" (nth 1 (analysisparams (object (om-view-container self)))))))
                                                                ))))
                                         :after-action  (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (read-from-string text))
                                                              (if (and (integerp number) (>= number 0))
                                                                  (setf (nth 1 (analysisparams (object (om-view-container self)))) number)
                                                                (progn 
                                                                  (om-beep)
                                                                  (om-set-dialog-item-text item 
                                                                                           (format nil "~D" (nth 1 (analysisparams (object (om-view-container self)))))))
                                                                ))))
                                         :font *om-default-font2*)
                    
                    (om-make-dialog-item 'om-static-text (om-make-point 30 250) 
                                         (om-make-point 200 20) "inharmonicity"
                                         :fg-color *om-white-color*
                                         :font *targeteditor-font*)

                    (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 225 250) (om-make-point 80 20)
                                                            ""
                                                            :range '("Manual" "Auto")
                                                            :value (if (numberp (nth 2 (analysisparams (object (om-view-container self)))))
                                                                       "Manual" "Auto")
                                                            :di-action (om-dialog-item-act item 
                                                                         (om-enable-dialog-item deltaval (= 0 (om-get-selected-item-index item)))
                                                                         (setf (nth 2 (analysisparams (object (om-view-container self))))
                                                                               (if (= 0 (om-get-selected-item-index item))
                                                                                   (read-from-string (om-dialog-item-text deltaval))
                                                                                 t)))
                                                            
                                                            )

                    (setf deltaval (om-make-dialog-item 'om-editable-text (om-make-point 160 250)
                                         (om-make-point 60 10) 
                                         (if (numberp (nth 2 (analysisparams (object (om-view-container self)))))
                                             (integer-to-string (nth 2 (analysisparams (object (om-view-container self)))))
                                           "0.015")
                                         :enable (numberp (nth 2 (analysisparams (object (om-view-container self)))))
                                         :di-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (read-from-string text))
                                                              (if (and (numberp number) (>= number 0))
                                                                  (setf (nth 2 (analysisparams (object (om-view-container self)))) number)
                                                                (progn 
                                                                  (om-beep)
                                                                  (om-set-dialog-item-text item 
                                                                                           (format nil "~D" (nth 2 (analysisparams (object (om-view-container self)))))))
                                                                ))))
                                         :after-action  (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (read-from-string text))
                                                              (if (and (numberp number) (>= number 0))
                                                                  (setf (nth 1 (analysisparams (object (om-view-container self)))) number)
                                                                (progn 
                                                                  (om-beep)
                                                                  (om-set-dialog-item-text item 
                                                                                           (format nil "~D" (nth 2 (analysisparams (object (om-view-container self)))))))
                                                                ))))
                                         :font *om-default-font2*))
                    
                    (om-make-dialog-item 'om-static-text (om-make-point 320 40) 
                                         (om-make-point 250 20) "SEARCH DOMAIN - pitch filter"
                                         :fg-color *om-white-color*
                                         :font *targeteditor-font*)
                    
                    
                    (setf (chordp self) (om-make-view 'minichordpanel 
                                                      :position (om-make-point 550 40) 
                                                      :size (om-make-point 100 150)
                                                      :chord (targetchord (object (om-view-container self)))))

                 
                        (om-make-dialog-item 'om-static-text (om-make-point 345 90) 
                                         (om-make-point 160 20)
                                         "Initial Chord"
                                         :fg-color *om-white-color*
                                         :font *targeteditor-font*)
                        (setf (initchoice self) (om-make-dialog-item 'om-radio-button 
                                                                     (om-make-point 320 90) 
                                                                     (om-make-point 120 20)
                                                                     ""
                                                                     :di-action (om-dialog-item-act item 
                                                                                  (om-enable-dialog-item (chordinfodi self) t)
                                                                                  (setf (chordmode (object (om-view-container self))) 0)
                                                                                  (setf (chordinfo (object (om-view-container self))) (+ 1 (om-get-selected-item-index (chordinfodi self))))
                                                                                  (setf (selection? (chordp self)) nil)
                                                                                  (enable-chord (chordp self) t)
                                                                                  (update-object-spectra (om-view-container self))
                                                                                  )
                                                                     :checked-p (and (/= 0 (chordinfo (object (om-view-container self))))  ;; include or force
                                                                                     (= (chordmode (object (om-view-container self))) 0))  ;; initchord
                                                                     :fg-color *om-white-color*
                                                                     :font *targeteditor-font*))
                        
                        (om-make-dialog-item 'om-static-text (om-make-point 345 120) 
                                             (om-make-point 160 20)
                                             "Manual"
                                             :fg-color *om-white-color*
                                             :font *targeteditor-font*)
                        (setf (manchoice self) (om-make-dialog-item 'om-radio-button 
                                                                (om-make-point 320 120) 
                                                                (om-make-point 20 20)
                                                                ""
                                                                :di-action (om-dialog-item-act item 
                                                                  (om-enable-dialog-item (chordinfodi self) t)
                                                                  (enable-chord (chordp self) t)
                                                                  (setf (chordmode (object (om-view-container self))) 1)
                                                                  (setf (chordinfo (object (om-view-container self))) (+ 1 (om-get-selected-item-index (chordinfodi self))))
                                                                  )
                                                                :fg-color *om-white-color*
                                                                :checked-p (and (/= 0 (chordinfo (object (om-view-container self)))) ;; include or force
                                                                                (= (chordmode (object (om-view-container self))) 1)) ;; manual
                                                                :font *targeteditor-font*))

                        (setf (chordinfodi self) (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 340 150) (om-make-point 190 20)
                                                            ""
                                                            :range '("Include the chord in the search domain" "Constrain search domain with the chord")
                                                            :value (let ((n (if (= 0 (chordinfo (object (om-view-container self))))
                                                                                0
                                                                              (- (chordinfo (object (om-view-container self))) 1))))
                                                                     (nth n '("Include the chord in the search domain" "Constrain search domain with the chord")))
                                                            :di-action (om-dialog-item-act item 
                                                                         (setf (chordinfo (object (om-view-container self)))
                                                                               (+ 1 (om-get-selected-item-index item))))
                                                            :enable (and (/= 0 (chordinfo (object (om-view-container self))))  ;; not auto (ignore chord)
                                                                        (/= 2 (chordmode (object (om-view-container self)))))  ;; not free   (no chord)
                                                            ))

                        (om-make-dialog-item 'om-static-text (om-make-point 345 195) 
                                             (om-make-point 160 60)
                                             "Auto"
                                             :fg-color *om-white-color*
                                             :font *targeteditor-font*)
                        (om-make-dialog-item 'om-static-text (om-make-point 345 210) 
                                             (om-make-point 240 60)
                                             "let Orchidee compute the search domain"
                                             :fg-color *om-white-color*
                                             :font *om-default-font1*)
                        (setf (orchichoice self) (om-make-dialog-item 'om-radio-button 
                                                                (om-make-point 320 195) 
                                                                (om-make-point 20 20)
                                                                ""
                                                                 :di-action (om-dialog-item-act item 
                                                                              (setf (chordinfo (object (om-view-container self))) 0)
                                                                              (setf (chordmode (object (om-view-container self))) 1)
                                                                              (om-enable-dialog-item (chordinfodi self) nil)
                                                                              (setf (selection? (chordp self)) nil)
                                                                              (enable-chord (chordp self) nil)
                                                                              )
                                                                :fg-color *om-white-color*
                                                                :font *targeteditor-font*
                                                                :checked-p (and (= (chordinfo (object (om-view-container self))) 0)     ;; auto (ignore chord)
                                                                                (/= 2 (chordmode (object (om-view-container self)))))   ;; not free  (no chord)
                                                                ))    
                        
                        
                        (om-make-dialog-item 'om-static-text (om-make-point 345 230) 
                                             (om-make-point 160 60)
                                             "Free"
                                             :fg-color *om-white-color*
                                             :font *targeteditor-font*)
                        (om-make-dialog-item 'om-static-text (om-make-point 345 245) 
                                             (om-make-point 240 60)
                                             "no constraints on the search domain"
                                             :fg-color *om-white-color*
                                             :font *om-default-font1*)
                        (setf (orchichoice self) (om-make-dialog-item 'om-radio-button 
                                                                (om-make-point 320 230) 
                                                                (om-make-point 20 20)
                                                                ""
                                                                 :di-action (om-dialog-item-act item 
                                                                              (setf (chordmode (object (om-view-container self))) 2)
                                                                              (setf (chordinfo (object (om-view-container self))) 0)
                                                                              (om-enable-dialog-item (chordinfodi self) nil)
                                                                              (setf (selection? (chordp self)) nil)
                                                                              (enable-chord (chordp self) nil)
                                                                              )
                                                                :fg-color *om-white-color*
                                                                :font *targeteditor-font*
                                                                :checked-p (= (chordmode (object (om-view-container self))) 2)))    ;;  free (no chord)

                    ;(setf (sendb self) (om-make-dialog-item 'om-button (om-make-point 420 250) (om-make-point 200 20)
                    ;                                        (format nil "Check Filter Data")
                    ;                                         :di-action (om-dialog-item-act item 
                    ;                                                      (unless (snd (object (om-view-container self))) 
                    ;                                                        (synthesize-sound (om-view-container self)))
                    ;                                                      ;(print (list (targetchord (object (om-view-container self))) 
                    ;                                                      ;             (object (chordp self))))
                    ;                                                      (let ((rep (submit-target (object (om-view-container self)))))
                    ;                                                        (if rep
                    ;                                                            (update-target-info rep (om-view-container self))
                    ;                                                          (om-beep-msg "No results received from target analysis...")
                    ;                                                          ))
                    ;                                                      )
                    ;                                         ))
                    )

   (if (= 0 (chordinfo (object (om-view-container self)))) (enable-chord (chordp self) nil))
   
   ))

;(remove '(2 3 4) '(1 2 3 4 5 6 7 8) :test #'(lambda (list elt) (member elt list)))

(defun update-target-info (rep editor)

  (let ((info (parse-analyse-results rep)))
    ;(print (lmidic (targetchord (object editor))))
    ;(print (nth 0 info))
    ;(print (nth 1 info))
    ;(print (nth 2 info))
    (case (chordinfo (object editor))
      (0 ; ACCEPT SOLUTIONS (AUTO)
       (setf (inside (targetchord (object editor)))
             (mapcar #'(lambda (m) (make-instance 'note :midic m))
                     (sort (car info) '<)))
       (enable-chord (chordp (targetp editor)) t)
       (update-panel (chordp (targetp editor)))
       )
      (otherwise 
       (when (and (or (nth 1 info) (nth 2 info))
                  (om-y-or-n-dialog (string+ (format nil "Orchidee suggested the following modification to the search domain:~%~%")
                                   (if (nth 1 info) (format nil "Missing pitches: ~a~%~%" (mapcar 'mc->orchidee (nth 1 info))) "")
                                   (if (nth 2 info) (format nil "Useless pitches: ~a~%~%" (mapcar 'mc->orchidee (nth 2 info))) "")
                                   "Include this in the target?")))
    
         (setf (inside (targetchord (object editor)))
               (mapcar #'(lambda (m) (make-instance 'note :midic m))
                        (sort (append (remove (nth 2 info)
                                             (lmidic (targetchord (object editor)))
                                             :test #'(lambda (list elt) (member elt list)))
                                     (nth 1 info)) '<)))
         (enable-chord (chordp (targetp editor)) t)
         (update-panel (chordp (targetp editor)))
         )))))                           

(defmethod om-draw-contents :after ((self targetdataeditor))
  (om-with-focused-view self
    (om-with-fg-color self *om-white-color*
      (om-draw-line 440 98 470 98)
      (om-draw-line 440 128 470 128)
      (om-draw-line 470 98 470 152)
      (when (= (display-mode (om-view-container self)) 1) 
        (om-with-line-size 4
          (om-draw-line 2 80 18 80)))
      )))


;;;========
;;; SOUND
;;;========

(defmethod om-draw-contents ((self targetsoundPanel))
  (call-next-method)
  (om-with-focused-view self
    ;(om-with-fg-color self *om-white-color*
    ;  (om-fill-rect 20 20 (- (w self) 70) (- (h self) 40)))
    (let ((thesound (snd (object (om-view-container (om-view-container self))))))
      (when thesound
        (let ((dur (if (om-sound-sample-rate thesound)
                       (/ (om-sound-n-samples thesound) (om-sound-sample-rate thesound))
                     0))
              (thepicture (sound-get-pict thesound)))
          (when thepicture
            (om-with-fg-color self *om-dark-gray-color*
              (om-draw-picture self thepicture
                               (om-make-point 0 0) (om-make-point (w self) (h self)))
              ))
          (om-with-fg-color self (om-make-color 1 0.2 0.2) ;*om-red-color*
            (loop for item in (markers thesound)
                  for k = 0 then (+ k 1) do
                  (om-with-pen (self :size (if (member k (selection? self)) 3 1)) 
                    (om-draw-line (round (* (w self) item) dur) 0 (round (* (w self) item) dur) (h self)))))
          ))
      )))

