;;; 
;;; OpenMusic Orchestration Tool
;;; Orchestrator process
;;;

(in-package :om)



;(setf test-list '(6000 6020 6040  6050  6060  6080  6100  6120  6140   6150   6160   6180 6200))
;               '("C3" "C3" "C3+" "C3+" "C3+" "C#3" "C#3" "C#3" "C#3+" "C#3+" "C#3+" "D3" "D3")
;(mapcar 'mc->orchidee test-list)
; (mc->orchidee 6180)

(defparameter *orchidee-note-scale*
      '(("C") ("C" . 1.8) ("C" . 1.4) ("C" . 3.8)
        ("C#") ("C#" . 1.8) ("C#" . 1.4) ("C#" . 3.8)
        ("D") ("D" . 1.8) ("D" . 1.4) ("D" . 3.8)
        ("D#") ("D#" . 1.8) ("D#" . 1.4) ("D#" . 3.8)
        ("E") ("E" . 1.8) ("E" . 1.4) ("E" . 3.8)
        ("F") ("F" . 1.8) ("F" . 1.4) ("F" . 3.8)
        ("F#") ("F#" . 1.8) ("F#" . 1.4) ("F#" . 3.8)
        ("G") ("G" . 1.8) ("G" . 1.4) ("G" . 3.8)
        ("G#") ("G#" . 1.8) ("G#" . 1.4) ("G#" . 3.8)
        ("A") ("A" . 1.8) ("A" . 1.4) ("A" . 3.8)
        ("A#") ("A#" . 1.8) ("A#" . 1.4) ("A#" . 3.8)
        ("B") ("B" . 1.8) ("B" . 1.4) ("B" . 3.8)))

(defparameter *orchidee-note-alterations*
  '((:s "#" +100) (:q "" +50) (:qs "#" +150)))

(defun mc->orchidee (midic)
  (multiple-value-bind (1/8tone-midic cents) (round midic 25)
    (multiple-value-bind (oct+2 midic<1200) (floor (* 1/8tone-midic 25) 1200)
      (let ((note (nth (round midic<1200 25) *orchidee-note-scale*)))
        (format nil "~A~A~A~A"
                (car note) 
                (- oct+2 1) 
                (if (cdr note) "+" "")
                (if (cdr note) (cdr note) "")
                )
        ))))

; (orchidee->mc "B4+")

(defun orchidee->mc (str)
  (let ((tmpstr str)
        (note (some  #'(lambda (note)
                        (when (and (null (cdr note))
                                   (eql 0 (search (car note) str :test #'string-equal)))
                          note)) *orchidee-note-scale*))
        midic)
    (setq midic (* (position note *orchidee-note-scale*)
                   (/ 1200 (length *orchidee-note-scale*))))
     ;; at this point: "C" -> 0 ; "D" -> 100 ; "E" -> 200 ; etc.
     ;; alteration
     (setq tmpstr (subseq tmpstr 1))
     (when (string-equal "#" (subseq tmpstr 0 1))
       (incf midic 100)
       (setq tmpstr (subseq tmpstr 1)))
     ;; octave
     (multiple-value-bind (oct i) (parse-integer tmpstr :start 0 :junk-allowed t)
       (when oct 
         (incf midic (* (+ oct 1) 1200))
         (setq tmpstr (subseq tmpstr i))))
     (when (and (> (length tmpstr) 0) (string-equal "+" (subseq tmpstr 0 1)))
       (setq tmpstr (subseq tmpstr 1))
       (multiple-value-bind (num den) (string-until-char tmpstr ".")
         (incf midic (* (read-from-string num) (/ 200 (read-from-string den))))))
     midic))
     


; (orchidee-msg t "/targetparameters" "/gettargetparameters" 999)


(defmethod! submit-target ((target soundtarget))
   :icon 899
  ;(print (integer-to-string (nth 1 (analysisparams target)))))
  (unless (snd target) (synthesize-sound target))
  (unless *orchidee-app-process* 
    (run-orchidee))
  (when *orchidee-app-process*
    (if (check-orchidee)
        (let* ((sf (om-sound-file-name (snd target)))
               (beg 500) (end 1000)
               (f0 (nth 0 (analysisparams target))) 
               (nbpart (nth 1 (analysisparams target))) 
               (delta (if (numberp (nth 2 (analysisparams target))) (nth 2 (analysisparams target)) 0.015))
               ;(notes (mapcar #'(lambda (n) (if (ratiop n) (float n) n)) (om/ (om-round (om* (lmidic (chord target)) 100) 0 100) 2)))
               (notes (mapcar 'mc->orchidee (lmidic (targetchord target))))
               msgrep error)
          (setf msgrep (orchidee-msg t "/ready" "/settargetparameters" 999 
                                     "fmin" f0
                                     "npartials" nbpart
                                     "delta" delta
                                     "autodelta" (if (numberp (nth 2 (analysisparams target))) 0 1)))
        
          (setf error (or error (and (stringp (car msgrep)) (string-equal (car msgrep) "/error"))))

          (setf msgrep (orchidee-msg t "/ready" "/setsoundfile" 996 (namestring sf)))
          (setf error (or error (and (stringp (car msgrep)) (string-equal (car msgrep) "/error"))))
          
          (if (= (chordmode target) 2) 
              (setf msgrep (apply 'orchidee-msg (list t "/ready" "/setfilter" 994 "note" "free")))
            (case (chordinfo target)
              (0 (setf msgrep (apply 'orchidee-msg (list t "/ready" "/setfilter" 994 "note" "auto"))))
              (1 (setf msgrep (apply 'orchidee-msg (append (list t "/ready" "/setfilter" 994 "note" "include") notes))))
              (2 (setf msgrep (apply 'orchidee-msg (append (list t "/ready" "/setfilter" 994 "note" "force") notes))))
              ))
          (setf error (or error (and (stringp (car msgrep)) (string-equal (car msgrep) "/error"))))
          
         ; (if error
         ;     (setf msgrep nil)
         ;   (setf msgrep (orchidee-msg t "/filter" "/analyze" 991)))
          msgrep)

      (om-beep-msg "ORCHIDEE server not responding..."))))

(defun parse-analyse-results (orchidee-msg)
  (when (and (string-equal (nth 0 orchidee-msg) "/filter")
              (string-equal (nth 2 orchidee-msg) "note"))
    (flet ((orcmsg-key-eq (x y) 
             (and (stringp x) (stringp y) (string-equal x y))))
      (let* ((pl (position "list" orchidee-msg :test #'orcmsg-key-eq))
             (pm (position "missing" orchidee-msg :test #'orcmsg-key-eq))
             (pu (position "useless" orchidee-msg :test #'orcmsg-key-eq))
             (endl (or (and pm pu (min pm pu)) pm pu)))
        (list
         (om-round (om* (subseq orchidee-msg (+ pl 1) endl) 100))
         (when pm (om-round (om* (subseq orchidee-msg (+ pm 1) pu) 100)))
         (when pu (om-round (om* (subseq orchidee-msg (+ pu 1)) 100))))))))
       

; (parse-analyse-results '("/filter" 991 "note" "list" 28.0F0 52.0F0 "missing" 67.0F0 65 "useless" 35.0F0 44.0F0 65.0F0 75.0F0 30.0F0 46.0F0 53.0F0))
; (parse-analyse-results '("/filter" 991 "note" "list" 28.0F0 52.0F0))
; (parse-analyse-results '("/filter" 991 "note" "list" 28.0F0 52.0F0 "missing" 67.0F0))
; (parse-analyse-results '("/filter" 991 "note" "list" 28.0F0 52.0F0 "useless" 35.0F0 44.0F0 65.0F0 75.0F0 30.0F0 46.0F0 53.0F0))
          
              
;(orchidee-msg 5 "/ready" "/setresolution" 887 2)

(defmethod! submit-orchestra ((orc orchestra))
  :icon 899
  (unless (check-orchidee) (run-orchidee))
  (when *orchidee-app-process*
    (let* ((instrs (format-orchestra orc))
           (msgrep nil))

      (orchidee-msg 5 "/acknowledge" "/setresolution" 887 (round (microtones orc) 2))

      (setf msgrep (apply 'orchidee-msg (append (list t "/ready" "/setorchestra" 888) instrs)))
      
      msgrep)))
  
; (clear-orchidee)

(defmethod! research-criteria ()
  :icon 899
  (let ((rep (orchidee-msg 2 "/criterialist" "/getcriterialist" 777)))
    (when rep (cddr rep))))

(defmethod! orc-fields ()
  :icon 899
  (let ((rep (orchidee-msg 2 "/dbqueryfields" "/dbgetqueryfields" 777 "message")))
    (when rep (cddr rep))))

;(defmethod! filed-values ()
;  :icon 899
;  (let ((rep (orchidee-msg 2 "/dbgetfieldvaluelist" "/dbgetqueryfields" 777 "message")))
;    (when rep (cddr rep))))


(defclass! orc-filter ()
           ((attribute :accessor attribute :initarg :attribute :initform "note")
            (mode :accessor mode :initarg :mode :initform "free")
            (vals :accessor vals :initarg :vals :initform nil))
           (:icon 899))

(defmethod get-slot-in-out-names ((self orc-filter))
  (values '("self" "attribute" "mode" "values")
          '(nil "note" "free" nil)
          '("object" "attribute to filter" "filter mode (select from list)" "values")
          '(nil nil ((2 (("free" "free")
                        ("auto" "auto")
                        ("force" "force")
                        ("include" "incude")
                        ("exclude" "exclude"))))
                nil)))

(defmethod! orchestrate ((target t) (orc t) &key criteria dbnames filters)
  :icon 899
  :numouts 1
  (unless (check-orchidee) (run-orchidee))
  (when *orchidee-app-process*
    (let ((error nil))
      (when target 
        (setf error (not (submit-target target))))
      (unless error 
        (when orc 
          (setf error (not (submit-orchestra orc))))
        (unless error
          (when criteria
            (setf msgrep (apply 'orchidee-msg (append (list t "/ready" "/setcriteria" 666) criteria))))
          (when dbnames
            (setf msgrep (apply 'orchidee-msg (append (list t "/ready" "/setfilter" 994 "dbname" "force") dbnames))))
          (when filters
            (mapcar #'(lambda (filter)
                        (setf msgrep (apply 'orchidee-msg (append (list t "/ready" "/setfilter" 996 (attribute filter) (mode filter)) 
                                                                  (vals filter)))))
                    (list! filters)))
          (let ((solutionfile (tmpfile "orchidee.txt")))
            (when (probe-file solutionfile) (delete-file solutionfile)) 
            (setf msgrep (orchidee-msg 'progress-bar "/ready" "/orchestrate" 777))
            (when (and msgrep (not (string-equal "/error" (car msgrep))))
              (setf msgrep (orchidee-msg 'progress-bar "/ready" "/exportsolutions" 778 (namestring solutionfile)))
              (when msgrep 
                (if (probe-file solutionfile) solutionfile
                  (om-beep-msg (string+ "Error: solution file " (namestring solutionfile) " not found.."))))
              )))))))



