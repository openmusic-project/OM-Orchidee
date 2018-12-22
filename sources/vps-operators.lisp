

(in-package :om)

(defmethod vps-op-defvals ((self (eql 'expand-spectre))) 
  (list 100))

(defmethod vps-op-keyword ((self (eql 'expand-spectre))) :expand-spectre)


;;; PARTIAL AMPS

(defmethod set-amplitudes ((self cr::fql) (amplitude-bpf bpf))
  (let ((n (length (get-vps-freqs self)))
        (amps (y-points amplitude-bpf)))
    (when (< (length amps) n)
      (setf amps (append amps (make-list (- n (length amps)) 
                                         :initial-element (car (last amps))))))
    (setf amps (first-n amps n))
    (initialize-instance self :the-list (get-vps-freqs self) :amplitudes amps)))

(defmethod vps-op-defvals ((self (eql 'set-amplitudes))) 
  (list 
   (simple-bpf-from-list '(0 1) '(1 1) 'bpf 4)))

(defmethod vps-op-keyword ((self (eql 'set-amplitudes))) :set-amplitudes)

;;; STRETCH

(defmethod! stretch-vps-freqs ((self cr::vps) val)
  (let ((freqs (get-vps-freqs self)))
    (initialize-instance self :the-list (om+ (car freqs) 
                                             (om* (/ val 100) (om- freqs (car freqs)))))
    self))

(defmethod vps-op-keyword ((self (eql 'stretch-vps-freqs))) :stretch-fact)
(defmethod vps-op-defvals ((self (eql 'stretch-vps-freqs))) '(100))
(defmethod vps-op-name ((self (eql 'stretch-vps-freqs))) "Stretch")
(defmethod vps-op-ranges ((self (eql 'stretch-vps-freqs))) '((0 200)))


;;; BAND-FILTER

(defmethod! filter-vps-freqs ((self cr::vps) low high)
  (let ((freqs (get-vps-freqs self))
        (amps (get-vps-amps self))
        fil)
    (setf fil (loop for f in freqs 
                    for a in amps 
                    when (and (>= f low) (<= f high))
                    collect (list f a)))
    (initialize-instance self :the-list (mapcar 'car fil) :amplitudes (mapcar 'cadr fil))
    self))

(defmethod vps-op-keyword ((self (eql 'filter-vps-freqs))) :filter)
(defmethod vps-op-defvals ((self (eql 'filter-vps-freqs))) '(0 20000))
(defmethod vps-op-name ((self (eql 'filter-vps-freqs))) "Band Filter")
(defmethod vps-op-ranges ((self (eql 'filter-vps-freqs))) '((0 20000) (0 20000)))


