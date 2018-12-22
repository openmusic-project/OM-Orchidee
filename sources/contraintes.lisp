(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONTRAINTES
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod! orc-size ((num integer) &optional (rule "="))
  :icon 809
  :menuins '((1 (("=" "=") ("min" "min") ("max" "max"))))
  :initvals '(10 "=")
  (list "size" rule num))


(defmethod! num-notes ((num integer) note &optional (rule "="))
  :icon 809
  :menuins '((2 (("=" "=") ("min" "min") ("max" "max"))))
  :initvals '(1 A3 "=")
  (list "note" rule num (string note)))
