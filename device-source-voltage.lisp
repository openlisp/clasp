 (in-package #:clasp)

;;
;; Devices definition part
;;

; Source class definition
(defclass class-source-voltage (class-device)
  ((name  :accessor name
          :initform (error "Must supply a name of device.")
          :initarg :name)
          
   (node+ :accessor node+
          :initform (error "Must supply a positive node.")
          :initarg :node+)
          
   (node- :accessor node-
          :initform (error "Must supply a negative node.")
          :initarg :node-)
          
   (voltage :accessor voltage
            :initform (error "Must supply a source voltage.")
            :initarg :voltage)))     
          
          
; y  | A       | voltage-var |   | rhs-current |
;---------- *  |-------------| = |-------------|    
; YA | Z       | current-var |   | rhs-voltage |           
(defmethod map-device ((d class-source-voltage) (m class-matrix-system))
  (let ((v+       (make-var-node 'v (node+ d)))
        (v-       (make-var-node 'v (node- d)))
        (i        (make-var-name 'i (name  d)))
        (vol    (voltage d)))
 (print "voltage g matrix")
;G matrix
        (set-g-value m i  v+  #'+  1)
        (set-g-value m i  v-  #'-  1)
        (set-g-value m v+ i   #'+  1)
        (set-g-value m v- i   #'-  1)
;RHS        
(print "voltage rhs vector")

        (set-rhs-number-value m i #'+ vol)))
 
 
; Function for easy source definition
; (e name node1 node2 value)
; example:
; (e "e1" 1 0 5)   
(defun e (name node+ node- voltage)
  (net-insert-device 
    (make-instance 'class-source-voltage 
                :name name
                :node+ node+
                :node- node-
                :voltage voltage)
     name))

