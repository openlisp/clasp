 (in-package #:clasp)


; Capacitor class definition
(defclass class-capacitor (class-device)
  ((name  :initarg :name
          :initform (error "Must supply a name of device e.g. R1, C1, D1, ...")
          :accessor name)
   (node+ :accessor node+
          :initform 1
          :initarg :node+)
   (node- :accessor node-
          :initform 2
          :initarg :node-)
   (value :accessor value
          :initform 100
          :initarg :value)))
          
          
          
;(defun test-device-map-c ()
;  (map-dc
;    (make-instance 'class-capacitor 
;                :name "name"
;                :node- 1
;                :node 2
;                :value "value")))           
                
                
                
;this method will be called only if it get class resistor and class dc analysis
; y  | A       | voltage-var |   | rhs-current |
;---------- *  |-------------| = |-------------|    
; YA | Z       | current-var |   | rhs-voltage |             
;inductor is shorcut in DC analysis
(defmethod map-device ((d class-capacitor) (m class-matrix-system))
  (let ((n+       (make-var-node  'v (node+ d)))
        (n-       (make-var-node  'v (node- d)))
        (q        (make-var-name  'q (name  d)))
        (c    (value d)))
        
        

;G matrix
        (set-g-value m  q q #'-   1)
;E matrix
        (set-e-value m n+ q #'+   1)
        (set-e-value m n- q #'-   1)
;Z matrix        
        (set-z-value m q n+ #'+   c)
        (set-z-value m q n- #'-   c)))

        
                        
                
                    

; Function for easy capacitor definition
; (c name node1 node2 value)
; example:
; (C "C1" 1 2 100)
(defun C (name node+ node- value)
  (print "Device Model - Ideal Capacitor")
  (net-insert-device 
    (make-instance 'class-capacitor 
                :name name
                :node+ node+
                :node- node-
                :value value)
     name))
                

