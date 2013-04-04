 (in-package #:clasp)


; Capacitor class definition
(defclass class-capacitor (class-device)
  ((name  :initarg :name
          :accessor name)
   (node+ :accessor node+
          :initarg :node+)
   (node- :accessor node-
          :initarg :node-)
   (value :accessor value
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
  (print "wja")
  (net-insert-device 
    (make-instance 'class-capacitor 
                :name name
                :node+ node+
                :node- node-
                :value value)
     name))
                

