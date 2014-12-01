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
(defmethod map-device ((d class-capacitor) (m matrice-system))
  (let ((n+       (make-var-node  'v (node+ d)))
        (n-       (make-var-node  'v (node- d)))
        (q        (make-var-name  'q (name  d)))
        (c    (value d)))
        
        


                       
    (setf (g-number-array q q  m)  -1)


    (setf (e-number-array n+ q  m) 1)
    (setf (e-number-array n- q  m) -1)



    (setf (z-number-array q n+  m) c)
    (setf (z-number-array q n-  m) (- c))
                

))
                    

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
                

