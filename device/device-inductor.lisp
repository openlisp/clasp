 (in-package #:clasp)


                   
; Inductor class definition
(defclass class-inductor (class-device)
  ((name  :initarg :name
          :accessor name)
   (node+ :initarg :node+
          :accessor node+)
   (node- :initarg :node-
          :accessor node-)
   (value :initarg :value
          :accessor value)))   
          
          
          
          
;this method will be called only if it get class resistor and class dc analysis
; y  | A       | voltage-var |   | rhs-current |
;---------- *  |-------------| = |-------------|    
; YA | Z       | current-var |   | rhs-voltage |             
;inductor is shorcut in DC analysis
(defmethod map-device ((d class-inductor) (m class-matrix-system))
  (let ((n+       (make-var-node 'v (node+ d)))
        (n-       (make-var-node 'v (node- d)))
        (i        (make-var-name 'i (name  d)))
        (l    (value d)))
        
          
        
;G matrix
        (set-g-value m  i n+ #'+  1)
        (set-g-value m  i n- #'+ -1)
;E matrix
        (set-e-value m n+ i  #'+  1)
        (set-e-value m n- i  #'+ -1)
;Z matrix        
        (set-z-value m i i   #'-  l)))
          
          
            
          
; Function for easy inductor definition
; (L name node1 node2 value)
; example:
; (L "L1" 1 2 100)   
(defun l (name node+ node- value)
  (net-insert-device 
    (make-instance 'class-inductor 
                :name name
                :node+ node+
                :node- node-
                :value value)
     name))
                   


