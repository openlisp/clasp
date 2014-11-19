 (in-package #:clasp)

;;
;; Devices definition part
;;

; Resistor class definition
(defclass class-resistor-fun (class-device)
  ((name  :initarg :name
          :accessor name)
   (node+ :initarg :node+
          :accessor node+)
   (node- :initarg :node-
          :accessor node-)
   (rfun :initarg :rfun
          :accessor rfun)))     


;rezistor neni nelinearne. Jen se musi hodnota resistance vypocitat evaluaci funkce
(defmethod resistor-fun-current ((resistor class-resistor-fun) v+ v-)
  #'(lambda () 
    (/ (-  (eval v+) (eval v-))  (funcall (rfun resistor)))))
    
(defmethod resistor-fun-current-dv+ ((resistor class-resistor-fun) v+ v-)
  #'(lambda () 
    (/ 1  (funcall (rfun resistor)))))
    
(defmethod resistor-fun-current-dv- ((resistor class-resistor-fun) v+ v-)
  #'(lambda () 
        (/ -1  (funcall (rfun resistor)))))    
      


;this method will be called only if it get class resistor and class dc analysis
;       | voltage-var |   | rhs-current |
;  G *  |-------------| = |-------------|    
;       | current-var |   | rhs-voltage |           
(defmethod  map-device ((d class-resistor-fun) (m matrice-system))
  (let ((v+       (make-var-node 'v (node+ d)))
        (v-       (make-var-node 'v (node- d)))
        (i        (make-var-name 'i (name  d))))
;        (rfun    (rfun d))) ;; not used definition
;G matrix
        (set-g-value m v+ i #'+  1)
        (set-g-value m v- i #'+ -1)        
        (set-g-value m i  i #'+ -1)        

        (set-equations-value m i (resistor-fun-current     d v+ v-)) 
        (set-d-value m i  v+     (resistor-fun-current-dv+ d v+ v-))        
        (set-d-value m i  v-     (resistor-fun-current-dv- d v+ v-))))


          
; Function for easy resitor definition
; r name node1 node2 value
; example:
; (R "R1" 1 2 100) 
(defun RF (name node+ node- rfun)
  (net-insert-device 
    (make-instance 'class-resistor-fun
                :name name
                :node+ node+
                :node- node-
                :rfun rfun)
     name))
     
     

