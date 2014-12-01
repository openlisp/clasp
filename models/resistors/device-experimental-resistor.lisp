 (in-package #:clasp)

;;
;; Devices definition part
;;

; Resistor class definition
(defclass class-experimental-resistor (class-device)
  ((name  :initarg :name
          :accessor name)
   (node+ :initarg :node+
          :accessor node+)
   (node- :initarg :node-
          :accessor node-)
   (value :initarg :value
          :accessor value)))     

;this method will be called only if it get class resistor and class dc analysis
;       | voltage-var |   | rhs-current |
;  G *  |-------------| = |-------------|    
;       | current-var |   | rhs-voltage |           
(defmethod  map-device ((d class-experimental-resistor) (m matrice-system))
  (let ((v+       (make-var-node 'v (node+ d)))
        (v-       (make-var-node 'v (node- d)))
        (g    (/ 1 (value d)))) ;; not used definition
;G matrix

(print "mapping resistor")

    (setf (g-equation-array v+ v+ m) g)              
    (setf (g-equation-array v+ v- m) (- g)) 
    (setf (g-equation-array v- v+ m) (- g)) 
    (setf (g-equation-array v- v- m) g) 
 

))


      
;(defun test-hello ()
;  (hello-world))      

;(defun test-device-map-r ()
;  (map-dc
;    (make-instance 'class-resistor 
;                :name "name"
;                :node1 1
;                :node2 2
;                :value "value")))

          
; Function for easy resitor definition
; r name node1 node2 value
; example:
; (R "R1" 1 2 100) 
(defun rex (name node+ node- value)
  (net-insert-device 
    (make-instance 'class-experimental-resistor 
                :name name
                :node+ node+
                :node- node-
                :value value)
     name))
     
     

