 (in-package #:clasp)


;;
;; Devices definition part
;;

; Source class definition
(defclass class-diode-simple2 (class-device)
  ((name  :accessor name
          :initform (error "Must supply a name of device.")
          :initarg :name)
          
   (node+ :accessor node+
          :initform (error "Must supply a positive node.")
          :initarg :node+)
          
   (node- :accessor node-
          :initform (error "Must supply a negative node.")
          :initarg :node-)))

;(defmethod test ((m class-diode) k) 
;  #'(lambda () (+ k 5 (n m)))) 


(defmethod diode-simple2-current ((diode class-diode-simple2) v+ v-)
  #'(lambda ()
      (COND
        ((< (- (eval v+) (eval v-)) 0) 0)
        (T (* 10e-12 (- (exp (/ (- (eval v+) (eval v-)) 26e-3))  1))))))

      
(defmethod diode-simple2-current-dv+ ((diode class-diode-simple2) v+ v-)
  #'(lambda () 
      (COND
        ((< (- (eval v+) (eval v-)) 0) 0)
        (t (* (exp (/ (- (eval v+) (eval v-)) 26e-3))  (/ 10e-12  26e-3)))))) 
   
(defmethod diode-simple2-current-dv- ((diode class-diode-simple2) v+ v-)
  #'(lambda ()      
      (COND
        ((< (- (eval v+) (eval v-)) 0) 0)
        (t (* (exp (/ (- (eval v+) (eval v-)) 26e-3))  (/ -10e-12  26e-3) ))))) 
       
          
   



  

(defmethod map-device ((d class-diode-simple2) (m class-matrix-system))
     (print "mapping diode simple")
  (let ((v+       (make-var-node 'v (node+ d)))
        (v-       (make-var-node 'v (node- d)))
        (i        (make-var-name 'i (name  d))))

    
        (set-g-value m v+ i #'+  1)
        (set-g-value m v- i #'+ -1)        
        (set-g-value m i  i #'+ -1)
        
      ;  (set-rhs-value m i #'+ (diode-current d))    ;rhska je v tomhle zapisu nula
        (set-equations-value m i (diode-simple2-current d v+ v-)) 

        (set-d-value m i  v+  (diode-simple2-current-dv+ d v+ v-))        
        (set-d-value m i  v-  (diode-simple2-current-dv- d v+ v-))
        
      ;factory starting predict   
 
        (set-rhsl-start-value m i #'+ 0.05d0)
        
         ))
        
        
 
  ;; apply car a  cdr a       
 ;;(setf a `(,f 1 2))
 
; Function for easy source definition
; (e name node1 node2 value)
; example:
; (d "d1" 1 0)   
(defun ds2 (name node+ node- )
  (net-insert-device 
;(setf a
    (make-instance 'class-diode-simple2
                :name name
                :node+ node+
                :node- node-)
               
      name
      )
      )


