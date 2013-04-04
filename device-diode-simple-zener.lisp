 (in-package #:clasp)

;;
;; Devices definition part
;;

; Source class definition
(defclass class-diode-simple-zener (class-device)
  ((name  :accessor name
          :initform (error "Must supply a name of device.")
          :initarg :name)
          
   (node+ :accessor node+
          :initform (error "Must supply a positive node.")
          :initarg :node+)
          
   (node- :accessor node-
          :initform (error "Must supply a negative node.")
          :initarg :node-)

   (IS    :accessor IS          ;saturacni proud
          :initarg :IS
          :initform 10e-12)

   (BV    :accessor BV              ;Break down voltage 
          :initarg :BV
          :initform 10)))
          
         
;(defmethod test ((m class-diode) k) 
;  #'(lambda () (+ k 5 (n m)))) 


(defmethod diode-simple-zener-current ((diode class-diode-simple-zener) v+ v-)
  #'(lambda ()
      (COND 
        ((< (- (eval v+) (eval v-)) (- (BV diode))) 
          (* (- (IS diode)) (- (exp (/ (+ (- (eval v+) (eval v-)) (BV diode)) -26e-3))  (exp (/  (- (eval v+) (eval v-))  26e-3))   )))
        (T 
          (* (IS diode) (- (exp (/ (- (eval v+) (eval v-)) 26e-3))  1))))))
       
      
(defmethod diode-simple-zener-current-dv+ ((diode class-diode-simple-zener) v+ v-)
  #'(lambda () 
      (COND
        ((< (- (eval v+) (eval v-)) (- (BV diode)))
          (* 
            (/ (IS diode) 26e-3)  
            (+ 
              (exp (/ (+ (- (eval v+) (eval v-)) (BV diode)) -26e-3))  
              (exp (/  (- (eval v+) (eval v-))  26e-3)))))
        (t (* (exp (/ (- (eval v+) (eval v-)) 26e-3))  (/ (IS diode)  26e-3)))))) 
   
(defmethod diode-simple-zener-current-dv- ((diode class-diode-simple-zener) v+ v-)
  #'(lambda ()      
      (COND
        ((< (- (eval v+) (eval v-)) (- (BV diode)))
          (* 
            (/ (IS diode) -26e-3)  
            (+ 
              (exp (/ (+ (- (eval v+) (eval v-)) (BV diode)) -26e-3))  
              (exp (/  (- (eval v+) (eval v-))  26e-3)))))
        (t (* (exp (/ (- (eval v+) (eval v-)) 26e-3))  (/ (IS diode)  26e-3) ))))) 
       
          
   



  

(defmethod map-device ((d class-diode-simple-zener) (m class-matrix-system))
     (print "mapping simple zener diode")
  (let ((v+       (make-var-node 'v (node+ d)))
        (v-       (make-var-node 'v (node- d)))
        (i        (make-var-name 'i (name  d))))

    
        (set-g-value m v+ i #'+  1)
        (set-g-value m v- i #'+ -1)        
        (set-g-value m i  i #'+ -1)
        
      ;  (set-rhs-value m i #'+ (diode-current d))    ;rhska je v tomhle zapisu nula
        (set-equations-value m i (diode-simple-zener-current d v+ v-)) 

        (set-d-value m i  v+  (diode-simple-zener-current-dv+ d v+ v-))        
        (set-d-value m i  v-  (diode-simple-zener-current-dv- d v+ v-))
        
      ;factory starting predict   
 
        (set-rhsl-start-value m i #'+ 0.05d0)
        
         ))
        
        
 
  ;; apply car a  cdr a       
 ;;(setf a `(,f 1 2))
 
; Function for easy source definition
; (e name node1 node2 value)
; example:
; (d "d1" 1 0)   
(defun dsz (name node+ node- is)
  (net-insert-device 
;(setf a
    (make-instance 'class-diode-simple-zener
                :name name
                :node+ node+
                :node- node-
                :is is)
               
      name
      )
      )


