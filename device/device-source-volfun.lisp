 (in-package #:clasp)

;;
;; Devices definition part
;;

; Source class definition
(defclass class-source-volfun (class-device)
  ((name  :accessor name
          :initform (error "Must supply a name of device.")
          :initarg :name)
          
   (node+ :accessor node+
          :initform (error "Must supply a positive node.")
          :initarg :node+)
          
   (node- :accessor node-
          :initform (error "Must supply a negative node.")
          :initarg :node-)
          
   
;   (start  :accessor start
;           :initform (error "Must supply a negative node.")
;           :initarg :start)
          
   
;   (stop   :accessor stop
;           :initform (error "Must supply a negative node.")
;           :initarg :stop)       
           
;   (repeat   :accessor repeat
;             :initform (error "Must supply a negative node.")
;             :initarg :repeat) 
             
   (volfun   :accessor volfun
             :initform (error "Must supply a negative node.")
             :initarg :volfun)))                                   
          
;   (voltage :accessor voltage
;            :initform (error "Must supply a source voltage.")
;            :initarg :voltage)))     
          

  (defmethod voltage-source-polarity+ (v+ v-)
  #'(lambda ()      
      (COND
        ((< (- (eval v+) (eval v-)) 0) 1)
        (T -1)))) 


  (defmethod voltage-source-polarity- (v+ v-)
  #'(lambda ()      
      (COND
        ((< (- (eval v+) (eval v-)) 0) -1)
        (T 1)))) 
       
          
; y  | A       | voltage-var |   | rhs-current |
;---------- *  |-------------| = |-------------|    
; YA | Z       | current-var |   | rhs-voltage |           
(defmethod map-device ((d class-source-volfun) (m class-matrix-system))
  (let ((v+       (make-var-node 'v (node+ d)))
        (v-       (make-var-node 'v (node- d)))
        (i        (make-var-name 'i (name  d)))
        (volfun   (volfun d)))
 (print "voltage g matrix")
;G matrix

    

       ; (set-g-value m i  v+  #'+  1)
       ; (set-g-value m i  v-  #'-  1)
        (set-g-value m v+ i   #'+  1)
        (set-g-value m v- i   #'-  1)


        (set-d-value m i  v+  (voltage-source-polarity+ v+ v-))        
        (set-d-value m i  v-  (voltage-source-polarity- v+ v-))

;; here must be orientation misteko othervise I dont know
;; Documentation and code has differend deffinition
       ; (set-g-value m i  v+  #'+  1)
      ;  (set-g-value m i  v-  #'-  1)
      ;  (set-g-value m v+ i   #'+  1)
      ;  (set-g-value m v- i   #'-  1)


;RHS        
;(print "voltage rhs vector")
        (set-rhs-equations-value m i volfun)))
 
 
; Function for easy source definition
; (e name node1 node2 value)
; example:
; (e "e1" 1 0 5)   
(defun ef (name node+ node- volfun)
  (net-insert-device 
    (make-instance 'class-source-volfun 
                :name name
                :node+ node+
                :node- node-
                :volfun volfun)
     name))

