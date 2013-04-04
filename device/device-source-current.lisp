 (in-package #:clasp)

;;
;; Devices definition part
;;

; Source class definition
(defclass class-source-current (class-device)
  ((name  :accessor name
          :initarg :name)
          
   (node+ :accessor node+
          :initarg :node+)
          
   (node- :accessor node-
          :initarg :node-)
          
   (current :accessor current
            :initarg :current)))     
          
          
; y  | A       | voltage-var |   | rhs-current |
;---------- *  |-------------| = |-------------|    
; YA | Z       | current-var |   | rhs-voltage |           

        
(defmethod map-device ((d class-source-current) (m class-matrix-system))
  (let ((n+       (make-var-node 'v (node+ d)))
        (n-       (make-var-node 'v (node- d)))
        (i        (current d)))
                
        (set-rhs-number-value m n+  #'- i)    
        (set-rhs-number-value m n-  #'+ i)))

        
;pROBABLU NOT NEEDED  
;  (print "source")          
;        (set-voltage-var-vector analysis n+)
;          (print "source 2")  
;        (set-voltage-var-vector analysis n-)            
;        (set-current-var-vector analysis new-index)  
;        (set-current-var-vector analysis new-index)  

 ; (print "source last")  
          
; ))
 
 
; Function for easy source definition
; (e name node1 node2 value)
; example:
; (e "e1" 1 0 5)   
(defun j (name node+ node- current)
  (net-insert-device 
    (make-instance 'class-source-current 
                :name name
                :node+ node+
                :node- node-
                :current current)
     name))

