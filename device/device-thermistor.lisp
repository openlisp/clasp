 (in-package #:clasp)

;;
;; Devices definition part
;;

; Source class definition
(defclass class-thermistor (class-device)
  ((name  :accessor name
          :initform (error "Must supply a name of device.")
          :initarg :name)
          
   (node+ :accessor node+
          :initform (error "Must supply a positive node.")
          :initarg :node+)
          
   (node- :accessor node-
          :initform (error "Must supply a negative node.")
          :initarg :node-)

   (therm-a     
          :accessor therm-a
          :initarg :therm-a
          :initform 1.1292e-3)  ;TN05 ALFALIGHT DEVICE

   (therm-b     
          :accessor therm-b
          :initarg :therm-b
          :initform 2.3412e-4)

   (therm-c
          :accessor therm-c
          :initarg :therm-c
          :initform 8.7674e-8)

          
          ))
          
;Overall, the Steinhart-Hart equation has replaced the Beta equation as the most useful tool for interpolating the NTC thermistor R/T curve characteristic. The Steinhart-Hart equation is a third order polynomial which provides excellent curve fitting for specific temperature spans within the temperature range of -80 °C to 260 °therm-c. 

(defmethod thermistor-current ((thermistor class-thermistor) v+ v-)
  #'(lambda () 
    (/ 
      (-  (eval v+) (eval v-))  
      (exp
        (-
          (expt
            (-
              (expt  
                (+ 
                  (expt (/ (therm-b thermistor) (* 3 (therm-c thermistor))) 3)
                  (expt (/ (- (therm-a thermistor) (/ 1 *temperature*)) (* 16 (therm-c thermistor))) 2))
                1/2)
              (/ (- (therm-a thermistor) (/ 1 *temperature*)) (* 2 (therm-c thermistor))))
            1/3)   
          (expt
            (+
              (expt  
                (+ 
                  (expt (/ (therm-b thermistor) (* 3 (therm-c thermistor))) 3)
                  (expt (/ (- (therm-a thermistor) (/ 1 *temperature*)) (* 16 (therm-c thermistor))) 2))
                1/2)
              (/ (- (therm-a thermistor) (/ 1 *temperature*)) (* 2 (therm-c thermistor))))
            1/3))))))   
         
      
(defmethod thermistor-current-dv+ ((thermistor class-thermistor) v+ v-)
  #'(lambda () 
    (/ 1  
      (exp
        (-
          (expt
            (-
              (expt  
                (+ 
                  (expt (/ (therm-b thermistor) (* 3 (therm-c thermistor))) 3)
                  (expt (/ (- (therm-a thermistor) (/ 1 *temperature*)) (* 16 (therm-c thermistor))) 2))
                1/2)
              (/ (- (therm-a thermistor) (/ 1 *temperature*)) (* 2 (therm-c thermistor))))
            1/3)   
          (expt
            (+
              (expt  
                (+ 
                  (expt (/ (therm-b thermistor) (* 3 (therm-c thermistor))) 3)
                  (expt (/ (- (therm-a thermistor) (/ 1 *temperature*)) (* 16 (therm-c thermistor))) 2))
                1/2)
              (/ (- (therm-a thermistor) (/ 1 *temperature*)) (* 2 (therm-c thermistor))))
            1/3))))))   
    
    
(defmethod thermistor-current-dv- ((thermistor class-thermistor) v+ v-)
  #'(lambda () 
    (/ -1  
      (exp
        (-
          (expt
            (-
              (expt  
                (+ 
                  (expt (/ (therm-b thermistor) (* 3 (therm-c thermistor))) 3)
                  (expt (/ (- (therm-a thermistor) (/ 1 *temperature*)) (* 16 (therm-c thermistor))) 2))
                1/2)
              (/ (- (A thermistor) (/ 1 *temperature*)) (* 2 (therm-c thermistor))))
            1/3)   
          (expt
            (+
              (expt  
                (+ 
                  (expt (/ (therm-b thermistor) (* 3 (therm-c thermistor))) 3)
                  (expt (/ (- (therm-a thermistor) (/ 1 *temperature*)) (* 16 (therm-c thermistor))) 2))
                1/2)
              (/ (- (therm-a thermistor) (/ 1 *temperature*)) (* 2 (therm-c thermistor))))
            1/3))))))   

; funkce pro vyhodnoceni pole 
  ;(apply #'+ (mapcar #'funcall (list (test 3 3) (test 3 2))))
  
  
  
  
;(setf f #'(lambda (n+ n-) 
; ; (cond ((> n+ 10) 
;           (* (+ 1 (exp (+ n+ n-))))
;         ((< n+ 10)  
;            (- 1 (exp (- n+ n-))))
;         (t n+))))   
        
          

;    vb (constant-symbol inf) V

;    cjo   0 F
;    EG 1.11
     ;sirka zakazaneho pasu Band gam voltage 
;    Fc 0.5
    ;Koeficient pro vypocet barierove kapacity v pmem smeru
;    IBV 10^-10
    ;zpetny proud pri pruraznem napeti
;    IBVL 0 
    ;Zpetny proud pri pruraznem napeti pro male proudy    
;    IKF (constant-symbol inf)
    ;Prou pri ohybyu charakteristiky
;    IS 10 ^-14
    ;saturacni proud
    ;ISR 0
    ;rekombinacni saturacni proud 
    ;KF 0
    ;koeficient blikaveho sumu      
    ;M 0
    ;Exponent bariarove kapacity
    
   
;   ( VT : (/ (*k T) q))     
   ;VA = (- VB  (* VT  (log (+ 1 (* n  IB (/ 1 IS ))))))       
;          ))     
;    i1 = (* IS (- (exp (/ (- VC VA) n VT)) 1 ))       
          
          
          



  

(defmethod map-device ((d class-thermistor) (m class-matrix-system))
    ; (print "mapping diode")
  (let ((v+       (make-var-node 'v (node+ d)))
        (v-       (make-var-node 'v (node- d)))
        (i        (make-var-name 'i (name  d))))
        ;(temp    '(symbol-par t))
        ;(boltz   '(symbol-par b))
        ;(echarge '(symbol-par e))
        ;(va      `(symbol-var v ,(node+ d)))
        ;(vc      `(symbol-var v ,(node- d))))
;G matrix
    
        (set-g-value m v+ i #'+  1)
        (set-g-value m v- i #'+ -1)        
        (set-g-value m i  i #'+ -1)
        
      ;  (set-rhs-value m i #'+ (diode-current d))    ;rhska je v tomhle zapisu nula
        (set-equations-value m i (thermistor-current d v+ v-)) 

        (set-d-value m i  v+  (thermistor-current-dv+ d v+ v-))        
        (set-d-value m i  v-  (thermistor-current-dv- d v+ v-))))
        
        
        
 
  ;; apply car a  cdr a       
 ;;(setf a `(,f 1 2))
 
; Function for easy source definition
; (e name node1 node2 value)
; example:
; (d "d1" 1 0)   
(defun rth (name node+ node-); value)
  (net-insert-device 
;(setf a
    (make-instance 'class-thermistor
                :name name
                :node+ node+
                :node- node-
                ;:value value
                )
               
      name
      )
      )


