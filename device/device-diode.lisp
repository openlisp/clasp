 (in-package #:clasp)


;;
;; Devices definition part
;;

; Source class definition
(defclass class-diode (class-device)
  ((name  :accessor name
          :initform (error "Must supply a name of device.")
          :initarg :name)
          
   (node+ :accessor node+
          :initform (error "Must supply a positive node.")
          :initarg :node+)
          
   (node- :accessor node-
          :initform (error "Must supply a negative node.")
          :initarg :node-)
          
   (AF    :accessor AF     ;exponent blikaveho šumu
          :initarg :af
          :initform 1)

   (CJO   :accessor CJO         ;Průrazné napětí
          :initarg :CJO
          :initform 0)

   (is    :accessor IS          ;saturacni proud
          :initarg :IS
          :initform (expt 10 -14))

   (N     :accessor N              ;emmission koeficient 
          :initarg :N
          :initform 1)))



(defmethod diode-current ((diode class-diode) va vc)
  #'(lambda () 
    (* (is diode) (- (exp (/ (- (eval va) (eval vc)) (* (n diode) (/ (* +boltzman-constant+ *temperature*)  +electron-charge+)))) 1 )))) 

      
(defmethod diode-current-dva ((diode class-diode) va vc)
  #'(lambda () 
    (* (is diode) (exp (/ (- (eval va) (eval vc)) (* (n diode) (/ (* +boltzman-constant+ *temperature*)  +electron-charge+))))))) 

(defmethod diode-current-dvc ((diode class-diode) va vc)
  #'(lambda () 
    (* (is diode) (exp (/ (- (eval va) (eval vc)) (* (n diode) (/ (* +boltzman-constant+ *temperature*)  +electron-charge+)))) -1))) 

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
          
          
          



  

(defmethod map-device ((d class-diode) (m class-matrix-system))
  (print "mapping diode")
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
        (set-equations-value m i (diode-current d v+ v-)) 

        (set-d-value m i  v+  (diode-current-dva d v+ v-))        
        (set-d-value m i  v-  (diode-current-dvc d v+ v-))))
        
        
        
 
  ;; apply car a  cdr a       
 ;;(setf a `(,f 1 2))
 
; Function for easy source definition
; (e name node1 node2 value)
; example:
; (d "d1" 1 0)   
(defun d (name node+ node- )
  (net-insert-device 
;(setf a
    (make-instance 'class-diode
                :name name
                :node+ node+
                :node- node-)
               
      name
      )
      )


