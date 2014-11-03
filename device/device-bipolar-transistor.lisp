 (in-package #:clasp)


; Bipolar Transistor class definition
(defclass class-bipolar-transistor (class-device)
  ((name  :initarg :name
          :initform (error "Must supply a name of device e.g. R1, C1, D1, ...")
          :accessor name)
   (node-e :accessor node-e
          :initform 1
          :initarg :node-e)
   (node-c :accessor node-c
          :initform 2
          :initarg :node-c)
   (node-b :accessor node-b
          :initform 3
          :initarg :node-b)          
   (alpha-reverse    
          :accessor alpha-reverse
          :initarg :alpha-reverse
          :initform 2.3412e-4)
   (alpha-forward
          :accessor alpha-forward
          :initarg :alpha-forward
          :initform 8.7674e-8)
   
   (vt    :accessor vt
          :initarg :vt
          :initform 26e-3) 
                 
        (is    :accessor is
          :initarg :is
          :initform 26e-3)
          
          
          ))
          



;Need to be defined


;IES base emittter  saturation current
;ICS base-collector saturation current

;VBE base-emmitter voltage
;VBC base-collector voltage

;ALPHAFORWARD large-signal forward current gains of the transistor int the common base conviguration
;ALPHAREVERSE large-signal reverse current gains of the transistor int the common base conviguration

;k - Boltzman constant
;T junction temperature in k
;e electron charge
;VT= kT/q



;----------------------------------------------------------------------------------

(defmethod bipolar-transistor-icc ((transistor class-bipolar-transistor) vb ve)
  #'(lambda () 
    (- (* (is transistor) (exp (/ (eval vb) (eval ve) (vt transistor) ))))))  ; 123 used to be vt it should be from database



(defmethod bipolar-transistor-ICC-dvb ((transistor class-bipolar-transistor) vb ve)
  #'(lambda () 
    (/ (* (is transistor) (exp (/ (- (eval vb) (eval ve)) (vt transistor) ))) (vt transistor) )))



(defmethod bipolar-transistor-ICC-dve ((transistor class-bipolar-transistor) vb ve)
  #'(lambda () 
    (/ (* (is transistor) (exp (/ (- (eval vb) (eval ve)) (vt transistor) )) -1 ) (vt transistor) )))


;------------------------------------------------------------------------------------


(defmethod bipolar-transistor-IEC ((transistor class-bipolar-transistor) vb vc)
  #'(lambda () 
    (- (* (is transistor) (exp (/ (eval vb) (eval vc) (vt transistor) )))  )))



(defmethod bipolar-transistor-IEC-dvb ((transistor class-bipolar-transistor) vb vc)
  #'(lambda () 
    (/ (* (is transistor) (exp (/ (- (eval vb) (eval vc)) (vt transistor) ))) (vt transistor) )))


(defmethod bipolar-transistor-IEC-dvc ((transistor class-bipolar-transistor) vb vc)
  #'(lambda () 
    (/ (* (is transistor) (exp (/ (- (eval vb) (eval vc)) (vt transistor) ))) (vt transistor) )))

          
       
                
                
                
;this method will be called only if it get class resistor and class dc analysis
; y  | A       | voltage-var |   | rhs-current |
;---------- *  |-------------| = |-------------|    
; YA | Z       | current-var |   | rhs-voltage |             
;inductor is shorcut in DC analysis
(defmethod map-device ((device-instance class-bipolar-transistor) (m class-matrix-system))
     (print "mapping bipolar transistor")
  (let ((vc         (make-var-node  'v (node-c device-instance)))
        (vb         (make-var-node  'v (node-b device-instance)))
        (ve         (make-var-node  'v (node-e device-instance)))

        (icc        (make-var-name  'icc (name device-instance)))
        (iec        (make-var-name  'iec (name device-instance)))
        
        (ic         (make-var-name  'ic (name  device-instance)))
        (ib         (make-var-name  'ib (name  device-instance)))
        
        (ie         (make-var-name  'ie (name  device-instance))))
        
        
        
     (print "stage 1")


;E matrix
        (set-g-value m vc ic #'+   1)
        (set-g-value m vb ie #'+   1)
        (set-g-value m vb ib #'+   1)


        (set-g-value m icc icc #'+   1)
        (set-g-value m icc iec #'-   (alpha-reverse device-instance))

       (set-g-value m iec icc #'-    (alpha-forward device-instance))

        (set-g-value m iec iec #'+   1)

        (set-g-value m ib ic #'-   1)
        (set-g-value m ib ie #'-   1)


(set-rhsl-start-value m iec #'+ 0.05d0)
(set-rhsl-start-value m ic #'+ 0.05d0)
(set-rhsl-start-value m ib #'+ 0.05d0)
(set-rhsl-start-value m ie #'+ 0.05d0)
(set-rhsl-start-value m icc #'+ 0.05d0)


     (print "stage 2")

;G matrix

;Z matrix        
;     (set-z-value m q n+ #'+   1)
;    (set-z-value m q n- #'-   1)
;  (set-rhs-value m i #'+ (diode-current d))    ;rhska je v tomhle zapisu nula


;-------------------
        (set-equations-value m icc (bipolar-transistor-icc device-instance vb ve)) 

        (set-d-value m icc  vb  (bipolar-transistor-icc-dvb device-instance vb ve))        
        (set-d-value m icc  ve  (bipolar-transistor-icc-dve device-instance vb ve))
;-------------------


     (print "stage 3")

        (set-equations-value m iec (bipolar-transistor-iec device-instance vb vc)) 


     (print "stage 4")

        (set-d-value m iec  vb  (bipolar-transistor-iec-dvb device-instance vb vc))        
        (set-d-value m iec  vc  (bipolar-transistor-iec-dvc device-instance vb vc))
        


     (print "stage 5")
        
                        
                   ))
                    

; Function for easy capacitor definition
; (c name node1 node2 value)
; example:
; (BJT "BJT1" 1 2 3)
(defun BJT (name node-b node-c node-e)
  (print "Device Model - Bipolar Transistor")
  (net-insert-device 
    (make-instance 'class-bipolar-transistor 
                :name name
                :node-b node-b
                :node-c node-c
                :node-e node-e
                )
     name))
                

