 (in-package #:clasp)


; Capacitor class definition
(defclass class-bipolar-transistor (class-device)
  ((name  :initarg :name
          :initform (error "Must supply a name of device e.g. R1, C1, D1, ...")
          :accessor name)
   (node+ :accessor node+
          :initform 1
          :initarg :node+)
   (node- :accessor node-
          :initform 2
          :initarg :node-)
   (value :accessor value
          :initform 100
          :initarg :value)))
          



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

(defmethod bipolar-transistor-ICC ((transistor class-bipolar-transistor) vb ve)
  #'(lambda () 
    (- (* is (expt (/ (eval vb) (eval ve) vt)))))) 



(defmethod bipolar-transistor-ICC-dvb ((transistor class-bipolar-transistor) vb ve)
  #'(lambda () 
    (/ (* Is (expt (/ (- (eval vb) (eval ve)) vt))) vt)))



(defmethod bipolar-transistor-ICC-dve ((transistor class-bipolar-transistor) vb ve)
  #'(lambda () 
    (/ (* Is (expt (/ (- (eval vb) (eval ve)) vt)) -1 ) vt)))


;------------------------------------------------------------------------------------


(defmethod bipolar-transistor-IEC ((transistor class-bipolar-transistor) vb vc)
  #'(lambda () 
    (- (* is (expt (/ (eval vb) (eval vc) vt)))))) 



(defmethod bipolar-transistor-charge-IEC-dvb ((transistor class-bipolar-transistor) vb vc)
  #'(lambda () 
    (/ (* Is (expt (/ (- (eval vb) (eval vc)) vt))) vt)))


(defmethod bipolar-transistor-charge-IEC-dvc ((transistor class-bipolar-transistor) vb vc)
  #'(lambda () 
    (/ (* Is (expt (/ (- (eval vb) (eval vc)) vt))) vt)))

          
       
                
                
                
;this method will be called only if it get class resistor and class dc analysis
; y  | A       | voltage-var |   | rhs-current |
;---------- *  |-------------| = |-------------|    
; YA | Z       | current-var |   | rhs-voltage |             
;inductor is shorcut in DC analysis
(defmethod map-device ((d class-bipolar-transistor) (m class-matrix-system))
  (let ((v+       (make-var-node  'v (node+ d)))
        (v-       (make-var-node  'v (node- d)))
        (q        (make-var-name  'q (name  d)))
        (c        (value d)))
        
        



;E matrix
        (set-e-value m vc ic #'+   1)
        (set-e-value m vb ie #'+   1)
        (set-e-value m vb ib #'+   1)


        (set-e-value m icc icc #'+   1)
        (set-e-value m icc iec #'-   ALPHAREVERSE))

       (set-e-value m iec icc #'-    ALPHAFORWARD))

        (set-e-value m iec iec #'+   1)

        (set-e-value m ib ic #'-   1)
        (set-e-value m ib ie #'-   1)


;G matrix

;Z matrix        
   ;     (set-z-value m q n+ #'+   1)
    ;    (set-z-value m q n- #'-   1)
        
              ;  (set-rhs-value m i #'+ (diode-current d))    ;rhska je v tomhle zapisu nula

;-------------------
        (set-equations-value m icc (bipolar-transistor-icc d vb ve)) 

        (set-d-value m icc  vb  (bipolar-transistor-icc-dvb d vb ve))        
        (set-d-value m icc  ve  (bipolar-transistor-icc-dve d vb ve))
;-------------------

        (set-equations-value m iec (bipolar-transistor-iec d vb vc)) 

        (set-d-value m iec  vb  (bipolar-transistor-iec-dvb d vb vc))        
        (set-d-value m iec  vc  (bipolar-transistor-iec-dvc d vb vc))
        

        
                        
                
                    

; Function for easy capacitor definition
; (c name node1 node2 value)
; example:
; (C "C1" 1 2 100)
(defun BJT (name node+ node- value)
  (print "Device Model - Bipolar Transistor")
  (net-insert-device 
    (make-instance 'class-bipolar-transistor 
                :name name
                :node+ node+
                :node- node-
                :value value)
     name))
                

