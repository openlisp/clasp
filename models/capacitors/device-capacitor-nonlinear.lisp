(in-package #:clasp)


; Capacitor class definition
(defclass class-capacitor-nonlinear (class-device)
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


;;; q = c * ( 1 + u^2 )
(defmethod capacitor-nonlinear-charge ((capacitor class-capacitor-nonlinear) v+ v-)
  #'(lambda ()
    (*  (+ (expt (- (eval v+) (eval v-)) 2) 1)  (value capacitor))))

;;; +dq+= 2 * u * c
(defmethod capacitor-nonlinear-charge-dv+ ((capacitor class-capacitor-nonlinear) v+ v-)
  #'(lambda ()
    (* (-  (eval v+) (eval v-))  (value capacitor) 2)))

;;; -dq= -2 * u * c
(defmethod capacitor-nonlinear-charge-dv- ((capacitor class-capacitor-nonlinear) v+ v-)
  #'(lambda ()
    (* (-  (eval v+) (eval v-))  (value capacitor) -2)))



;(defun test-device-map-c ()
;  (map-dc
;    (make-instance 'class-capacitor
;                :name "name"
;                :node- 1
;                :node 2
;                :value "value")))



;this method will be called only if it get class resistor and class dc analysis
; y  | A       | voltage-var |   | rhs-current |
;---------- *  |-------------| = |-------------|
; YA | Z       | current-var |   | rhs-voltage |
;inductor is shorcut in DC analysis
(defmethod map-device ((d class-capacitor-nonlinear) (m matrice-system))
  (let (
      (v+       (make-var-node  'v (node+ d)))
      (v-       (make-var-node  'v (node- d)))
      (q        (make-var-name  'q (name  d)))
      (c        (value d)))
    
    
    
    
    
    
    (setf (g-number-array q q  m)  -1)
    
    
    (setf (e-number-array v+ q  m) 1)
    (setf (e-number-array v- q  m) -1)
    
    
    
    
    
    (setf (nonlinear-equation-vector q m)  (capacitor-nonlinear-charge d v+ v-))
    (setf (differetial-equation-array q v+ m) (capacitor-nonlinear-charge-dv+ d v+ v-))
    (setf (differetial-equation-array q v- m)   (capacitor-nonlinear-charge-dv- d v+ v-))
    
    
    
    
    ))








; Function for easy capacitor definition
; (c name node1 node2 value)
; example:
; (C "C1" 1 2 100)
(defun CR (name node+ node- value)
  (print "Device Model - Nonlinear Capacitor")
  (net-insert-device
    (make-instance 'class-capacitor-nonlinear
      :name name
      :node+ node+
      :node- node-
      :value value)
    name))


