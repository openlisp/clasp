;;;; Copyright (c) 2013 David Cerny, All Rights Reserved
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;;   * Redistributions of source code must retain the above copyright
;;;;     notice, this list of conditions and the following disclaimer.
;;;;
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


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
      :initarg :node-)

    ))

;(defmethod test ((m class-diode) k)
;  #'(lambda () (+ k 5 (n m))))


(defmethod diode-simple2-current ((diode class-diode-simple2) v+ v-)
  #'(lambda ()
    (COND
      ((< (- (eval v+) (eval v-)) 0) 0)
      (T (- (* 10e-12 (- (exp (/ (- (eval v+) (eval v-)) 26e-3))  1)))))))


(defmethod diode-simple2-current-dv+ ((diode class-diode-simple2) v+ v-)
  #'(lambda ()
    (COND
      ((< (- (eval v+) (eval v-)) 0) 0)
      (T (- (/ (exp (/ (- (eval v+) (eval v-)) 38.462))  26e9))))))

(defmethod diode-simple2-current-dv- ((diode class-diode-simple2) v+ v-)
  #'(lambda ()
    (COND
      ((< (- (eval v+) (eval v-)) 0) 0)
      (T (+ (/ (exp (/ (- (eval v+) (eval v-)) 38.462))   26e9) )))))


(defmethod map-device ((d class-diode-simple2) (m matrice-system))
  (print "mapping diode simple 2")
  (let ((v+       (make-var-node 'v (node+ d)))
      (v-       (make-var-node 'v (node- d)))
      (i        (make-var-name 'i (name  d))))
    
    
  ;  (set-g-value m v+ i #'+  1)
   ; (set-g-value m v- i #'+ -1)
 ;   (set-g-value m i  i #'+ -1)
    
    ;  (set-rhs-value m i #'+ (diode-current d))    ;rhska je v tomhle zapisu nula
  ;  (set-equations-value m i (diode-simple2-current d v+ v-))
    
  ;  (set-d-value m i  v+  (diode-simple2-current-dv+ d v+ v-))

  ;  (set-d-value m i  v-  (diode-simple2-current-dv- d v+ v-))



    (setf (g-number-array v+ i m) 1)   
    (setf (g-number-array v- i m) -1)   
  ;  (setf (g-number-array i i m) -1)  
        
      ;  (set-rhs-value m i #'+ (diode-current d))    ;rhska je v tomhle zapisu nula
    ;    (set-equations-value m i (diode-simple-current d v+ v-)) 

    ;    (set-d-value m i  v+  (diode-simple-current-dv+ d v+ v-))        
    ;    (set-d-value m i  v-  (diode-simple-current-dv- d v+ v-))
        
      ;factory starting predict   

    (setf (nonlinear-equation-vector i m)  (diode-simple2-current d v+ v-))
    (setf (differetial-equation-array i v+ m) (diode-simple2-current-dv+ d v+ v-))
    (setf (differetial-equation-array i v- m)   (diode-simple2-current-dv- d v+ v-))

))
    
    ;factory starting predict
    
  ;  (set-rhsl-start-value m i #'+ 0.1562d0)))



;; apply car a  cdr a
;;(setf a `(,f 1 2))

; Function for easy source definition
; (e name node1 node2 value)
; example:
; (d "d1" 1 0)
(defun ds2 (name node+ node- )
  (net-insert-device

    (make-instance 'class-diode-simple2
      :name name
      :node+ node+
      :node- node-)
    
    name))


