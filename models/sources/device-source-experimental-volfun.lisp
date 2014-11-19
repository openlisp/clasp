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
(defclass class-source-experimental-volfun (class-device)
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
          

  (defmethod voltage-source-polarity+ (volfun v+ v-)
  #'(lambda ()      

      (COND
        ((< (funcall volfun) 0) -1)
        (T +1)))) 


  (defmethod voltage-source-polarity- (volfun v+ v-)
  #'(lambda ()    
     ; (print (funcall volfun))  
      (COND
        ((< (funcall volfun) 0) +1)
        (T -1)))) 
       
          
; y  | A       | voltage-var |   | rhs-current |
;---------- *  |-------------| = |-------------|    
; YA | Z       | current-var |   | rhs-voltage |           
(defmethod map-device ((d class-source-volfun) (m matrice-system))
  (let ((v+       (make-var-node 'v (node+ d)))
        (v-       (make-var-node 'v (node- d)))
        (i        (make-var-name 'i (name  d)))
        (volfun   (volfun d)))
 (print "voltage g matrix")
;G matrix

    

     ;;   (set-g-value m i  v+  #'+  1)
     ;   (set-g-value m i  v-  #'-  1)
     ;   (set-g-value m v+ i   #'+  1)
     ;  (set-g-value m v- i   #'-  1)


        (set-gd-value m i  v+  (voltage-source-polarity+ volfun v+ v-))        
        (set-gd-value m i  v-  (voltage-source-polarity- volfun v+ v-))

        (set-gd-value m  v+ i (voltage-source-polarity+ volfun v+ v-))        
        (set-gd-value m  v- i (voltage-source-polarity- volfun v+ v-))

;; here must be orientation misteko othervise I dont know
;; Documentation and code has differend deffinition
       ; (set-g-value m i  v+  #'+  1)
      ;  (set-g-value m i  v-  #'-  1)
      ;  (set-g-value m v+ i   #'+  1)
      ;  (set-g-value m v- i   #'-  1)


;RHS        
;(print "voltage rhs vector")
        (set-rhs-equations-value m i volfun)))
 
;;;
;;; Esperimental functional coltage source 
;;; 
;;; Usage example:
;;;   (e "e1" 1 0 5)   
;;;
(defun eef (name node+ node- volfun)
  (net-insert-device 
    (make-instance 'class-source-volfun 
                :name name
                :node+ node+
                :node- node-
                :volfun volfun)
     name))

