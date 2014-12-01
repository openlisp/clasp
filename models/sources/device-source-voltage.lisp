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
(defclass class-source-voltage (class-device)
  ((name  :accessor name
          :initform (error "Must supply a name of device.")
          :initarg :name)
          
   (node+ :accessor node+
          :initform (error "Must supply a positive node.")
          :initarg :node+)
          
   (node- :accessor node-
          :initform (error "Must supply a negative node.")
          :initarg :node-)
          
   (voltage :accessor voltage
            :initform (error "Must supply a source voltage.")
            :initarg :voltage)))     
          
          
; y  | A       | voltage-var |   | rhs-current |
;---------- *  |-------------| = |-------------|    
; YA | Z       | current-var |   | rhs-voltage |           
(defmethod map-device ((d class-source-voltage) (m matrice-system))
  (let ((v+       (make-var-node 'v (node+ d)))
        (v-       (make-var-node 'v (node- d)))
        (i        (make-var-name 'i (name  d)))
        (vol    (voltage d)))
;G matrix

;        (set-g-value m i  v+  #'+  1)
;        (set-g-value m i  v-  #'-  1)
;        (set-g-value m v+ i   #'+  1)
;        (set-g-value m v- i   #'-  1)


;        (set-rhs-number-value m i #'+ vol)
(print "voltage 1")

    (setf (g-number-array i v+  m)   1)
(print "voltage 2")

    (setf (g-number-array i v-  m)  -1)
(print "voltage 3")

    (setf (g-number-array v+ i  m)   1)
(print "voltage 4")

    (setf (g-number-array v- i  m)  -1)

(print "voltage 5")

    (setf (rhs-number-vector i m)  vol)

(print "voltage 6")


))
 
 
; Function for easy source definition
; (e name node1 node2 value)
; example:
; (e "e1" 1 0 5)   
(defun e (name node+ node- voltage)
  (net-insert-device 
    (make-instance 'class-source-voltage 
                :name name
                :node+ node+
                :node- node-
                :voltage voltage)
     name))

