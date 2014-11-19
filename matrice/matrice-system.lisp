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


; USAGE
;;(get-vector (g-equation-array matrix))
;;(set-vector-value (g-equation-array matrix) row col op num)


(in-package #:clasp)

(defclass matrice-system () (
    
    (g-number-array
      :reader get-g-number-array
      :initform (make-instance 'number-array ))
    
    (g-equation-array
      :reader get-g-equation-array
      :initform (make-instance 'equation-array  ))
    
    (e-number-array
      :reader get-e-number-array
      :initform (make-instance 'number-array ))
    
    (z-number-array
      :reader get-z-number-array
      :initform (make-instance 'number-array ))
    
    (differetial-equation-array
      :reader get-differetial-equation-array
      :initform (make-instance 'equation-array ))
    
    (nonlinear-equation-vector
      :reader get-nonlinear-equation-vector
      :initform (make-instance 'equation-vector ))
    
    (initial-number-vector
      :reader get-initial-number-vector
      :initform (make-instance 'number-vector ))
    
    (rhs-number-vector
      :reader get-rhs-number-vector
      :initform (make-instance 'number-vector ))
    
    
    
    
    (variable-label-vector
      :accessor variable-label-vector
      :initform (make-array 1 :fill-pointer 1 :adjustable t :initial-contents  (list (make-var-node 'V 0))))
    
    
    (is-nonlinear-circuit
      :accessor is-nonlinear-circuit
      :initform nil)
    
    (is-differential-circuit
      :accessor is-differential-circuit
      :initform nil)
    
    (size
      :accessor size
      :initform 1)
    
    ))


(defgeneric make-var-node (name num)
  (:documentation "Creates variable node."))
(defmethod make-var-node (name num)
  (intern
    (concatenate 'string
      (string-upcase (format nil "~s" name))
      (string-upcase (format nil "~s" num)))))


(defgeneric make-var-name (name str)
  (:documentation "Creates variable name."))
(defmethod make-var-name (name str)
  (intern
    (concatenate 'string
      (string-upcase (format nil "~s" name))
      (string-upcase (string str)))))





(defgeneric get-position (m value )
  (:documentation "Return position of the element in matrix, if element is missing in matrix it returns max index and element is added as new element in matrix"))
(defmethod get-position ((m matrice-system) value)
  (let
    ((old-pos (position-if #'(lambda (x) (equal x value)) (stack m) :end (size m)))
      (new-pos (size m)))
    (cond (old-pos old-pos)
      (t
        (set value 0)
        (setf (aref (stack m) new-pos) value)
        (setf (size m) (+ (size m) 1))
        new-pos))))


















;arrays value
;g-number-array
(defgeneric (setf g-number-array) (value row col matrice)
  (:documentation "Setter for lookup methods usage: (setf (initial-value-vector row col matrice) value)"))
(defmethod (setf g-number-array) (value row col (matrice matrice-system))
  (set-array-value (get-g-number-array matrice) (get-position matrice row) (get-position matrice col) value ))


;  (e-number-array

(defgeneric (setf e-number-array) (value row col matrice)
  (:documentation "Setter for lookup methods usage: (setf (initial-value-vector row col matrice) value)"))
(defmethod (setf e-number-array) (value row col (matrice matrice-system))
  (set-array-value (get-e-number-array matrice) (get-position matrice row) (get-position matrice col) value ))



; (z-number-array

(defgeneric (setf z-number-array) (value row col matrice)
  (:documentation "Setter for lookup methods usage: (setf (initial-value-vector row col matrice) value)"))
(defmethod (setf z-number-array) (value row col (matrice matrice-system))
  (set-array-value (get-z-number-array matrice) (get-position matrice row) (get-position matrice col) value ))


;arrays equation



; (differetial-equation-array
(defgeneric (setf differetial-equation-array) (value row col matrice)
  (:documentation "Setter for lookup methods usage: (setf (initial-value-vector row col matrice) value)"))
(defmethod (setf differetial-equation-array) (value row col (matrice matrice-system))
  (set-array-equation (get-differetial-equation-array matrice) (get-position matrice row) (get-position matrice col) value ))









;vectors value



(defgeneric (setf initial-vector-value) (value row matrice)
  (:documentation "Setter for lookup methods usage: (setf (initial-value-vector row matrice) value)"))
(defmethod (setf initial-vector-value) (value row (matrice matrice-system))
  (set-vector-value (get-initial-number-vector matrice) (get-position matrice row) value))




;

;   (rhs-number-vector

(defgeneric (setf irhs-number-vector) (value row matrice)
  (:documentation "Setter for lookup methods usage: (setf (initial-value-vector row matrice) value)"))
(defmethod (setf rhs-number-vector) (value row (matrice matrice-system))
  (set-vector-value (get-rhs-number-vector matrice) (get-position matrice row) value))


;vectors equation


;    (nonlinear-equation-vector

(defgeneric (setf nonlinear-equation-vector) (value row matrice)
  (:documentation "Setter for lookup methods usage: (setf (initial-value-vector row matrice) value)"))
(defmethod (setf nonlinear-equation-vector) (value row (matrice matrice-system))
  (set-vector-value (get-nonlinear-equation-vector matrice) (get-position matrice row) value))































;(defgeneric set-model-vector-value (m system-type row value  )
;  (:documentation "This method will map all paremteres to particular matrice systems"))

;(defmethod set-model-vector-value ((m matrice-system) system-type row value )
;  (let ((op #'+)
;      (pos-row (get-position m row)))
;    (funcall system-type pos-row op value)))



;(defgeneric set-model-array-value (m system-type row col value  )
;  (:documentation "This method will map all paremteres to particular matrice systems"))

;(defmethod set-model-array-value ((m matrice-system) system-type row col value )
;  (let ((op #'+)
;      (pos-row (get-position m row))
;      (pos-col (get-position m col)))
;    (funcall system-type pos-row pos-col op value)))







