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
    
    
    (rhs-equation-vector
      :reader get-rhs-equation-vector
      :initform (make-instance 'equation-vector ))
    
    
    
    
    (variable-label-vector
      :reader get-variable-label-vector
      
      :initform (make-array 1 :adjustable t :initial-contents  (list (make-var-node 'V 0))))
    
    ; :initform (list (make-var-node 'V 0)))
    
    (is-nonlinear-circuit
      :accessor is-nonlinear-circuit
      :initform nil)
    
    (is-differential-circuit
      :accessor is-differential-circuit
      :initform nil)
    
    ; (size
    ;   :accessor size
    ;   :initform 1)
    
    ))



(defgeneric size (m)
  (:documentation "Returns reduces Size of arrays"))
(defmethod size ((m matrice-system))
  (1-  (real-size m)))


(defgeneric real-size  (m)
  (:documentation "Returns Real Size of arrays"))
(defmethod real-size ((m matrice-system))
  (length (get-variable-label-vector m)))



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
  (print "get fck position")
  (print (get-variable-label-vector m))
  (print value)
  (print " ")
  (let
    ((found-pos (position-if #'(lambda (x) (equal x value)) (get-variable-label-vector m) :end (length (get-variable-label-vector m))))
      (new-pos (length (get-variable-label-vector m))))
    (print found-pos)
    (cond (found-pos (1- found-pos))
      (t
        ;(set value 0)
        (setf (variable-label-vector m) value)
        ;(setf (size m) (+ (size m) 1))
        ; (print "wjhot the fuxxl")
        ;  (print (get-variable-label-vector m))
        ; (print         new-pos)
        (1- new-pos )))))








;arrays value
;g-number-array
(defgeneric check-zero-position-array (row col matrice)
  (:documentation "Setter for lookup methods usage: (setf (g-number-array row col matrice) value)"))
(defmethod check-zero-position-array (row col (matrice matrice-system))
  (cond
    	((equal (make-var-node 'V 0) row)   nil)
    	((equal (make-var-node 'V 0) col)   nil)
    	(t    t )))

(defgeneric check-zero-position-vector (pos matrice)
  (:documentation "Setter for lookup methods usage: (setf (g-number-array row col matrice) value)"))
(defmethod check-zero-position-vector (pos (matrice matrice-system))
  (cond
    	((equal (make-var-node 'V 0) pos)   nil)
    	(t    t )))





(defgeneric (setf variable-label-vector) (value m)
  (:documentation "Setter for lookup methods usage: (setf (g-number-array row col matrice) value)"))
(defmethod (setf variable-label-vector) (value (m matrice-system))
  (print value)
  (let ((size (length (get-variable-label-vector m))))
    (adjust-array (get-variable-label-vector m) (1+ size ))
    (print (get-variable-label-vector m))
    (setf (aref (get-variable-label-vector m) size)  value )))







;arrays value
;g-number-array
(defgeneric (setf g-number-array) (value row col matrice)
  (:documentation "Setter for lookup methods usage: (setf (g-number-array row col matrice) value)"))
(defmethod (setf g-number-array) (value row col (matrice matrice-system))
  
  
  (if (check-zero-position-array row col matrice)
    
    (set-array-value (get-g-number-array matrice) (get-position matrice row) (get-position matrice col) value )
    
    ))


(defgeneric (setf g-equation-array) (value row col matrice)
  (:documentation "Setter for lookup methods usage: (setf (initial-value-vector row col matrice) value)"))
(defmethod (setf g-equation-array) (value row col (matrice matrice-system))
  (if (check-zero-position-array row col matrice)
    (set-array-equation (get-g-equation-array matrice) (get-position matrice row) (get-position matrice col) value )))






;  (e-number-array

(defgeneric (setf e-number-array) (value row col matrice)
  (:documentation "Setter for lookup methods usage: (setf (z-number-array row col matrice) value)"))
(defmethod (setf e-number-array) (value row col (matrice matrice-system))
  (if (check-zero-position-array row col matrice )
    (set-array-value (get-e-number-array matrice) (get-position matrice row) (get-position matrice col) value )))



; (z-number-array

(defgeneric (setf z-number-array) (value row col matrice)
  (:documentation "Setter for lookup methods usage: (setf (initial-value-vector row col matrice) value)"))
(defmethod (setf z-number-array) (value row col (matrice matrice-system))
  (if (check-zero-position-array row col matrice )
    (set-array-value (get-z-number-array matrice) (get-position matrice row) (get-position matrice col) value )))


;arrays equation



; (differetial-equation-array
(defgeneric (setf differetial-equation-array) (value row col matrice)
  (:documentation "Setter for lookup methods usage: (setf (initial-value-vector row col matrice) value)"))
(defmethod (setf differetial-equation-array) (value row col (matrice matrice-system))
  (if (check-zero-position-array row col matrice)
    (set-array-equation (get-differetial-equation-array matrice) (get-position matrice row) (get-position matrice col) value )))









;vectors value



(defgeneric (setf initial-vector-value) (value row matrice)
  (:documentation "Setter for lookup methods usage: (setf (initial-value-vector row matrice) value)"))
(defmethod (setf initial-vector-value) (value row (matrice matrice-system))
  (if (check-zero-position-vector row matrice )
    (set-vector-value (get-initial-number-vector matrice) (get-position matrice row) value)))




;

;   (rhs-number-vector

(defgeneric (setf rhs-number-vector) (value row matrice)
  (:documentation "Setter for lookup methods usage: (setf (rhs-number-vector row matrice) value)"))
(defmethod (setf rhs-number-vector) (value row (matrice matrice-system))
  (if (check-zero-position-vector row matrice )
    (set-vector-value (get-rhs-number-vector matrice) (get-position matrice row) value)))






;vectors equation


;    (nonlinear-equation-vector

(defgeneric (setf nonlinear-equation-vector) (value row matrice)
  (:documentation "Setter for lookup methods usage: (setf (nonlinear-equation-vector row matrice) value)"))
(defmethod (setf nonlinear-equation-vector) (value row (matrice matrice-system))
  (if (check-zero-position-vector row matrice )
    (set-vector-equation (get-nonlinear-equation-vector matrice) (get-position matrice row) value)))



(defgeneric (setf rhs-equation-vector) (value row matrice)
  (:documentation "Setter for lookup methods usage: (setf (rhs-equation-vector row matrice) value)"))
(defmethod (setf rhs-equation-vector) (value row (matrice matrice-system))
  (if (check-zero-position-vector row matrice matrice-system)
    (set-vector-equation (get-rhs-equation-vector matrice) (get-position matrice row) value)))






;3
(defgeneric adjust-size (arr size)
  (:documentation "adjust size of given object array"))
;2




;1
; Ask for this on STACK Overflow
(defgeneric  eval-element (elm)
  (:documentation "returns Equations"))

(defmethod eval-element ((elm list)) 
 (print "eval element list")
(apply #'+ (mapcar #'eval-element elm )))

(defmethod eval-element ((elm number)) 
 (print "eval element number")
  elm)

(defmethod eval-element ((elm function)) 
 (print "eval element simple function")
 (print (funcall elm))
  (funcall elm))

(defmethod eval-element ((elm symbol)) 
 (print "eval element simple symbol")
 (cond ((eq (eval elm) nil) 0)
        (t  (eval-element (eval elm)))))




















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





(defgeneric print-all-matrices  (matrice)
  (:documentation "Setter for lookup methods usage: (setf (rhs-equation-vector row matrice) value)"))
(defmethod print-all-matrices ((matrice matrice-system))
  
  
  
  (print "variable-label-vector")
  (print (get-variable-label-vector matrice))
  
  
  (print "g-number-array")
  (print (get-array (get-g-number-array matrice)))
  
  (print "g-equation-array")
  (print (get-array (get-g-equation-array matrice)))
  
  (print "e-number-array")
  (print (get-array (get-e-number-array matrice)))
  
  (print "z-number-array")
  (print (get-array (get-z-number-array matrice)))
  
  
  
  (print "differetial-equation-array")
  (print (get-array  (get-differetial-equation-array matrice)))
  
  
  (print "nonlinear-equation-vector")
  (print (get-vector (get-nonlinear-equation-vector matrice)))
  
  
  (print "initial-number-vector")
  (print (get-vector (get-initial-number-vector matrice)))
  
  
  (print "rhs-number-vector")
  (print (get-vector (get-rhs-number-vector matrice)))
  
  
  (print "rhs-equation-vector")
  (print (get-vector (get-rhs-equation-vector matrice)))
  
  )


