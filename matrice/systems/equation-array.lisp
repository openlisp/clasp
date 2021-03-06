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

(defclass equation-array ()
  ((initial-size
      :initarg :initial-size
      :reader initial-size
      :initform 1
      :documentation "")
    
    (adjustable
      :initarg :adjustable
      :reader is-adjustable
      :initform t
      :documentation "")

    (data-array
      :reader get-array
      :initform '()
      :documentation "")
    
    (initial-element
      :initarg :initial-element
      :reader initial-element
      :initform '()
      :documentation "")))


(defgeneric size (arr)
  (:documentation "Returns array size."))

(defmethod size ((arr equation-array))
  (car (array-dimensions (get-array arr))))



(defgeneric set-array-equation (arr row col value)
  (:documentation "Set new array equation."))
(defmethod set-array-equation ((arr equation-array) row col value)
 (print "Set new array equation.")
  (let ((size (size arr))
      (initial-element (initial-element arr))
      (max-index (max row col)))
    (unless (< max-index size)
      (adjust-array (get-array arr) (list (1+ max-index) (1+ max-index)) :initial-element initial-element ))
      (push value (aref (get-array arr) row col))))






;2
(defmethod adjust-size ((arr equation-array) size)
 (print "Adjust equation-array")
  (adjust-array (get-array arr) (list size size) :initial-element (initial-element arr)))



;(defgeneric add-array-equation (arr row col value)
;  (:documentation "Add new array equation."))
;(defmethod add-array-equation ((arr equation-array) row col value)
;  (adjust-array (get-array arr) (list size size))
;  (set-array-equation (arr row col value)))



(defmethod initialize-instance :after ((arr equation-array) &key)
  (let
    ((adjustable (is-adjustable arr))
      (initial-element (initial-element arr))
      (initial-size (initial-size arr)))
    (setf
      (slot-value arr 'data-array)
      (make-array (list initial-size initial-size) :initial-element initial-element :adjustable adjustable))))



