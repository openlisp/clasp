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


(defclass number-vector ()
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
    
    (data-vector
      :reader get-vector
      :initform '()
      :documentation "")
    
    (initial-element
      :initarg :initial-element
      :reader initial-element
      :initform 0.0d0
      :documentation "")))

(defgeneric size (vec)
  (:documentation "Returns vector size."))
(defmethod size ((vec number-vector))
  (length (get-vector vec)))


;(defgeneric add-vector-value (vec op value)
;  (:documentation "Add new vector value."))
;(defmethod add-vector-value ((vec number-vector) op value)
;  (vector-push (funcall op 0.0d0 value) (get-vector vec) ))



(defgeneric set-vector-value (vec pos value)
  (:documentation "Set Initial start vector."))
(defmethod set-vector-value ((vec number-vector) pos  value)
  (let ((size (size vec))
      (initial-element (initial-element vec))
      (max-index pos))
    (unless (< max-index size)
      (adjust-array (get-vector vec) (1+ max-index) :initial-element initial-element))
    (let ((var (aref (get-vector vec) pos)))
      (setf
        (aref (get-vector vec) pos)
        (+ var value)))))

; (let ((data-vector-value (elt (get-vector vec) pos)))
;   (setf
;     data-vector-value
;     (funcall op data-vector-value value))))




;2
(defmethod adjust-size ((vec number-vector) size)
 (print "Adjust number-vector")
  (adjust-array  (get-vector vec) size  :initial-element (initial-element vec) ))




(defmethod initialize-instance :after ((vec number-vector) &key)
  (let
    ((adjustable (is-adjustable vec))
      (initial-element (initial-element vec))
      (initial-size (initial-size vec)))
    (setf
      (slot-value vec 'data-vector)
      (make-array initial-size :initial-element initial-element :adjustable adjustable  ))))


