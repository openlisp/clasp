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


;dc sweeep
(defun dc-sweep (start step stop &optional solver data)
  (make-instance 'matrice-system)
  (make-instance 'class-variables)          
  (let ((m       (make-instance 'matrice-system))
        (v       (make-instance 'class-variables)))
    (map-all-devices m)    

 ;   (print (get-sub-stack-vector m 0 (size m)))
    (set-new-symbol-var-matrix v start step stop (get-sub-stack-vector m 0 (size m))) ; will generate only one time point
    (loop for i from 0 below (first (size v)) do
      (setf *time-pos* i)
      (setf *time* (get-symbol-var-time v *time-pos*))
      (dc-solve m v solver data))
    (get-all-symbols v)))




; Maximal position of element in list
;(defun pos-max-element (array) 
;  (let ((max 0)
;        (pos 0))       
;    (loop for i from 0 below (length array) do 
;      (if 
;        (< max (aref array i))
;          (progn 
;            (setf max (aref array i)) 
;            (setf pos i))))
;    pos))




