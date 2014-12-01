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

(defun trans (start step stop &optional solver)
  (make-instance 'matrice-system)
  (make-instance 'class-variables)
  (let ((m       (make-instance 'matrice-system))
      ;        (v       (make-instance 'class-variables))
      (h 1e-8)
      (reltol 1e-3)
      (abstol 1e-12)
      (chgtol 1e-14)
      ;Mapovani zarizeni ktere jsou v simulaci
      (vtrans  (make-instance 'class-variables)))
    (map-all-devices m)
    
    
    (print "1*")
    (print (get-variable-label-vector m ))
    (print "2*")
    
    ;tvorba tridy s vysledky
    (set-new-symbol-var-matrix vtrans start step stop (get-variable-label-vector m))
    (print "3*")
    ;postupne se menici se pole
    ;    (set-new-symbol-var-matrix vtrans start (/ step 50) (+ start (/ step 5)) (get-sub-stack-vector m 0 (size m)))
    
    ; DC point
    ;x_n
    (setf *time-pos* 0)
    
    (setf *time* (get-symbol-var-time vtrans *time-pos*))  ; cas 0
    
    (print "4*")
    
    (solver-newton-raphson m vtrans (1- (size m)) *residual* 50)
    
    ;    (print "size m ")
    ;    (print (size m))
    ;    (set-sub-symbol-var-vector v *time-pos* 0 (get-sub-symbol-var-vector vtrans *time-pos* 0 (1- (size m))))
    
    (print "5*")
    
    (setf *time-pos* 1)
    (set-symbol-var-time vtrans *time-pos* (+ *time* h))
    (setf *time* (+ *time* h)) ; cas 0 + h
    
    
    ;    (print "5.5*")
    ;    (print "test navratovosti bdf konvergencni funkce")
    
    ; kdyz to navrati nulu tak se to musi prepocitat s mensim krokem
    ; kdyz ti navrati 1 tak je vse v pohode
    (solver-bdf-0 m vtrans (1- (size m)) h *residual* 50)
    
    
    ;x_n+1
    ;    (print "6*")
    
    (setf *time-pos* 2)
    
    (setf *time* (get-symbol-var-time vtrans *time-pos*)) ; tady toto nastavovani casu se musi dit podle kroku
    
    (solver-bdf-1 m vtrans (1- (size m)) h *residual* 50)  ; 0 + h + h
    (print "7*")
    
    
    ;x_n+2    ;order
    (setf *time-pos* 3)
    (setf *time* (get-symbol-var-time vtrans *time-pos*))
    (solver-bdf-2 m vtrans (1- (size m)) h *residual* 50)
    
    
    
    (loop for i from 4 below (first (size vtrans)) do
      (setf *time-pos* i)
      (setf *time* (get-symbol-var-time vtrans *time-pos*))
      (solver-bdf-1 m vtrans (1- (size m)) h *residual* 50))
    
    
    (get-all-symbols vtrans)
    ))
;      (loop for i from 1 below (first (size v)) do
;      (setf *time-pos* i)
;      (setf *time* (get-symbol-var-time v *time-pos*))
;      (solver-newton-raphson m v i residual max-iter))

;
;    (setf *time-pos* i)
;
;    (trans-solve m v solver)
;    (get-all-symbols v)))
