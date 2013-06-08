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



;dc solve
(defmethod dc-solve ((m class-matrix-system) (v class-variables) solver &optional data)
  (let ((i  (1- (size m)))
        (residual *residual*)
        (max-iter 300)
        (pop-size 50)
        (max-gen 500)
        ;(damp-coef 0.8d0)
        )
    (if (linear m)
         (solver-luf m v i)
         (cond 
           ((eq solver 'newton-raphson) (solver-newton-raphson m v i residual max-iter))
           ((eq solver 'damped-newton-raphson) (solver-damped-newton-raphson m v i residual max-iter data))      
           ((eq solver 'evolutionary-newton-raphson) (solver-evolutionary-newton-raphson m v i data residual max-iter))      
           ((eq solver 'particle-swarm) (solver-particle-swarm m v i residual max-gen pop-size 0.01 0.01 50)) ;c1 c2 space-size
           (t (solver-newton-raphson m v i residual max-iter))))))





; Solver BDF 
;(defmethod trans-solve ((m class-matrix-system) (v class-variables) &optional solver)
;  (let* (
;        (residual *residual*)
;        (max-iter 300)
;        (i  (1- (size m))))
;        (solver-bdf m v i  epsilon max-iter)
    ;(print i)
   ; (map-all-devices m)    
   ; (set-new-symbol-var-matrix v start step stop (get-sub-stack-vector m 0 (size m)))







(defun print-state (iter argval argval-length residual)
  (with-open-file (stream "output/dc-iter.dat" :direction :output  
                                   :if-exists :APPEND
                                   :if-does-not-exist :create)

    (format stream "~%~5d;   " iter) 
    (iter:iter (iter:for k from 0 below argval-length) 
    (format stream " ~15f; " (grid:gref argval k))) 
    (format stream " ~15f " residual)))
 

;vypis pro geneticky algoritmus
(defun print-state-genetic (iter sub-iter argval argval-length residual)
  (with-open-file (stream "output/dc-iter.dat" :direction :output  
                                   :if-exists :APPEND
                                   :if-does-not-exist :create)
                                   
    (format stream "~%~5d;   " iter) 
    (format stream " ~5d;   " sub-iter) 

    (iter:iter (iter:for k from 0 below argval-length) 
    (format stream " ~15f; " (grid:gref argval k))) 
    (format stream " ~15f " residual)))
    
;debug print
(defun print-debug (label value)
  (print label)
  (print value))



 

;Rovnice pro vypocet nahodne hodnoty v danem rozmezi
(defun random-range (start stop) 
  (+ (random (-  stop start)) start))
    

