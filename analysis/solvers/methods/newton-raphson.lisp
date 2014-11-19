;;;; copyright (c) 2011-2014 David Cerny, 2014 Senman, all rights reserved
;;;;
;;;; redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;;   * redistributions of source code must retain the above copyright
;;;;     notice, this list of conditions and the following disclaimer.
;;;;
;;;;   * redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;;
;;;; this software is provided by the author 'as is' and any expressed
;;;; or implied warranties, including, but not limited to, the implied
;;;; warranties of merchantability and fitness for a particular purpose
;;;; are disclaimed.  in no event shall the author be liable for any
;;;; direct, indirect, incidental, special, exemplary, or consequential
;;;; damages (including, but not limited to, procurement of substitute
;;;; goods or services; loss of use, data, or profits; or business
;;;; interruption) however caused and on any theory of liability,
;;;; whether in contract, strict liability, or tort (including
;;;; negligence or otherwise) arising in any way out of the use of this
;;;; software, even if advised of the possibility of such damage.


(in-package #:clasp)


;;;
;;; non linear newton raphson
;;;

(defun residual-test (residual epsilon)
  (if (< (abs residual) epsilon)
    t
    nil))


(defun symbol-eval (x)
  (if (symbolp x)
    (funcall x)
     x))

(defmethod solver-newton-raphson ((m matrice-system) (var-arr class-variables) i epsilon max-iter)
  (format t "~%~%Solver: Newton Raphson~% Matrix-dimesion: ~d~%Epsilon: ~d~%Max Iter: ~d~%" i epsilon max-iter)
  
  (let* (
      
      
      (roulete (gsl:make-random-number-generator gsl:+CMRG+ 0))
      
      (linear-matrices
        (grid:map-n-grids
          :sources
          (list
            (list (get-sub-g-array m 1 1 i i) nil)
            (list (get-sub-e-array m 1 1 i i) nil))
          :destination-specification `((grid:foreign-array ,i ,i) double-float)
          :combination-function (lambda (a b) (+ a b))))
      (residual 0)
      ; (new-value-vector (grid:make-foreign-array 'double-float :dimensions (list i) :initial-element 0.6d0)))
      
      (old-rhs-vector (grid:make-foreign-array 'double-float :dimensions (list i) :initial-element 0.0d0))
      )
    
    ;(print "Sleeping")
    
    ; (sleep 10)
    (print 1)
    
    
    ;;debug
    (setf max-iter 10)
    
    
    (setf new-value-vector
      (grid:map-grid
        :source old-rhs-vector
        :destination-specification `((grid:foreign-array ,i ) double-float)
        :element-function (lambda (x) (coerce (+ x  (gsl:sample roulete :gaussian :sigma 2.0d0) )  'double-float))))
    
    
    
    (print 2)
    
    (loop for iter from 0
      	 while (< iter max-iter)
      	   initially
      
      (iter:iter(iter:for k from 1 below (size m))
        (set (grid:gref (stack m) k)  (grid:gref new-value-vector (- k 1))))	
      
      	 do
      	
      	
      (format t "~%Iteration Number: ~d/~d~%" iter max-iter)
    ;  (print "something 1 ")
    ;  (print (get-sub-gd-array m 1 1 i i)  )
   ;   (print "something 2 ")
    ;  (print (get-sub-d-array m 1 1 i i))
    ;  (print "something 3")
   ;   (sleep 10)
      
      
      (let* (
          

          (value-linear-matrices
            (grid:map-n-grids
              :sources
              (list
                (list linear-matrices nil)
                (list
                  (grid:map-grid
                    :source (get-sub-gd-array m 1 1 i i)
                    :element-function (lambda (x) (coerce (if  (eq x nil)  0.0d0 (apply #'+  (mapcar #'symbol-eval x)))  'double-float)))  nil))
              :destination-specification `((grid:foreign-array ,i ,i) double-float)
              :combination-function (lambda (a b ) (+ a b))))
          ;  (coerce  (if  (eq x nil)  0.0d0  x) 'double-float)
          
         
           (jacobian-matrix
            (grid:map-n-grids
              :sources
              (list
                (list value-linear-matrices nil)
                (list
                  (grid:map-grid
                    :source (get-sub-d-array m 1 1 i i)
                    :element-function (lambda (x) (coerce (if  (eq x nil)  0.0d0 (apply #'+  (mapcar #'symbol-eval x)))  'double-float)))  nil))
              :destination-specification `((grid:foreign-array ,i ,i) double-float)
              :combination-function (lambda (a b ) (+ a b))))
          
           ;rhs vector
          (rhs-vector
            (grid:map-n-grids
              :sources
              (list
                (list ;rhs equations
                  (grid:map-grid
                    :source (get-sub-rhs-equations-vector m 1 i) ;rhs equations
                    :element-function (lambda (x) (coerce (if  (eq x nil)  0.0d0 (apply #'+  (mapcar #'symbol-eval x))) 'double-float))) nil)
                (list ;rhs values
                  (grid:map-grid
                    :source (get-sub-rhs-number-vector m 1 i) ;rhs values
                    :element-function (lambda (x) (coerce  (if  (eq x nil)  0.0d0  x) 'double-float))) nil)
                (list
                  (gsll:matrix-product
                    value-linear-matrices
                    new-value-vector) nil) ; linear vector
                (list
                  (grid:map-grid
                    :source (get-sub-equations-vector m 1 i)  ; non linear equations
                    :element-function (lambda (x) (coerce (if  (eq x nil)  0.0d0 (apply #'+  (mapcar #'symbol-eval x)))  'double-float))) nil))
              
              :combination-function (lambda (a b c d) (- (+ a b) (+ c d )))
              :destination-specification `((grid:foreign-array ,i) double-float)))
         
          
          )
        
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        
        ;(print (grid:map-grid
        ;                   :source (get-sub-equations-vector m 1 (size m))  ; non linear equations
        ;                  :element-function (lambda (x) (coerce (apply #'+ (mapcar #'funcall x))  'double-float))))
        (print "we got thourh")

         (sleep 30)
        
        
        
        (setf residual
          (-
            (gsl:euclidean-norm old-rhs-vector)
            (gsl:euclidean-norm rhs-vector)))
        
        (print "residual ")
        (print residual)
        
        (if (and (residual-test residual epsilon) (> iter 0))
          (progn
            (print-state iter new-value-vector i residual)
            (return t))
          (progn
            (setf old-rhs-vector rhs-vector)))
        
        
        
        (multiple-value-bind
          (jacobian-matrix perm)
          (gsl:lu-decomposition jacobian-matrix)
          (ignore-errors (gsl:lu-solve jacobian-matrix rhs-vector perm)))
        
        
        (setf new-value-vector
          (grid:map-n-grids
            :sources
            (list
              (list new-value-vector nil)
              (list rhs-vector nil))
            :destination-specification `((grid:foreign-array ,i) double-float)
            
            :combination-function (lambda (a b) (+ a b))))
        
        (print-state iter new-value-vector i residual)
        (set-symbol-var-value var-arr *time-pos* 1 (grid:gref new-value-vector (- 1 1)))
        
        (iter:iter (iter:for k from 1 below (size m))
          (set-symbol-var-value var-arr *time-pos* k (grid:gref new-value-vector (- k 1)))
          (set (grid:gref (stack m) k)  (grid:gref new-value-vector (- k 1)))))
      
      finally
      (print-state iter new-value-vector i residual)
      (incf *not-convergency*)
      (return nil))))

