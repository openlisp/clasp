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


;5
(defgeneric  map-equations (equations)
  (:documentation "returns Equations"))
(defmethod map-equations (equations)
  (print "Mapping Equations ")
  (print equations)

  (grid:map-grid
    :source equations
    :element-function (lambda (x) (coerce (eval-element x) 'double-float))))

;5
(defgeneric map-numbers (numbers)
  (:documentation "returns Mapped values"))
(defmethod map-numbers (numbers)
  (print "Mapping Numbers")
  (grid:map-grid
    :source numbers
    :element-function (lambda (x) (coerce x 'double-float))))










;tohle je normalni mapovani dane matice
;3
(defgeneric eval-value-linear-matrices (m)
  (:documentation "returns  Evaluation of Value Linear Matrices"))
(defmethod eval-value-linear-matrices ((m matrice-system))
  
  (print "Evaluation of Value Linear Matrices")
  
  (adjust-size (get-g-number-array m) (size m)  )
  (adjust-size (get-e-number-array m) (size m)  )  
  
  (grid:map-n-grids
    :sources
    (list
      (list (get-array (get-g-number-array m)) nil)
      (list (get-array (get-e-number-array m)) nil))
    :destination-specification `((grid:foreign-array ,(size m) ,(size m)) double-float)
    :combination-function (lambda (a b) (+ a b))))


;4

(defgeneric eval-linear-matrices (m)
  (:documentation "returns Evaluation of Linear Matrices"))
(defmethod eval-linear-matrices ((m matrice-system))
  
  (print "Evaluation of Linear Matrices")
  
  (adjust-size  (get-g-equation-array m) (size m))



   (print (eval-value-linear-matrices m))

(print "Wait For read")
(read)



  (grid:map-n-grids
    :sources
    (list
      (list (eval-value-linear-matrices m) nil)
      (list  (map-equations (get-array (get-g-equation-array m))) nil))
    :destination-specification `((grid:foreign-array ,(size m) ,(size m)) double-float)
    :combination-function (lambda (a b ) (+ a b))))



;4

(defgeneric eval-jacobian-matrice (m)
  (:documentation "returns Evaluation of Jacobian Matrice"))
(defmethod eval-jacobian-matrice ((m matrice-system))
  (print "Evaluation of Jacobian Matrice")
  (grid:map-n-grids
    :sources
    (list
      (list (eval-linear-matrices m) nil)
      (list (map-equations (get-array (get-differetial-equation-array m))) nil))
    :destination-specification `((grid:foreign-array ,(size m) ,(size m)) double-float)
    :combination-function (lambda (a b ) (+ a b))))


;4
(defgeneric eval-rhs-vector (m)
  (:documentation "returns Evaluation of RHS Vector"))
(defmethod eval-rhs-vector ((m matrice-system))
  
  (print "Evaluation of RHS Vector")
  
  (adjust-size (get-rhs-equation-vector m) (size m))
  (adjust-size (get-rhs-number-vector m) (size m))  
  
  (print (get-vector (get-rhs-equation-vector m)))
  
  (grid:map-n-grids
    :sources
    (list
      (list  (map-equations (get-vector (get-rhs-equation-vector m))) nil)
      (list  (map-numbers (get-vector (get-rhs-number-vector m )))   nil))
    
    :combination-function (lambda (a b) (+ a b) )
    :destination-specification `((grid:foreign-array ,(size m)) double-float)))




;4
(defgeneric eval-nonlinear-equation-vector (m)
  (:documentation "returns Evaluation of Nonlinear Equation Vector"))
(defmethod eval-nonlinear-equation-vector ((m matrice-system))
  (print "Evaluation of Nonlinear Equation Vector")
  
  (adjust-size (get-nonlinear-equation-vector m) (size m)  )


(print (map-equations  (get-vector (get-nonlinear-equation-vector m))))
  (map-equations  (get-vector (get-nonlinear-equation-vector m))))




;5


(defgeneric eval-linear-matrice-product (m new-value-vector)
  (:documentation "returns Evaluation of Linear Matrice Product"))
(defmethod eval-linear-matrice-product ((m matrice-system) new-value-vector)
  (print "Evaluation of Linear Matrice Product")
  
  (print (eval-value-linear-matrices m))
  (print "after eval-value-linear-matrices m") 

  (print new-value-vector)
  (print "after new-value-vector")

  (gsll:matrix-product
    (eval-value-linear-matrices m)
    new-value-vector))




; 5


(defgeneric eval-lhs-vector (m new-value-vector)
  (:documentation "return Evaluation of LHS Vector"))
(defmethod eval-lhs-vector ((m matrice-system) new-value-vector)
  
  (print "Evaluation of LHS Vector")
  
(print (eval-linear-matrice-product m new-value-vector))

    (print "Righ After eval-linear-matrice-product m new-value-vector")

(print (eval-nonlinear-equation-vector m))
    (print "Righ After eval-nonlinear-equation-vector m")


  (grid:map-n-grids
    :sources
    (list
      (list (eval-linear-matrice-product m new-value-vector) nil)
      (list (eval-nonlinear-equation-vector m) nil))
    
    :combination-function (lambda (a b) (+ a b))
    :destination-specification `((grid:foreign-array ,(size m)) double-float)))













; Eval value vector je takova specialni eval funkce ktera by mela jit neka m do mapovani

(defun eval-value-vector (value-vector rhs-vector size )
  
  (print "Evaluation of New Value Vector")
  (grid:map-n-grids
    :sources
    (list
      (list value-vector nil)
      (list rhs-vector nil))
    :destination-specification `((grid:foreign-array ,size) double-float)
    :combination-function (lambda (a b) (+ a b))))










(defun eval-nr-residual (old-rhs-vector rhs-vector)
  (-
    (gsl:euclidean-norm old-rhs-vector)
    (gsl:euclidean-norm rhs-vector)))






(defun eval-nr-residual-test (residual epsilon)
  (if (< (abs residual) epsilon)
    t
    nil))



(defun generate-random-vector(random-number-generator source-vector size)
  
  (grid:map-grid
    :source source-vector
    :destination-specification `((grid:foreign-array ,size ) double-float)
    :element-function (lambda (x) (coerce (+ x  (gsl:sample random-number-generator :gaussian :sigma 2.0d0) )  'double-float))))













(defgeneric solver-newton-raphson (m var-arr epsilon max-iter)
  (:documentation "evaluate matrix set using Newton Raphson Method"))

(defmethod solver-newton-raphson ((m matrice-system) (var-arr class-variables) epsilon max-iter)
  (format t "~%~%Solver: Newton Raphson~% Matrix-dimesion: ~d~%Epsilon: ~d~%Max Iter: ~d~%" (size m) epsilon max-iter)
  
  (let* (
      (roulete (gsl:make-random-number-generator gsl:+CMRG+ 0))
     ; (max-iter 10)  ;; only debug purposes
      (old-rhs-vector (grid:make-foreign-array 'double-float :dimensions (list (size m)) :initial-element 0.0d0))
      (new-value-vector (generate-random-vector roulete old-rhs-vector (size m) )))
    
    
    (print 1)
    (loop for iter from 0
      	 while (< iter max-iter)
      	   initially
      
      (iter:iter(iter:for k from 0 below (size m))
        (set (grid:gref (get-variable-label-vector m) (1+ k))  (grid:gref new-value-vector k )))
      
      	 do
      	
      	(print 2)
      
      (print (eval-rhs-vector m))
      	(print "2a")
      
      (print (eval-lhs-vector m new-value-vector))
      	(print "2b")
      
      
      (let* (
          
          ; (value-linear-matrices (eval-value-linear-matrices m))
          (jacobian-matrice (eval-jacobian-matrice m))
          (rhs-vector
            (grid:map-n-grids
              :sources
              (list
                (list (eval-rhs-vector m) nil)
                (list (eval-lhs-vector m new-value-vector) nil))
              
              :combination-function (lambda (a b) (- a b))
              :destination-specification `((grid:foreign-array ,(size m)) double-float))))
        
        
        (print 3)
        (format t "~%Iteration Number: ~d/~d~%" iter max-iter)
        
        (setf residual (eval-nr-residual old-rhs-vector rhs-vector))
        (print 4)
        (if (and (eval-nr-residual-test residual epsilon) (> iter 0))
          (progn
            (print-state iter new-value-vector (size m) residual)
            (return t))
          (progn
            (setf old-rhs-vector rhs-vector)))
        
        (print 5)
        (multiple-value-bind
          (jacobian-matrice perm)
          (gsl:lu-decomposition jacobian-matrice)
          (ignore-errors (gsl:lu-solve jacobian-matrice rhs-vector perm)))
        
        (print 6)
        (setf new-value-vector (eval-value-vector new-value-vector rhs-vector  (size m) ))
        
        (print 7)
        (print-state iter new-value-vector (size m) residual)
        ;        (set-symbol-var-value var-arr *time-pos* 1 (grid:gref new-value-vector (- k 1))) ; weird TODO delete
        
        (iter:iter (iter:for k from 0 below  (size m))
          (set-symbol-var-value var-arr *time-pos* k (grid:gref new-value-vector k ))
          (set (grid:gref (get-variable-label-vector m) (1+ k))  (grid:gref new-value-vector k )))
        (print 8)
        )
      
      finally
      (print-state iter new-value-vector (size m) residual)
      (incf *not-convergency*)
      (return nil))
    
    
    
    ))

