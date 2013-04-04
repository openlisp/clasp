 (in-package #:clasp)


;
; Nelinearini nr
;

(defun residual-test (residual epsilon)
  (if (< (abs residual) epsilon) 
    t
    nil))

(defmethod solver-newton-raphson ((m class-matrix-system) (var-arr class-variables) i  epsilon max-iter)
(print "solver-newton-raphson")
  (let (
    (linear-matrices   
      (grid:map-n-grids 
           :sources 
             (list 
               (list (get-sub-g-array m 1 1 i i) nil)
               (list (get-sub-e-array m 1 1 i i) nil))
           :combination-function (lambda (a b ) (+ a b))))
    (residual 0)       
    (new-value-vector     
               (grid:make-foreign-array 'double-float :dimensions (list i) :initial-element 0.0d0)) 
    (old-rhs-vector (grid:make-foreign-array 'double-float :dimensions (list i) :initial-element 0.5d0))) 

(LET ((RNG (gsl:MAKE-RANDOM-NUMBER-GENERATOR gsl:+mt19937+ 1)))
    (setf new-value-vector 
             (grid:map-grid  
                           :source new-value-vector     
                           :element-function #'(lambda (x) (* (- (gsl:sample rng :uniform)  x) 2) ))))     


;(print "Pos1")
   (loop for iter from 0
	 while (< iter max-iter)
	   initially  
        (iter:iter (iter:for k from 1 below (size m)) 
          (set (grid:gref (stack m) k)  (grid:gref new-value-vector (- k 1))))	 
	 do 
	   
;	   (print (get-sub-d-array m 1 1 i i))
 ;      (print "Pos2")
       (let* (
         (jacobian-matrix
           (grid:map-n-grids 
                :sources 
                  (list 
                    (list linear-matrices nil)  
                    (list     
                      (grid:map-grid 
                           :source (get-sub-d-array m 1 1 i i)
                           :element-function (lambda (x) (coerce (apply #'+ (mapcar #'funcall x))  'double-float)))  nil)) 
                :combination-function (lambda (a b) (+ a b)))) 
         ;RHS Vector            
         (rhs-vector 
           (grid:map-n-grids 
                :sources 
                  (list 
                    (list ;RHS equations
                      (grid:map-grid 
                           :source (get-sub-rhs-equations-vector m 1 i) ;RHS equations
                           :element-function (lambda (x) (coerce (apply #'+ (mapcar #'funcall x)) 'double-float))) nil) 
                    (list ;RHS values
                      (grid:map-grid 
                           :source (get-sub-rhs-number-vector m 1 i) ;RHS values
                           :element-function (lambda (x) (coerce  x 'double-float))) nil)                           
                    (list 
                      (gsll:matrix-product 
                        linear-matrices
                        new-value-vector) nil) ; LINEAR VECTOR  
                    (list ;RHS lineae equations
                      (grid:map-grid 
                           :source (get-sub-equations-vector m 1 (size m))  ; NON LINEAR EQUATIONS
                           :element-function (lambda (x) (coerce (apply #'+ (mapcar #'funcall x))  'double-float))) nil))  
                :combination-function (lambda (a b c d) (- (+ a b) (+ c d )))
                :destination-specification `((grid:foreign-array ,i) double-float))))              
;         (print "pos3")
      ; (let (
         (setf residual  
           (-        
             (gsl:euclidean-norm old-rhs-vector) 
             (gsl:euclidean-norm rhs-vector) ))   
         
         (if (and (residual-test residual epsilon) (> iter 0)) 
           (progn
             (print-state iter new-value-vector i residual)
             (return t))
           (progn
             (setf old-rhs-vector rhs-vector)))
     ;    (print "pos4")
       (multiple-value-bind 
         (jacobian-matrix perm)
         (gsl:lu-decomposition jacobian-matrix)
         (ignore-errors (gsl:lu-solve jacobian-matrix rhs-vector perm)))           
      ;  (print "pos5")                  
       (setf new-value-vector 
                (grid:map-n-grids 
                  :sources 
                     (list 
                       (list new-value-vector nil)
                       (list rhs-vector nil))            
                  :combination-function (lambda (a b) (+ a b))))
        
   ;      (print "nechapu")
      (set-symbol-var-value var-arr *time-pos* 1 (grid:gref new-value-vector (- 1 1)))
  ;       (print "blbost")
       (iter:iter (iter:for k from 1 below (size m)) 
         (set-symbol-var-value var-arr *time-pos* k (grid:gref new-value-vector (- k 1)))
         (set (grid:gref (stack m) k)  (grid:gref new-value-vector (- k 1)))
         
   ;        (print "co to je")        
         ))
         
     finally           
       (print-state iter new-value-vector i residual)
       (incf *not-convergency*) 
       (return nil))))
