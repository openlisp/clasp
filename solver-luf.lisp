 (in-package #:clasp)



;
; linearni DC LUF
;
(defmethod solver-luf ((m class-matrix-system) (var-arr class-variables) i)  
  (let ((matrix 
          (grid:map-n-grids 
               :sources
                 (list 
                   (list (get-sub-g-array m 1 1 i i)  nil)
                   (list (get-sub-e-array m 1 1 i i)  nil))                          
               :combination-function (lambda (a b) (+ a b))))               
        (rhs-vector
          (grid:map-n-grids
               :sources
                 (list
                    (list 
                      (grid:map-grid 
                           :source (get-sub-rhs-equations-vector m 1 i) ;RHS equations
                           :element-function (lambda (x) (coerce (apply #'+ (mapcar #'funcall x)) 'double-float))) nil) 
                    (list 
                      (grid:map-grid 
                           :source (get-sub-rhs-number-vector m 1 i) ;RHS values
                           :element-function (lambda (x) (coerce  x 'double-float))) nil)) 
                 :destination-specification `((grid:foreign-array ,i) double-float)
                 :combination-function (lambda (a b) (+ a b)))))
               
        (print "lu solve")    
        (multiple-value-bind 
          (matrix perm)
          (gsl:lu-decomposition matrix)
          (gsl:lu-solve matrix rhs-vector perm))
        (set-sub-symbol-var-vector var-arr *time-pos* 1 rhs-vector)
        (print "lu solve done")
        (iter:iter (iter:for k from 1 below (size m)) 
          (set (grid:gref (stack m) k) (grid:gref rhs-vector (- k 1)))) 
        (print "linear DC done")))

