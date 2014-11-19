 (in-package #:clasp)

;
; Evolutionary Newton raphson

(defmethod solver-evolutionary-newton-raphson ((m matrice-system) (var-arr class-variables) i pop-size epsilon max-iter)
  (let (
    (linear-matrices   
      (grid:map-n-grids 
           :sources 
             (list 
               (list (get-sub-g-array m 1 1 i i) nil)
               (list (get-sub-e-array m 1 1 i i) nil))
           :combination-function (lambda (a b ) (+ a b))))
    (new-value-vector     
               (grid:make-foreign-array 'double-float :dimensions (list i) :initial-element 0.0d0))

    (pop-value-vector  (grid:make-foreign-array 'double-float :dimensions (list pop-size i ) :initial-element 0.0d0))
    (sub-iter-loop 0) 
    (residual (make-array pop-size)) 
    (pop-indexes (loop for i from 0 below pop-size collect i))    
    (average 0))

; random uniformly distributed values

(LET ((RNG (gsl:MAKE-RANDOM-NUMBER-GENERATOR gsl:+mt19937+ (random 3))))
    (iter:iter (iter:for col from 0 below i)
      (setf (grid:column pop-value-vector col)        
      (grid:make-foreign-array 'double-float :dimensions pop-size :initial-contents      	    
        (LOOP FOR I FROM 0 below pop-size COLLECT (*  (- (gsl:sample rng :uniform) 0.5)  2) )))))

   (loop for iter-loop from 0
	 while (< iter-loop max-iter) ;max-iter
	 do  
	   (setf pop-indexes (delete-if #'(lambda (x) (< (/ average pop-size) (aref residual x ))) pop-indexes)) 
	 (setf pop-size 0)
	 (setf average 0)

	 (loop for pers in pop-indexes
	   do

             	   (incf pop-size) 
       (iter:iter (iter:for k from 1 below (size m))       
   	        (set (grid:gref (stack m) k)  (grid:gref pop-value-vector pers (- k 1)  )))
;            (setf (grid:gref new-value-vector k) (grid:gref pop-value-vector pers (- k 1)  )))

         (setf new-value-vector
          (grid:map-grid :source (grid:row pop-value-vector pers)))


       (let* ( ;sestaveni jakobiho matice
       

          
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
                :destination-specification `((grid:foreign-array ,i) double-float)))
                
             (old-rhs-vector (grid:map-grid :source rhs-vector)))
               
         

       (multiple-value-bind 
         (jacobian-matrix perm)
         (gsl:lu-decomposition jacobian-matrix)
         (ignore-errors (gsl:lu-solve jacobian-matrix rhs-vector perm)))
       (setf (grid:row pop-value-vector pers)
                (grid:map-n-grids 
                  :sources 
                     (list 
                       (list new-value-vector nil)
                       (list rhs-vector nil))            
                  :combination-function (lambda (a b) (+ a b))))
        (setf (aref residual pers )
           (abs  
           (-        
             (gsl:euclidean-norm old-rhs-vector) 
             (gsl:euclidean-norm rhs-vector))))
     ;   (princ (aref residual pers ))
        (setf average  (+ average (aref residual pers ))))) ; konec generace pop indexes
        (setf pop-indexes (delete-duplicates pop-indexes :key #'(lambda (x) (aref residual x ) ))) 
        (setf sub-iter-loop (+ sub-iter-loop pop-size))
         (if (and (residual-test (aref residual (first pop-indexes)) epsilon) (eq pop-size 1))
           (progn
 ;            (print "Vyhra")
             (print-state-genetic iter-loop sub-iter-loop new-value-vector i (aref residual (first pop-indexes)))
             (iter:iter (iter:for k from 1 below (size m)) 
             (set-symbol-var-value var-arr *time-pos* k (grid:gref new-value-vector (- k 1)))) 
             (return t)))

     finally           
       (print-state-genetic iter-loop sub-iter-loop new-value-vector i (aref residual (first pop-indexes)))
       (incf *not-convergency*)
       (return nil) )))

