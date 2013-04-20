(in-package #:clasp)

(defun residual-test (residual epsilon)
  (if (< (abs residual) epsilon) 
    t
    nil))



(defmethod solver-bdf-0 ((m class-matrix-system) (vtrans class-variables) i h epsilon max-iter)
    (print "bdf0 - 0")
  (let* (
     

;    (pred-coef #(0 0 0 0 0 0) ;koeficienty predictoru
;    (corr-coef #(0 0 0 0 0 0) ;koeficienty correctoru    
    (residual 0)
    (return-value 0 )

    (integ-matrices   
      (grid:map-n-grids 
           :sources 
             (list 
               (list (get-sub-e-array m 1 1 i i) nil)
               (list (get-sub-z-array m 1 1 i i) nil))
           :combination-function (lambda (a b ) (+ a b))))



;      (grid:map-n-grids 
;           :sources 
;             (list 
;               (list (get-sub-g-array m 1 1 i i) nil)
;               (list  nil))
;           :combination-function (lambda (a b ) (+ a b))))



    (new-value-vector     
              (get-sub-symbol-var-vector vtrans (- *time-pos* 1) 1 i))

    (new-value-vector-deriv
              (grid:make-foreign-array 'double-float :dimensions (list i) :initial-element 0.0d0))
      
    
    ; here should be probably some random pregenerated values, or values from previous DC analysis
    (old-rhs-vector (grid:make-foreign-array 'double-float :dimensions (list i) :initial-element 0.5d0)))



       (print "bdf0 1")

;
; Generation of DC solution of circuit
;
   (loop for iter from 0
	 while (< iter max-iter)
	   initially  
        (iter:iter (iter:for k from 1 below (size m)) 
          (set (grid:gref (stack m) k)  (grid:gref new-value-vector (- k 1))))	 
	 do 
       (let* (
         (jacobian-matrix
           (grid:map-n-grids 
                :sources 
                  (list 
                    (list 
                      (get-sub-g-array m 1 1 i i) nil)   ; linearni matice


                                         
                    (list     
                      (grid:map-grid 
                           :source (get-sub-d-array m 1 1 i i)
                           :element-function (lambda (x) (coerce (apply #'+ (mapcar #'funcall x))  'double-float)))  nil)
                           
                           ) 
                :combination-function (lambda (a b) (+ a b )))) 
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
                        (get-sub-g-array m 1 1 i i)  
                        new-value-vector) nil) ; LINEAR VECTOR  

                    (list   ;mozna ze by to tu melo byt
                      (gsll:matrix-product 
                        integ-matrices ;(get-sub-z-array m 1 1 i i)
                        new-value-vector-deriv) nil) ; LINEAR VECTOR  

                    (list ;RHS lineae equations
                      (grid:map-grid 
                           :source (get-sub-equations-vector m 1 (size m))  ; NON LINEAR EQUATIONS
                           :element-function (lambda (x) (coerce (apply #'+ (mapcar #'funcall x))  'double-float))) nil))  
               
                :combination-function (lambda (a b c d e) (- (+ a b) (+ c d e)))
                :destination-specification `((grid:foreign-array ,i) double-float))))              
;         (print "pos3")
      ; (let (

      (print "bdf 1 4")
      (print rhs-vector)
      (print "jacobian")
      (print jacobian-matrix)

         (print "residual")
         (print residual)
       
       (print "bdf0  2") 
         (setf residual  
           (-        
             (gsl:euclidean-norm old-rhs-vector) 
             (gsl:euclidean-norm rhs-vector) ))   
         
         (if (and (residual-test residual epsilon) (> iter 0)) 
           (progn
             (print-state iter new-value-vector i residual)
             (setf return-value 1)
             (return t))
           (progn
             (setf old-rhs-vector rhs-vector)))
             
             
      
             
            
       (print "bdf0 3") 
             
             
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
                  :combination-function (lambda (a b) (+ a b))))
                  
      (set-symbol-var-value vtrans *time-pos* 1 (grid:gref new-value-vector (- 1 1)))
       
       (iter:iter (iter:for k from 1 below (size m)) 
       
         (set-symbol-var-value vtrans *time-pos* k (grid:gref new-value-vector (- k 1)))
         (set (grid:gref (stack m) k)  (grid:gref new-value-vector (- k 1)))
         )))
         

         return-value 
    
 
         
         
         ;(print "integration order 0 finishes")
         
         ))






























(defmethod solver-bdf-1 ((m class-matrix-system) (vtrans class-variables) i h epsilon max-iter)
   (print "bdf 1 0")
  (let* (
   
    (tau2 (/ (- (get-symbol-var-time vtrans *time-pos*) (get-symbol-var-time vtrans (- *time-pos* 1)))  h)) 
    (a1p (/ tau2 (- tau2 1)))
    (a2p (/ -1   (- tau2 1 )))
    (a0c -1)
    (a1c  1)
  
 
;    (pred-coef #(0 0 0 0 0 0) ;koeficienty predictoru
;    (corr-coef #(0 0 0 0 0 0) ;koeficienty correctoru    
    (residual 0)       

;      (grid:map-n-grids 
;           :sources 
;             (list 
;               (list (get-sub-g-array m 1 1 i i) nil)
;               (list  nil))
;           :combination-function (lambda (a b ) (+ a b))))

    (integ-matrices   
      (grid:map-n-grids 
           :sources 
             (list 
               (list (get-sub-e-array m 1 1 i i) nil)
               (list (get-sub-z-array m 1 1 i i) nil))
           :combination-function (lambda (a b ) (+ a b))))



    (new-value-vector     
         (grid:map-n-grids 
                :sources 
                  (list 
                    (list (get-sub-symbol-var-vector vtrans (- *time-pos* 1) 1 i) nil)
                    (list (get-sub-symbol-var-vector vtrans (- *time-pos* 2) 1 i) nil))  
                :combination-function (lambda (xn1 xn2) (+ (* a1p xn1) (* a2p xn2))))) ; vzdy ten nejnovejsi ma nejnizsi koef a1
  

    (new-value-vector-deriv
         (grid:map-n-grids 
                :sources 
                  (list 
                    (list new-value-vector nil)
                    (list (get-sub-symbol-var-vector vtrans (- *time-pos* 1) 1 i) nil))  
                :combination-function (lambda (xn xn1) (* (- (/ 1 h)) (+ (* a0c xn) (* a1c xn1))))))


      

    
    
    (old-rhs-vector (grid:make-foreign-array 'double-float :dimensions (list i) :initial-element 0.5d0)))



   (print "bdf 1 1")   
   (print new-value-vector)
   (print new-value-vector-deriv)
;
; Generation of DC solution of circuit
;
   (loop for iter from 0
	 while (< iter max-iter)
	   initially  
        (iter:iter (iter:for k from 1 below (size m)) 
          (set (grid:gref (stack m) k)  (grid:gref new-value-vector (- k 1))))	 
	 do
	   (print (get-sub-z-array m 1 1 i i) ) 
       (let* (
         (jacobian-matrix
           (grid:map-n-grids 
                :sources 
                  (list 
                    (list 
                      (get-sub-g-array m 1 1 i i) nil)   ; linearni matice


                    (list     
                      (grid:map-grid 
                           :source integ-matrices   ;integracni matice z
                           :element-function (lambda (x) (coerce  (- (* x (/ a0c h)))  'double-float)))  nil)
                               
                    (list     
                      (grid:map-grid 
                           :source (get-sub-d-array m 1 1 i i)
                           :element-function (lambda (x) (coerce (apply #'+ (mapcar #'funcall x))  'double-float)))  nil)
                           
                           ) 
                :combination-function (lambda (a b c) (+ a b c)))) 
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
                        (get-sub-g-array m 1 1 i i)  
                        new-value-vector) nil) ; LINEAR VECTOR  

                    (list 
                      (gsll:matrix-product 
                        integ-matrices
                        new-value-vector-deriv) nil) ; LINEAR VECTOR  

                    (list ;RHS lineae equations
                      (grid:map-grid 
                           :source (get-sub-equations-vector m 1 (size m))  ; NON LINEAR EQUATIONS
                           :element-function (lambda (x) (coerce (apply #'+ (mapcar #'funcall x))  'double-float))) nil))  
                :combination-function (lambda (a b c d e) (-  (+ a b) (+ c d e)))
                :destination-specification `((grid:foreign-array ,i) double-float))))              
;         (print "pos3")
      ; (let  (
      (print "bdf 1 4")
      (print rhs-vector)
      (print "jacobian")
      (print jacobian-matrix)
      
         (setf residual  
           (-        
             (gsl:euclidean-norm old-rhs-vector) 
             (gsl:euclidean-norm rhs-vector) ))
             
         (print "residual")
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
                  :combination-function (lambda (a b) (+ a b))))
        (setf new-value-vector-deriv
               (grid:map-n-grids 
                :sources 
                  (list 
                    (list new-value-vector nil)
                    (list (get-sub-symbol-var-vector vtrans (- *time-pos* 1) 1 i) nil))  
                :combination-function (lambda (xn xn1) (* (- (/ 1 h)) (+ (* a0c xn) (* a1c xn1))))))           
                  
                  
                  
      (set-symbol-var-value vtrans *time-pos* 1 (grid:gref new-value-vector (- 1 1)))
       (iter:iter (iter:for k from 1 below (size m)) 
         (set-symbol-var-value vtrans *time-pos* k (grid:gref new-value-vector (- k 1)))
         (set (grid:gref (stack m) k)  (grid:gref new-value-vector (- k 1)))
         )))
         


         
         
         (print "integration order 1 finishes")))
       





;
; New direction bdf of order 2
;

(defmethod solver-bdf-2 ((m class-matrix-system) (vtrans class-variables) i h epsilon max-iter)
  (print "bdf 2 0")
  (let* (
;    (h (time-axis-steps vtrans))
    (tau2 (/ (- (get-symbol-var-time vtrans *time-pos*) (get-symbol-var-time vtrans (- *time-pos* 1)))  h)) 
    (tau3 (/ (- (get-symbol-var-time vtrans *time-pos*) (get-symbol-var-time vtrans (- *time-pos* 2)))  h)) 


    (big-dp (* (- tau3 tau2) (+ 1 (* tau2 tau3) (- tau2) (- tau3))))
    (big-dc (- (* tau2 tau2) tau2))
     
    (a1p (/ (- (* tau2 tau3 tau3) (* tau2 tau3 tau2))    big-dp))
    (a2p (/ (- tau3 (* tau2 tau3))  big-dp))
    (a3p (/ (- (* tau2 tau2) tau2)  big-dp))
    
    (a0c (/ (- 1 (* tau2 tau2)) big-dc))
    (a1c (/ (* tau2 tau2) big-dc))
    (a2c (/ -1 big-dc))
  

    (integ-matrices   
      (grid:map-n-grids 
           :sources 
             (list 
               (list (get-sub-e-array m 1 1 i i) nil)
               (list (get-sub-z-array m 1 1 i i) nil))
           :combination-function (lambda (a b ) (+ a b))))


 
;    (pred-coef #(0 0 0 0 0 0) ;koeficienty predictoru
;    (corr-coef #(0 0 0 0 0 0) ;koeficienty correctoru    
    (residual 0)       
    (new-value-vector     
         (grid:map-n-grids 
                :sources 
                  (list 
                    (list (get-sub-symbol-var-vector vtrans (- *time-pos* 1) 1 i) nil)
                    (list (get-sub-symbol-var-vector vtrans (- *time-pos* 2) 1 i) nil)  
                    (list (get-sub-symbol-var-vector vtrans (- *time-pos* 3) 1 i) nil)) 
                :combination-function (lambda (xn1 xn2 xn3) (+ (* a1p xn1) (* a2p xn2) (* a3p xn3) ))))
  
    (new-value-vector-deriv
         (grid:map-n-grids 
                :sources 
                  (list 
                    (list new-value-vector nil)                    
                    (list (get-sub-symbol-var-vector vtrans (- *time-pos* 1) 1 i) nil)
                    (list (get-sub-symbol-var-vector vtrans (- *time-pos* 2) 1 i) nil))  
                :combination-function (lambda (xn xn1 xn2) (* (- (/ 1 h)) (+ (* a0c xn) (* a1c xn1) (* a2c xn2) )))))
    
    (old-rhs-vector (grid:make-foreign-array 'double-float :dimensions (list i) :initial-element 0.5d0)))
;
; Generation of DC solution of circuit
;
   (loop for iter from 0
	 while (< iter max-iter)
	   initially  
        (iter:iter (iter:for k from 1 below (size m)) 
          (set (grid:gref (stack m) k)  (grid:gref new-value-vector (- k 1))))	 
	 do 
       (let* (
         (jacobian-matrix
           (grid:map-n-grids 
                :sources 
                  (list 
                    (list 
                      (get-sub-g-array m 1 1 i i) nil)   ; linearni matice


                    (list     
                      (grid:map-grid 
                           :source integ-matrices   ;integracni matice z
                           :element-function (lambda (x) (coerce  (- (* x (/ a0c h)))  'double-float)))  nil)
                               
                    (list     
                      (grid:map-grid 
                           :source (get-sub-d-array m 1 1 i i)
                           :element-function (lambda (x) (coerce (apply #'+ (mapcar #'funcall x))  'double-float)))  nil)
                           
                           ) 
                :combination-function (lambda (a b c) (+ a b c)))) 
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
                        (get-sub-g-array m 1 1 i i)  
                        new-value-vector) nil) ; LINEAR VECTOR  

                    (list 
                      (gsll:matrix-product 
                        integ-matrices
                        new-value-vector-deriv) nil) ; LINEAR VECTOR  

                    (list ;RHS lineae equations
                      (grid:map-grid 
                           :source (get-sub-equations-vector m 1 (size m))  ; NON LINEAR EQUATIONS
                           :element-function (lambda (x) (coerce (apply #'+ (mapcar #'funcall x))  'double-float))) nil))  
                :combination-function (lambda (a b c d e) (- (+ a b) (+ c d e)))
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
                  :combination-function (lambda (a b) (+ a b))))
      (set-symbol-var-value vtrans *time-pos* 1 (grid:gref new-value-vector (- 1 1)))
       (iter:iter (iter:for k from 1 below (size m)) 
         (set-symbol-var-value vtrans *time-pos* k (grid:gref new-value-vector (- k 1)))
         (set (grid:gref (stack m) k)  (grid:gref new-value-vector (- k 1)))
         )))
         
         (print "integration order 2 finishes")))










 
 



