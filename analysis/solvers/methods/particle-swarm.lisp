 (in-package #:clasp)

;
;Particle swarm
;

(defmethod solver-particle-swarm ((m matrice-system) (var-arr class-variables) i epsilon max-generations pop-size c1 c2 space-size)
; Initiate
  (let (
    (sub-iter-loop pop-size)
    (linear-matrices   
      (grid:map-n-grids 
           :sources 
             (list 
               (list (get-sub-g-array m 1 1 i i) nil)
               (list (get-sub-e-array m 1 1 i i) nil))
           :combination-function (lambda (a b ) (+ a b))))

    (pop-pos-matrix  (grid:make-foreign-array 'double-float :dimensions (list pop-size i ) :initial-element 0.0d0))

    (best-pos-matrix  (grid:make-foreign-array 'double-float :dimensions (list pop-size i ) :initial-element 0.0d0))
    (residual-best-pos (make-array pop-size)) 

    (pop-velocity-matrix  (grid:make-foreign-array 'double-float :dimensions (list pop-size i ) :initial-element 0.0d0))


    (residual-best-pop 0) 
    (best-pop-vector  (grid:make-foreign-array 'double-float :dimensions (list i ) :initial-element 0.0d0))


   (rng (gsl:MAKE-RANDOM-NUMBER-GENERATOR gsl:+mt19937+ (random pop-size))))
   
     (iter:iter (iter:for col from 0 below i)
      (setf (grid:column pop-pos-matrix col)        
      (grid:make-foreign-array 'double-float :dimensions pop-size :initial-contents      	    
        (LOOP FOR I FROM 0 below pop-size COLLECT (*  (- (gsl:sample rng :uniform) 0.5)  )))))

; Main iteration algoritm
   (loop for iter-loop from 0
	 while (< iter-loop max-generations) 
	 do 
      (loop for pers from 0 below pop-size 
	   do
         (iter:iter (iter:for k from 1 below (size m))       
   	        (set (grid:gref (stack m) k)  (grid:gref pop-pos-matrix pers (- k 1)  )))
;set row
        ; (setf new-value-vector
;         (grid:map-grid :source (grid:row pop-pos-matrix pers)))  

       (let (
         (fitness-vector 
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
                        (grid:map-grid :source (grid:row pop-pos-matrix pers))) nil) ; LINEAR VECTOR  
                    (list ;RHS lineae equations
                      (grid:map-grid 
                           :source (get-sub-equations-vector m 1 (size m))  ; NON LINEAR EQUATIONS
                           :element-function (lambda (x) (coerce (apply #'+ (mapcar #'funcall x))  'double-float))) nil))  
                :combination-function (lambda (a b c d) (- (+ a b) (+ c d )))
                :destination-specification `((grid:foreign-array ,i) double-float))))
                

         ;vypocet residua
        (let ((residual 
           (abs            
             (gsl:euclidean-norm fitness-vector))))
          (print "residual:")   
          (print residual)   
         ;nsatavi nejlepsiho vysledek pokud je lepsi nez predelsy vysledek    
         (cond 
           ((and (< pers 1) (<  iter-loop 1))  
             (setf (aref residual-best-pos pers) residual)         
             (setf (grid:row best-pos-matrix pers)
               (grid:map-grid :source (grid:row pop-pos-matrix pers))))              
           ((< residual (aref residual-best-pos pers) )
             (setf (aref residual-best-pos pers) residual)         
             (setf (grid:row best-pos-matrix pers)
               (grid:map-grid :source (grid:row pop-pos-matrix pers)))))  

         ;nastavi nejlepsi jedinec v cele generaci pokud je lepsi nez soucasny nejlepsi jedinec    
         (cond          
           ((and (< pers 1) (<  iter-loop 1))
              (setf residual-best-pop residual)         
              (setf best-pop-vector
                (grid:map-grid :source (grid:row pop-pos-matrix pers))))
           ((< residual residual-best-pop )
              (setf residual-best-pop residual)         
              (setf best-pop-vector
                (grid:map-grid :source (grid:row pop-pos-matrix pers)))))

        )))


;; PSO main algorithm

   (let (
     (rng 
       (gsl:MAKE-RANDOM-NUMBER-GENERATOR gsl:+mt19937+ (random pop-size))))

     (iter:iter (iter:for pers from 0 below pop-size)       
       (iter:iter (iter:for k from 0 below i)       
         (setf
           (grid:gref pop-velocity-matrix pers k)
             (+
               (grid:gref pop-velocity-matrix pers k)
               (* 
                  
                 (gsl:sample rng :uniform)          
                 c1
                 (-
                   (grid:gref best-pos-matrix pers k) 
                   (grid:gref pop-pos-matrix pers k)))
               (* 
                 (gsl:sample rng :uniform)                       
                 c2
                 (-
                   (grid:gref best-pop-vector k) 
                   (grid:gref pop-pos-matrix pers k))))))))
                  
                  
       (setf pop-pos-matrix 
           (grid:map-n-grids 
                :sources 
                  (list 
                    (list pop-velocity-matrix nil) 
                    (list pop-pos-matrix nil))
                :combination-function (lambda (a b) (+ a b))))
                
           (print "new generation")

     finally           
       
       (print-state-genetic iter-loop sub-iter-loop  best-pop-vector i residual-best-pop)
       (return t))))

