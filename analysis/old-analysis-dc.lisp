 (in-package #:clasp)


;;;
;;
;;DC simulation function
;;




(defun dc ()
;     (print "linear dc started")  
     (make-instance 'class-matrix-system)
     (print "matrix")
     (make-instance 'class-variables)
     (print "variables")
          

  (let ((m       (make-instance 'class-matrix-system))
        (v       (make-instance 'class-variables)))

    (map-all-devices m)
    ;; doprogramovat casovou osu   
;    (set-new-symbol-var-matrix v 0.0 0.1 8.0 (get-sub-stack-vector m 0 (size m))) ;will generate set of tiume poinsts
 
     (set-new-symbol-var-matrix v 0 1 1 (get-sub-stack-vector m 0 (size m))) ; will generate only one time point
 
    (print "test size ")
    (print (first (size v)))
    
    (loop for i from 0 below (first (size v)) do
    (setf *time-pos* i)
    (setf *time* (get-symbol-var-time v *time-pos*))       


    (luf m v))

    
    (get-all-symbols v)
        
    ))



;;stepped DC;


(defun stepped-dc (start step end)
  (let ((m       (make-instance 'class-matrix-system))
        (v       (make-instance 'class-variables)))
    (map-all-devices m)
       
    ;; doprogramovat casovou osu   
;    (set-new-symbol-var-matrix v 0.0 0.1 8.0 (get-sub-stack-vector m 0 (size m))) ;will generate set of tiume poinsts
 
     (set-new-symbol-var-matrix v start step end (get-sub-stack-vector m 0 (size m))) ; will generate only one time point
 
    (print "test size ")
    (print (first (size v)))
    
    (loop for i from 0 below (first (size v)) do
    (setf *time-pos* i)
    (setf *time* (get-symbol-var-time v *time-pos*))       


    (luf m v))

    
    (get-all-symbols v)
    (print      *not-convergency*)      
    ))















;
; linearni DC LUF
;
(defmethod luf-dc-linear ((m class-matrix-system) (var-arr class-variables) i)
        (print "luf linear")    
  
  
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

;
; Nelinearini DC luf
;



;(defun nonlinear-test-delta (solver absolute-error relative-error)
;  :documentation
;  "Test for the convergence of the sequence by comparing the
;   last step dx with the absolute error and relative
;   errors given to the current position x.  The test returns
;   T if the following condition is achieved:
;   |dx_i| < epsabs + epsrel |x_i|
;   for each component of x and returns NIL otherwise.")

;(defun nonlinear-test-residual (solver absolute-error)
;  :documentation			; FDL
;  "Test the residual value f against the absolute error,
;   returning T if the following condition is achieved:
;   \sum_i |f_i| < absolute_error
;   and returns NIL otherwise.  This criterion is suitable
;   for situations where the precise location of the root x is
;   unimportant provided a value can be found where the
;   residual is small enough.")



(defun print-state (iter argval argval-length residual)
(with-open-file (stream "output/dc_iter.dat" :direction :output  
                                   :if-exists :APPEND
                                   :if-does-not-exist :create)

  (format stream "~% ~d " iter) 
;  (format stream "~{~f  ~}" (coerce argval 'list)
;  (format stream "~{~f  ~}" (coerce argval 'list)
  (iter:iter (iter:for k from 0 below argval-length) 
    (format stream " ~f " (grid:gref argval k)))
 
  (format stream " ~f " residual)
;  (print iter)
;  (princ argval)
;  (princ fnval)
))
 


(defun residual-test (residual epsilon)
  (if (< (abs residual) epsilon) 
      t
      nil))
 

;Rovnice pro vypocet nahodne hodnoty v danem rozmezi
(defun random-range (start stop) 
  (+ (random (-  stop start)) start))



(defmethod luf-dc-nonlinear ((m class-matrix-system) (var-arr class-variables) i  epsilon max-iter est-type)
  (let ((linear-matrices   ;nepocitame nelinearni stav, nelinearni rovnice jsou ommitovany
          (grid:map-n-grids 
               :sources 
                  (list 
                    (list (get-sub-g-array m 1 1 i i) nil)
                    (list (get-sub-e-array m 1 1 i i) nil))
                :combination-function (lambda (a b ) (+ a b))))
         (new-value-vector   ; pro nastrel pocatecni hodnoty  
           (grid:map-n-grids 
                :sources 
                  (list 
                    (list ;RHS equations
                     ;test 2 
                     (grid:make-foreign-array 'double-float :dimensions (list i) :initial-element 0.0d0) nil )
;                      (grid:map-grid 
 ;                          :source (get-sub-rhsl-start-vector m 1 i) ;RHS equations
  ;                         :element-function (lambda (x) (coerce x 'double-float))) nil) 
                    (list ;RHS values
                      (grid:map-grid 
                           :source (get-sub-rhs-number-vector m 1 i) ;RHS values
                           :element-function (lambda (x) (coerce  x 'double-float))) nil))
                :combination-function (lambda (a b) (+ a b))
                :destination-specification `((grid:foreign-array ,i) double-float)))
           
         (old-rhs-vector (grid:make-foreign-array 'double-float :dimensions (list i) :initial-element 0.0d0))) ;some value    
            ;residual
        ; (residual epsilon))  
                
                
     (if est-type  ; asi condition
           (multiple-value-bind 
             (linear-matrices perm)
             (gsl:lu-decomposition linear-matrices)
             (gsl:lu-solve linear-matrices new-value-vector perm))           
                 
        (iter:iter (iter:for k from 0 below i) 
          (setf (grid:gref new-value-vector k) (random-range -1d0 1d0))))
             
   
 
        (print new-value-vector)
      ;  (print "current time position")
      ;  (print curr-time-pos)  
        
        (print  *time*)   


;;; Pocatek iterace
   (loop for iter from 0
	 while (< iter max-iter)
;     (when (< iter max-iter) (return nil))
 		      
	 initially  
          ;prirazeni pocatecniho nastrelu hodnotam
        (iter:iter (iter:for k from 1 below (size m)) 
          (set (grid:gref (stack m) k)  (grid:gref new-value-vector (- k 1))))	 

             ;(setf old-rhs-vector new-value-vector)

 	  ; (print-state iter new-value-vector i )
	 do
           
   (let* (
          ;Linear estimation vector
        ; (linear-estimation-vector
        ;   (gsll:matrix-product 
        ;           linear-matrices
        ;           new-value-vector
        ;           ))
        ;Jacobian
         (jacobian-matrix
           (grid:map-n-grids 
                :sources 
                  (list 
                    (list linear-matrices nil)   ;LINEAR MARICES
                    (list     
                      (grid:map-grid ;D ARRAY
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
                           
                          ; :destination-specification `((grid:foreign-array ,i) double-float)) )
                    (list 
                      (gsll:matrix-product 
                        linear-matrices
                        new-value-vector) nil) ; LINEAR VECTOR  
                    (list ;RHS lineae equations
                      (grid:map-grid 
                           :source (get-sub-equations-vector m 1 (size m))  ; NON LINEAR EQUATIONS
                           :element-function (lambda (x) (coerce (apply #'+ (mapcar #'funcall x))  'double-float))) nil))
                         ; :destination-specification `((grid:foreign-array ,i) double-float))      
               :combination-function (lambda (a b c d) (- (+ a b) (+ c d )))
               :destination-specification `((grid:foreign-array ,i) double-float) )))              
         
    ;     (print "guess")
    ;     (print    (grid:map-grid 
       ;                 :source (get-sub-stack-vector m 1 (size m))
     ;;                   :element-function (lambda (x) (coerce (eval x) 'double-float)) 
        ;                :destination-specification `((grid:foreign-array ,i) double-float)))       
;         (print linear-matrices)
        ; (print "jacobian-matrix")
        ; (print jacobian-matrix)
        ; (print "rhs-vector")
        ; (print rhs-vector)
         ;(grid:map-grid 
         ;                  :source (get-sub-rhs-number-vector m 1 i) ;RHS values
         ;                  :element-function (lambda (x) (coerce  x 'double-float)))) 

;         (print "jacobian-matrix")         
;         (print jacobian-matrix)
;         (print "(get-sub-d-array m 1 1 i i)")
;         (print (grid:map-grid 
;                           :source (get-sub-d-array m 1 1 i i)  ; NON LINEAR EQUATIONS
;                           :element-function (lambda (x) (coerce (apply #'+ (mapcar #'funcall x))  'double-float))))
;          (print "aref")                
         ; (print (aref (d-array (arrays m)) 4 5))
          
        ;  (print (aref (d-array (arrays m)) 5 5))
          
;          (print (aref (d-array (arrays m)) 5 4))
          
         ; (print (aref (d-array (arrays m)) 4 4))              
 ;         (print "gesss")                 
 ;         (print (grid:map-grid 
 ;                       :source (get-sub-stack-vector m 1 (size m))
 ;                       :element-function (lambda (x) (coerce (eval x) 'double-float)) 
 ;                       :destination-specification `((grid:foreign-array ,i) double-float)))          
                           
;         (print "rhs-vector")         
;         (print rhs-vector) 
   
 ;  (print (gsl:euclidean-norm old-rhs-vector))
 ;  (print (gsl:euclidean-norm rhs-vector))
       (let ((residual  
               (-        
               (gsl:euclidean-norm old-rhs-vector) 
               (gsl:euclidean-norm rhs-vector) )))   
                
;           (print "residual")  
           (if (and (residual-test residual epsilon) (> iter 0)) 
                (progn
                  (print-state iter new-value-vector i residual)
                  (return t))
                (progn
                ;  (print-state iter new-value-vector i residual) 
                  (setf old-rhs-vector rhs-vector))))
           
;         (print "residual done")
         ; pretoceni vektroru      
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
                
                
                 
     ;   (print "new-value")          
     ;   (print new-value-vector)
;        (print "new-rhs")
        
;        (print rhs-vector)
;        (print "sub stack vector")
        
;(iter:iter (iter:for k from 1 below (size m)) 
;          (princ (eval (grid:gref (stack m) k)))
        ;  (set-symbol-var-value var-arr *time* k (grid:gref new-value (- k 1)))
;            )
;        (print " ")
;        (print "rhs sub vector")
;        (print (get-sub-rhs-equations-vector  m 1))
      
        
        
        (iter:iter (iter:for k from 1 below (size m)) 
          (set (grid:gref (stack m) k)  (grid:gref new-value-vector (- k 1)))
          (set-symbol-var-value var-arr *time-pos* k (grid:gref new-value-vector (- k 1)))
            )
             

        ) finally           
          (print-state iter new-value-vector i residual)
          (incf *not-convergency*)
          (return nil))


        
        ))





;reseni ridke matice
(defmethod luf ((m class-matrix-system) (var-arr class-variables))
  (let ((i  (1- (size m))))
     (print "luf")
    (if (linear m)
         (luf-dc-linear       m var-arr i)
         (if (luf-dc-nonlinear    m var-arr i *residual* *max-dc-iter* nil)
             t
             (luf-dc-nonlinear    m var-arr i *residual* *max-dc-iter* nil)) 
         )
         ))
          
           


      




  

