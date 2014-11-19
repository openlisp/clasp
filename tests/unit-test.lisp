
(require "asdf")

(load "~/quicklisp/setup.lisp")
(ql:quickload "gsll")

(ql:quickload "antik")




(setf i 10)

(setq aaa 

     (grid:make-foreign-array 'double-float :dimensions '(i)))

(setq a 

   (MULTIPLE-VALUE-LIST
    (LET ((RNG (gsl:MAKE-RANDOM-NUMBER-GENERATOR gsl:+MT19937+ 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT
	   (gsl:sample rng :gaussian :sigma 10.0d0)))))


(setq b

   (grid:make-foreign-array 
   (MULTIPLE-VALUE-LIST
    (LET ((RNG (gsl:MAKE-RANDOM-NUMBER-GENERATOR gsl:+CMRG+ 0)))
      (LOOP FOR I FROM 0 TO 10 COLLECT (gsl:sample rng :uniform)))))



;(setf new-value-vector
;(grid:map-grid
;:source new-value-vector
;:destination-specification `((grid:foreign-array ,i) double-float)
;:element-function #'(lambda (x) (* (- (gsl:sample roulete :gaussian) x) 2) ))) 


(print a)


(print b)







(quit)
