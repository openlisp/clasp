
(require "asdf")

(load "~/quicklisp/setup.lisp")

(ql:quickload "gsll")
(ql:quickload "antik")



;(let* (
;    (gen (gsl:make-quasi-random-number-generator gsl:+sobol+ 5))
;    (roulete (gsl:make-random-number-generator gsl:+CMRG+ 0))
;    (vec (grid:make-foreign-array 'double-float :dimensions 10))
;    (randomvec (loop for i from 0 to 10 collect (gsl:sample roulete :gaussian :sigma 10.0d0))))
;
;  (print randomvec)
;  (print
;    (grid:map-grid
;
;      :source vec
;      ;:destination-specification `((grid:foreign-array 10 ) double-float)
;      :element-function (lambda (x) (coerce (+ x  (gsl:sample roulete :gaussian :sigma 10.0d0) )  'double-float))))
;
;  (print (loop repeat 2 do (gsl:qrng-get gen vec) append
;      (coerce (grid:copy-to vec) 'list))))





(setf k '(1 #'+ #'6 4 5 6))
(print "slice 2 4")
(print (slice k 2 4))
(print k)





(quit)
