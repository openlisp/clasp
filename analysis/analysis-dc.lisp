 (in-package #:clasp)

(defun dc ( &optional solver)

(print "Starting DC analysis")

  (make-instance 'matrice-system)
(print "hr 1")
  (make-instance 'class-variables)    
(print "hr 1")
      
  (let ((m       (make-instance 'matrice-system))
        (v       (make-instance 'class-variables))
        (start 0 )
        (step  1 )
        (stop  1 ))
    (map-all-devices m)
    
    ;(print (get-sub-stack-vector m 0 (size m)))

(print "hr 1")
(print (size m))

;; Time point generation
    (set-new-symbol-var-matrix v start step stop (get-sub-stack-vector m 0 (size m))) ; will generate only one time point

(print "hr 2")
;iterate over all time values, in this case only one value
    (loop for i from 0 below (first (size v)) do
      (setf *time-pos* i)
      (setf *time* (get-symbol-var-time v *time-pos*))
      (dc-solve m v solver))
    (get-all-symbols v)))








