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
    
    
    (print "MAp all devices started")
    
    (map-all-devices m)
    
    (print (get-variable-label-vector m ))
    
    (print "hr 1a size m")
    ;(print (size m))
    
    ;; Time point generation
    (set-new-symbol-var-matrix v start step stop (get-variable-label-vector m )) ; will generate only one time point
    
    (print "hr 2b")
    ;iterate over all time values, in this case only one value
    (loop for i from 0 below (first (size v)) do
      (setf *time-pos* i)
      (setf *time* (get-symbol-var-time v *time-pos*))
      (dc-solve m v solver))
    (get-all-symbols v)))








