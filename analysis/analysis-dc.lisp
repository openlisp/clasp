 (in-package #:clasp)

(defun dc ( &optional solver)
  (make-instance 'class-matrix-system)
  (make-instance 'class-variables)          
  (let ((m       (make-instance 'class-matrix-system))
        (v       (make-instance 'class-variables))
        (start 0 )
        (step  1 )
        (stop  1 ))
    (map-all-devices m)
    
    ;(print (get-sub-stack-vector m 0 (size m)))
    (set-new-symbol-var-matrix v start step stop (get-sub-stack-vector m 0 (size m))) ; will generate only one time point
    (loop for i from 0 below (first (size v)) do
      (setf *time-pos* i)
      (setf *time* (get-symbol-var-time v *time-pos*))
      (dc-solve m v solver))
    (get-all-symbols v)))




