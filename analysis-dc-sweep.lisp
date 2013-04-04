 (in-package #:clasp)


;dc sweeep
(defun dc-sweep (start step stop &optional solver data)
  (make-instance 'class-matrix-system)
  (make-instance 'class-variables)          
  (let ((m       (make-instance 'class-matrix-system))
        (v       (make-instance 'class-variables)))
    (map-all-devices m)    
 ;   (print "chyba")
 ;   (print (get-sub-stack-vector m 0 (size m)))
    (set-new-symbol-var-matrix v start step stop (get-sub-stack-vector m 0 (size m))) ; will generate only one time point
    (loop for i from 0 below (first (size v)) do
      (setf *time-pos* i)
      (setf *time* (get-symbol-var-time v *time-pos*))
      (dc-solve m v solver data))
    (get-all-symbols v)))




; Maximal position of element in list
;(defun pos-max-element (array) 
;  (let ((max 0)
;        (pos 0))       
;    (loop for i from 0 below (length array) do 
;      (if 
;        (< max (aref array i))
;          (progn 
;            (setf max (aref array i)) 
;            (setf pos i))))
;    pos))




