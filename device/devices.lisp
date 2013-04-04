 (in-package #:clasp)




(defmethod map-all-devices ((m class-matrix-system))
  (print "Maping started")
  ;(print *net*)
  (maphash #'(lambda (name device) 
   (print name)
   (print device)
   ; (format nil "Mapping device ~a." name)
      (map-device device m)
      ) *net*)
   (print "Maping finished"))

