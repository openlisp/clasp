 (in-package #:clasp)


; Basic class device, all devicese are inherences of this class
(defclass class-device () ())

;; Funkce pro odstranění všech prvků z databáze
(defun net-clear ()
  (setf
   *net* (make-hash-table :test #'equal)  
   ))    
   
;velikost tabulky   
(defun net-size ()
  (hash-table-size *net*))

;Funkce pro vložení nebo přepsání prvku z databáze
(defmethod net-insert-device ((device class-device) name)
  (setf (gethash name *net*) device))
        
;Funkce pro odstranění prvku z databáze 
(defun net-remove-device (name)
  (remhash name *net*))


(defgeneric map-device (device matrix-system)
  (:documentation "Map devices for analysis."))

; This will make *net* array which will hold all devices included in; circuit
(defvar *net* (make-hash-table :test #'equal))

