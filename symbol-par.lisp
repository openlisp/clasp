 (in-package #:clasp)

;(defparameter *symbol-par-hash* (make-hash-table :test #'equal))

;(defun get-symbol-par (x)
;  (gethash x *symbol-par-hash*))
 
;(defun set-symbol-par (x value)
;  (setf (gethash x *symbol-par-hash*) value))

;(defun print-all-symbol-par ()
; (format nil "{~{~{~S => ~S~}~^, ~}}"
;  (format t "~&Name~20TValue~{~{~&~S~20T~S~}~}"
;   (loop for key being the hash-keys of *symbol-par-hash* 
;         for value being the hash-values of *symbol-par-hash*
;         collect (list key value)))))


;(set-symbol-par 'T 300) ;temperature
;(set-symbol-par 'e 1.60218d-19) ;electronic charge 
;(set-symbol-par 'pi pi) ;pi     
;(set-symbol-par 'm0 9.108d-31) ; × 10-31 kg 	electron rest mass
;(set-symbol-par 'c 2.998d8) ; m/s 	speed of light in vacuum
;(set-symbol-par 'epsilon0 8.85418d-14) ;farad/cm
;(set-symbol-par 'h 	6.626d-27) ; × 10-27 erg·s
;(set-symbol-par 'k 	1.3806d-23);  × 10-23 joule/K 	;Boltzmann's constant

;simulation parametres
(defparameter *temperature* 300
      "Simulation temperature in kelvins.")

(defparameter *time* 0
      "Simulation time in s")

(defparameter *time-pos* 0
      "Time position")

  
(defparameter *max-dc-iter* 500
      "maximal iteration")

(defparameter *residual* 1.0d-7
      "minimal residual")

(defparameter *max-ode-iter* 2000
      "maximal ode solver iteration" )

(defparameter *not-convergency* 0
      "maximal ode solver iteration" )

      
;Simulation constants
(defconstant +speed-of-light+ 2.998d8 
      "speed of light in km/s")
(defconstant +electron-charge+ 1.60218d-19
      "electronic charge")
(defconstant +boltzman-constant+ 1.3806d-23 
      "joule/K 	;Boltzmann's constant")

(defconstant +istp-temp-constant+ 288.15 
      "Kelvin ;International Standard Metric Conditions for natural gas and similar fluids, temperature.")

(defconstant +istp-press-constant+ 101325  
      "Pa ;International Standard Metric Conditions for natural gas and similar fluids, pressure.")


;(defconstant this-is-a-constant 'never-changing "for a test") 

;(constantp 'this-is-a-constant)
 
;(defparameter *temperature* 300)



