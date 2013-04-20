 ;;;; Copyright (c) 2013 David Cerny, All Rights Reserved
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;;   * Redistributions of source code must retain the above copyright
;;;;     notice, this list of conditions and the following disclaimer.
;;;;
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 (in-package #:clasp)


; Basic class device, all devicese are inherences of this class
(defclass class-device () ())

;; Funkce pro odstranění všech prvků z databáze
(defun net-clear ()
  (setf
   *net* (make-hash-table :test #'equal)  
   ))    
   
;; return size of the *net* table  
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

