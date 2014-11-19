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


; This will make *net* array which will hold all devices included in; circuit
(defvar *net* (make-hash-table :test #'equal))


;; Funkce pro odstranění všech prvků z databáze
(defun net-clear ()
  (setf
    *net* (make-hash-table :test #'equal)))

;; return size of the *net* table
(defun net-size ()
  (hash-table-size *net*))



(defgeneric net-insert-device (device name)
  (:documentation "Insert new device into network."))
(defmethod net-insert-device ((device class-device) name)
  (setf (gethash name *net*) device))


(defgeneric net-remove-device (name)
  (:documentation "Function for removing device from network."))
(defmethod net-remove-device (name)
  (remhash name *net*))



(defgeneric map-all-devices (m)
  (:documentation "Map all device. Go through definition list and map device one by one"))
(defmethod map-all-devices ((m matrice-system))
  (print "Device maping started")
  (maphash #'(lambda (name device)
      (format nil "Mapping device ~a ~a ~%" name device)
      (map-device device m))
    *net*)
  (print "Device maping finished"))

