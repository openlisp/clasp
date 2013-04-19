;;; Copyright (c) 2011 David Cerny, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;


;Help
;for load a proget you must use
;(asdf:operate 'asdf:load-op :clasp)
;(ql:quickload "gsll")
;     (ql:quickload "grid")
;     (ql:quickload "foreign-array")

;command
; to get updated all dist
;(ql:update-all-dists)
;to update ql client
; (ql:update-client)

;(defparameter m1 #m(1 2 3 ^ 0 6 8))

(in-package :asdf)

#-(or openmcl sbcl cmu scl clisp lispworks ecl allegro cormanlisp abcl)
(error "Sorry, this Lisp is not yet supported.  Patches welcome!")

(defsystem :clasp
  :author "David Cerny <cernyd1@fel.cvut.cz>"
  :maintainer "David Cerny <cernyd1@fel.cvut.cz>"
  :version "0.0.3"
  :licence "BSD"
  :description ""
  :long-description ""

  :depends-on (gsll
               :iterate)
 ; :depends-on (:iterate)    

  :components ((:file "package")
               (:file "net"
                      :depends-on ("package"))    
                                                 
               (:file "matrix-system"
                      :depends-on ("package"
                                   "net"))

               (:file "symbol-par"
                      :depends-on ("package"                      
                                   "matrix-system"))

               (:file "symbol-var"
                      :depends-on ("package"                      
                                   "matrix-system"))
                                   
                                   
               (:file "device/device-capacitor"
                      :depends-on ("package"
                                   "net"
                                   "matrix-system"))          
                                   
               (:file "device/device-resistor"
                      :depends-on ("package"
                                   "net"
                                   "matrix-system"))

               (:file "device/device-thermistor"
                      :depends-on ("package"
                                   "net"
                                   "matrix-system"))
                                             
               (:file "device/device-source-voltage"
                      :depends-on ("package"
                                   "net"
                                   "matrix-system")) 

               (:file "device/device-source-volfun"
                      :depends-on ("package"
                                   "net"
                                   "matrix-system"))                                                                      

               (:file "device/device-source-current"
                      :depends-on ("package"
                                   "net"
                                   "matrix-system"))                                                                      

               (:file "device/device-inductor"
                      :depends-on ("package"
                                   "net"
                                   "matrix-system"))          

               (:file "device/device-diode"
                      :depends-on ("package"
                                   "net"
                                   "symbol-par"
                                   "matrix-system"))          


               (:file "device/device-resistor-nonlinear"
                      :depends-on ("package"
                                   "net"
                                   "matrix-system"))          

               (:file "device/device-capacitor-nonlinear"
                      :depends-on ("package"
                                   "net"
                                   "matrix-system"))          


               (:file "device/device-diode-simple"
                      :depends-on ("package"
                                   "net"
                                   "matrix-system"))          

               (:file "device/device-diode-simple2"
                      :depends-on ("package"
                                   "net"
                                   "matrix-system"))          

               (:file "device/device-diode-simple-zener"
                      :depends-on ("package"
                                   "net"
                                   "matrix-system"))          

               (:file "device/device-resistor-fun"
                      :depends-on ("package"
                                   "net"
                                   "matrix-system"))          

               (:file "device/devices"
                      :depends-on ("package"
                                   "net"
                                   "device/device-source-volfun"
                                   "device/device-source-current"
                                   "device/device-capacitor"
                                   "device/device-resistor"
                                   "device/device-thermistor"
                                   "device/device-inductor"
                                   "device/device-source-voltage"
                                   "device/device-diode"
                                   "device/device-resistor-nonlinear"
                                   "device/device-resistor-fun"
                                   "device/device-diode-simple"
                                   "device/device-diode-simple2"
                                   "device/device-diode-simple-zener"
                                   "device/device-capacitor-nonlinear"
                                   "device/device-resistor-nonlinear"))


               (:file "solver/solver-damped-newton-raphson"
                      :depends-on ("package"
                                   "symbol-var"
                                   "symbol-par"

;                                   "net"
                                   "matrix-system"
                                   ))          

               (:file "solver/solver-bdf"
                      :depends-on ("package"
                                   "symbol-var"
                                   "symbol-par"

;                                   "net"
                                   "matrix-system"
                                   ))          




               (:file "solver/solver-particle-swarm"
                      :depends-on ("package"
                                   "symbol-var"
                                   "symbol-par"

;                                   "net"
                                   "matrix-system"
                                   ))          

               (:file "solver/solver-evolutionary-newton-raphson"
                      :depends-on ("package"
                                   "symbol-var"
                                   "symbol-par"

;                                   "net"
                                   "matrix-system"
                                   ))   

               (:file "solver/solver-newton-raphson"
                      :depends-on ("package"
                                   "symbol-var"
                                   "symbol-par"

;                                   "net"
                                   "matrix-system"
                                   ))  

               (:file "solver/solver-luf"
                      :depends-on ("package"
                                   "symbol-var"
                                   "symbol-par"

;                                   "net"
                                   "matrix-system"
                                   ))  


               (:file "solver/solvers"
                      :depends-on ("package"
                                   "solver/solver-damped-newton-raphson"
                                   "solver/solver-particle-swarm"
                                   "solver/solver-evolutionary-newton-raphson"
                                   "solver/solver-newton-raphson"
                                   "solver/solver-luf"))
                          

               (:file "test/test"
                      :depends-on ("package"
                                   "symbol-var"
                                   "symbol-par"
                                   "analysis/analysis-dc"
                                   "analysis/analysis-dc-sweep"))
                                   


                      
               (:file "analysis/analysis-dc"
                      :depends-on ("package"
;                                   "matrix-system"
                                   "device/devices"
                                   "solver/solvers"))

               (:file "analysis/analysis-dc-sweep"
                      :depends-on ("package"
;;                                   "symbol-var"
;;                                   "symbol-par"
;;                                   "matrix-system"
                                   "device/devices"
                                   "solver/solvers"))
                                   


               (:file "analysis/analysis-trans"
                      :depends-on ("package"
;                                   "matrix-system"
                                   "device/devices"
                                   "solver/solvers"))


                                                                      
                                   ))         
                       





