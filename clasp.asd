;;;; Copyright (c) 2003 David Cerny, All Rights Reserved
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


;;;; clasp.asd
;;;; map-device make-var-node stack get-sub- GET-SUB-G-ARRAY


(in-package :asdf)

#-(or openmcl sbcl cmu scl clisp lispworks ecl allegro cormanlisp abcl)
(error "Sorry, this Lisp is not yet supported. Patches welcome!")

(asdf:defsystem #:clasp
  :author "David Cerny <cernyd1@fel.cvut.cz>"
  :maintainer "David Cerny <cernyd1@fel.cvut.cz>"
  :version "0.1.1"
  :licence "BSD"
  :description "Common LISP as Simulator Program for Electrical Circuits"
  :long-description "CLASP is an unusual and efficient usage of functional programming language Common LISP as simulation program (CLASP) for electronic circuits. The principle of automatic self-modifying program has enabled complete freedom in definition of methods for optimized solution of any problem and speeding up the entire process of simulation. "
  :depends-on (#:gsll #:iterate)
  
  :components
  
  ((:file "package")
    
    (:module matrice
      :serial t
      :depends-on ("package")
      :components
      ((:file "matrice-system")
        (:module systems
          :serial t
          :components
          ((:file "equation-array")
            (:file "equation-vector")
            (:file "number-array")
            (:file "number-vector")))))
    
    (:module symbols
      :serial t
      :depends-on (matrice)
      :components
      ((:file "symbol-par")
        (:file "symbol-var")))
    
    
    (:module device
      :serial t
      :depends-on (symbols)
      :components
      ((:file "device")))
    
    
    (:module net
      :serial t
      :depends-on (device)
      :components
      ((:file "net")))
    
    
    (:module models
      :serial t
      :depends-on (device)
      
      :components
      ((:module capacitors
          :serial t
          :components
          ((:file "device-capacitor")
            (:file "device-capacitor-nonlinear")))
        
        
        
        (:module resistors
          :serial t
          :components
          ((:file "device-resistor")
            (:file "device-experimental-resistor")
            (:file "device-resistor-nonlinear")
            (:file "device-resistor-fun")))
        
        
        
        (:module sources
          :serial t
          :components
          ((:file "device-source-voltage")
            (:file "device-source-volfun")
            (:file "device-source-current")))
        
        
        (:module diodes
          :serial t
          :components
          ((:file "device-diode")
            (:file "device-diode-simple")
            (:file "device-diode-simple2")
            (:file "device-diode-simple3")
            (:file "device-diode-simple-zener")))
        
        
        (:module transistors
          :serial t
          :components
          
          ((:file "device-bipolar-transistor")))
        
        
        (:module inductors
          :serial t
          :components
          
          ((:file "device-inductor")))
        
        
        
        (:module misc
          :serial t
          :components
          
          ((:file "device-thermistor")))))
    
    
    
    
    (:module analysis
      :serial t
      :depends-on ("symbols" "models")
      :components
      
      ((:file "analysis-dc")
        (:file "analysis-dc-sweep")
        (:file "analysis-trans")
        (:file "analysis-ni-trans")
        
        
        (:module solvers
          :serial t
          :components
          
          ((:file "dc-solve")
            
            (:module methods			
              :components
              
              ((:file "damped-newton-raphson")
                (:file "particle-swarm")
                (:file "evolutionary-newton-raphson")
                (:file "newton-raphson")
                (:file "bdf")
                (:file "luf")))))))
    
    
    
    
    (:module tests
      :serial t
      :depends-on ("symbols" "analysis" "models")
      :components
      ((:file "test")))
    
    ))




