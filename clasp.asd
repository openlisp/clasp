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
;;;;


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
    
    (:module systems
      :serial t
      :depends-on ("package")
      :components
      ((:file "equation-array")
        (:file "equation-vector")
        (:file "number-array")
        (:file "number-vector")))
    
    (:module matrice
      :serial t
      :depends-on (systems)
      :components
      ((:file "matrice-system")))
    
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
    
   

))




