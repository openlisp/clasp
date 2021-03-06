;;;; Copyright (c) 2012 David Cerny, All Rights Reserved
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



;;; Package defines set of public CLASP functions

(cl:defpackage #:clasp
  (:use #:cl)
;  (:import-from #:zpb-ttf
;                #:open-font-loader
;                #:xmin
;               #:xmax
;                #:ymin
;                #:ymax
;                #:bounding-box)
  (:export
;    #:hello-world
;    #:set-random-sparse-table
;    #:print-column-name-values
;    #:print-rhs-values
;    #:solve-sparse-matrix
;    #:test-device-map-r
;    #:test-device-map-c 

   ;;test analysis 
    #:test-thermistor
    #:test-simple-diode
    #:test-trans-simple-diode
    #:test-trans-simple-capacitor
    #:test-trans-ni-simple-capacitor
    #:simple-test-two-way-rectifier
    #:simple-test-two-way-rectifier-two 
    #:test-trans-bipolar-transistor 
    
 
 
    #:test-zener-stabilizator
    #:test-7-segment
    #:test-two-way-rectifier
    #:test-zener-diode
    #:test-matrix-solvers
    #:test-solvers-test     
    #:test-solvers 


    #:complex-matrix


    #:net-clear
    #:net-remove-device
    #:net-insert-device
    #:net-size


    ))
   
