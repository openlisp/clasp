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

;set-g-value happend

(require "asdf")

(format t "CLASP Initialization. ~%")
(load "~/quicklisp/setup.lisp")
(ql:quickload "gsll")
(ql:quickload "antik")

; added due staple requirements
;(require :sb-introspect)
;(ql:quickload :staple)



;not shure whether this is needed
;(push "." asdf:*central-registry*)
(push (truename "./") asdf:*central-registry*)

(asdf:load-system :clasp)

(format t "CLASP Inicialization finished.~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%")

;(staple:generate :clasp)

; Initial testing circuit. 
;(clasp:test-trans-ni-simple-capacitor)
;(clasp:simple-test-two-way-rectifier)
;(clasp:simple-test-two-way-rectifier-two)
;(clasp:test-trans-bipolar-transistor)


(clasp:test-solvers)


;Exit
(quit)
