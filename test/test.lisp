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


;;; Overal Simulation Tests
(defun test-solvers ()
  (print "start - test-solvers")
  (net-clear)
  ;; Simple circuit definition, One voltage source,
  ;; resistor and one non-linear diode.
  (E "E1" 1 0 5)
  (R "R1" 1 2 500)  
  (DS2 "D1"  2 0)

  (print-string-output "Newton Raphson")
  (print-string-iter "Newton Raphson")  
  (time (dc 'newton-raphson)) 
       
  (print-string-output "Damped Newton Raphson")
  (print-string-iter "Damped Newton Raphson")  
  (print-string-output (time (dc 'damped-newton-raphson)))  
 
  (print-string-iter "Evolutionary Newton Raphson")    
  (print-string-output "Evolutionary Newton Raphson")    
  (time (dc 'evolutionary-newton-raphson))      
  
  (print-string-output "Particle Swarm")    
  (print-string-iter "Particle Swarm")    
  (time (dc 'particle-swarm))      

  (print "end - test-solvers")
  t 
  )



(defun test-trans-simple-diode ()
  (print "start - test-simple-diode")
  (net-clear)

;  (EF "E1" 1 0 #'(lambda () *TIME*))
  (E "E1" 1 0 5)   
  (R "R1" 1 2 500)  
  (DS2 "D1"  2 0)
  (trans 0.0 0.1 5.0 'newton-raphson) 
)


(defun test-trans-simple-capacitor ()
  (print "start - test-simple-capacitor")
  (net-clear)

;  (EF "E1" 1 0 #'(lambda () *TIME*))
  (E "E1" 1 0 5)   
  (R "R1" 1 2 500)  
  (C "C1"  2 0 2e-8)
  (trans 0.0 0.1 5.0 'newton-raphson) 
)



(defun test-trans-ni-simple-capacitor ()
  (print "start - test-simple-capacitor")
  (net-clear)

;  (EF "E1" 1 0 #'(lambda () *TIME*))
  (E "E1" 1 0 5)   
  (R "R1" 1 2 500)  
  (C "C1"  2 0 2e-8)
  (trans-ni 0.0 0.1 5.0 'newton-raphson) 
)




; 1 test-simple-diode
(defun test-simple-diode ()
  (print "start - test-simple-diode")
  (net-clear)

  (EF "E1" 1 0 #'(lambda () *TIME*))
  (R "R1" 1 2 500)  
  (DS2 "D1"  2 0)
   
;  (print "generating deviced finished")
;  (time (dc-sweep 0.0 0.1 5.0 'damped-newton-raphson))      

  (print-string-output "test-simple-diode - Newton Raphson")
  (print-string-iter "test-simple-diode - Newton Raphson")  
  (dc-sweep 0.0 0.1 5.0 'newton-raphson) 
       
  (print-string-output "test-simple-diode - Damped Newton Raphson - 0.8")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.8")  
  (dc-sweep 0.0 0.1 5.0 'damped-newton-raphson 0.8)  
 
  (print-string-output "test-simple-diode - Damped Newton Raphson - 0.6")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.6")  
  (dc-sweep 0.0 0.1 5.0 'damped-newton-raphson 0.6)

   (print-string-output "test-simple-diode - Damped Newton Raphson - 0.4")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.4")  
  (dc-sweep 0.0 0.1 5.0 'damped-newton-raphson 0.4)
  
   (print-string-output "test-simple-diode - Damped Newton Raphson - 0.2")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.2")  
  (dc-sweep 0.0 0.1 5.0 'damped-newton-raphson 0.2)  
 
  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 2")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 2")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 2)      

  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 3")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 3")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 3)

  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 4")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 4")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 4)
  
  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 5")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 5")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 5)
  
  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 6")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 6")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 6)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson  - 7")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson  - 7")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 7)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson  - 8")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson  - 8")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 8)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 9")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 9")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 9)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 10")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 10")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 10)    

  (print "end - test-simple-diode")
  t 
  )


; 2 zener diode 
(defun test-zener-diode ()
  (print "start - test-simple-diode")
  (net-clear)

  (EF "E1" 1 0 #'(lambda () *TIME*))
  (R "R1" 1 2 500)  
  (DSZ "D1" 0 2 11e-12)  
   
(print-string-output "test-simple-diode - Newton Raphson")
  (print-string-iter "test-simple-diode - Newton Raphson")  
  (dc-sweep 8.0 0.1 13.0 'newton-raphson) 
       
  (print-string-output "test-simple-diode - Damped Newton Raphson - 0.8")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.8")  
  (dc-sweep 8.0 0.1 13.0 'damped-newton-raphson 0.8)  
 
  (print-string-output "test-simple-diode - Damped Newton Raphson - 0.6")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.6")  
  (dc-sweep 8.0 0.1 13.0 'damped-newton-raphson 0.6)

   (print-string-output "test-simple-diode - Damped Newton Raphson - 0.4")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.4")  
  (dc-sweep 8.0 0.1 13.0 'damped-newton-raphson 0.4)
  
   (print-string-output "test-simple-diode - Damped Newton Raphson - 0.2")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.2")  
  (dc-sweep 8.0 0.1 13.0 'damped-newton-raphson 0.2)  
 
  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 2")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 2")    
  (dc-sweep 8.0 0.1 13.0 'evolutionary-newton-raphson 2)      

  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 3")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 3")    
  (dc-sweep 8.0 0.1 13.0 'evolutionary-newton-raphson 3)

  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 4")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 4")    
  (dc-sweep 8.0 0.1 13.0 'evolutionary-newton-raphson 4)
  
  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 5")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 5")    
  (dc-sweep 8.0 0.1 13.0 'evolutionary-newton-raphson 5)
  
  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 6")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 6")    
  (dc-sweep 8.0 0.1 13.0 'evolutionary-newton-raphson 6)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson  - 7")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson  - 7")    
  (dc-sweep 8.0 0.1 13.0 'evolutionary-newton-raphson 7)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson  - 8")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson  - 8")    
  (dc-sweep 8.0 0.1 13.0 'evolutionary-newton-raphson 8)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 9")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 9")    
  (dc-sweep 8.0 0.1 13.0 'evolutionary-newton-raphson 9)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 10")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 10")    
  (dc-sweep 8.0 0.1 13.0 'evolutionary-newton-raphson 10)    



 ; (print "generating deviced finished")
 ; (time (dc-sweep 8.0 0.1 13.0))      
  (print "end - test-simple-diode")
  t
  )
  
  

; 2.5 diode stabilisator
(defun test-zener-stabilizator ()
  (print "start - test-zener-stabilizator")
  (net-clear)

  (E "E1" 1 0 10)
  (R "R1" 1 2 220)  
  (DSZ "D1" 0 2 11e-12)  
  (RF "R2" 2 0 #'(lambda () *TIME*))
   
  (print-string-output "test-simple-diode - Newton Raphson")
  (print-string-iter "test-simple-diode - Newton Raphson")  
  (dc-sweep 1000 20 2000 'newton-raphson) 
       
  (print-string-output "test-simple-diode - Damped Newton Raphson - 0.8")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.8")  
  (dc-sweep 1000 20 2000 'damped-newton-raphson 0.8)  
 
  (print-string-output "test-simple-diode - Damped Newton Raphson - 0.6")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.6")  
  (dc-sweep 1000 20 2000 'damped-newton-raphson 0.6)

   (print-string-output "test-simple-diode - Damped Newton Raphson - 0.4")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.4")  
  (dc-sweep 1000 20 2000 'damped-newton-raphson 0.4)
  
   (print-string-output "test-simple-diode - Damped Newton Raphson - 0.2")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.2")  
  (dc-sweep 1000 20 2000 'damped-newton-raphson 0.2)  
 
  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 2")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 2")    
  (dc-sweep 1000 20 2000 'evolutionary-newton-raphson 2)      

  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 3")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 3")    
  (dc-sweep 1000 20 2000 'evolutionary-newton-raphson 3)

  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 4")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 4")    
  (dc-sweep 1000 20 2000 'evolutionary-newton-raphson 4)
  
  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 5")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 5")    
  (dc-sweep 1000 20 2000 'evolutionary-newton-raphson 5)
  
  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 6")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 6")    
  (dc-sweep 1000 20 2000 'evolutionary-newton-raphson 6)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson  - 7")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson  - 7")    
  (dc-sweep 1000 20 2000 'evolutionary-newton-raphson 7)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson  - 8")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson  - 8")    
  (dc-sweep 1000 20 2000 'evolutionary-newton-raphson 8)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 9")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 9")    
  (dc-sweep 1000 20 2000 'evolutionary-newton-raphson 9)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 10")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 10")    
  (dc-sweep 1000 20 2000 'evolutionary-newton-raphson 10)    


   
   
 ; (print "generating deviced finished")
;  (time (dc-sweep 1000 20 2000))   
  (print "end - test-zener-stabilizator")
  t 
  )  
  
; 3 LED 7-segment
(defun test-7-segment ()
  (print "start")
  (net-clear)

  (E "E1" 1 0 10)
  (R "R1" 1 2 500)
  (DS2 "D1"  2 0)

  (R "R2" 1 3 500)
  (DS2 "D2"  3 0)

  (R "R3" 1 4 500)
  (DS2 "D3"  4 0)

  (R "R4" 1 5 500)
  (DS2 "D4"  5 0)

  (R "R5" 1 6 500)
  (DS2 "D5"  6 0)

  (R "R6" 1 7 500)
  (DS2 "D6"  7 0)

  (R "R7" 1 8 500)
  (DS2 "D7"  8 0)
     
  (print "generating deviced finished")
  (print-string-output "test-simple-diode - Newton Raphson")
  (print-string-iter "test-simple-diode - Newton Raphson")  
  (dc-sweep 0.0 0.1 5.0 'newton-raphson) 
       
  (print-string-output "test-simple-diode - Damped Newton Raphson - 0.8")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.8")  
  (dc-sweep 0.0 0.1 5.0 'damped-newton-raphson 0.8)  
 
  (print-string-output "test-simple-diode - Damped Newton Raphson - 0.6")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.6")  
  (dc-sweep 0.0 0.1 5.0 'damped-newton-raphson 0.6)

   (print-string-output "test-simple-diode - Damped Newton Raphson - 0.4")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.4")  
  (dc-sweep 0.0 0.1 5.0 'damped-newton-raphson 0.4)
  
   (print-string-output "test-simple-diode - Damped Newton Raphson - 0.2")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.2")  
  (dc-sweep 0.0 0.1 5.0 'damped-newton-raphson 0.2)  
 
  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 2")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 2")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 2)      

  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 3")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 3")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 3)

  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 4")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 4")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 4)
  
  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 5")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 5")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 5)
  
  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 6")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 6")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 6)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson  - 7")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson  - 7")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 7)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson  - 8")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson  - 8")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 8)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 9")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 9")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 9)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 10")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 10")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 10)    
  (print "end")
  t 
  )
; Dobes mel dobrou poznamku. V pripade ze neni potreba pocitat proud tekouci 
; danou soucastkou nemusi se tak ta soucastka vubec definovat. 

; 4 Thermistor
(defun test-thermistor ()
  (print "start")
  (net-clear)

  (E "E1" 1 0 5)
  (RTH "R1" 1 0 );500)  
  
   
  (print "generating deviced finished")
  (print-string-output "test-simple-diode - Newton Raphson")
  (print-string-iter "test-simple-diode - Newton Raphson")  
  (dc-sweep 0.0 0.1 5.0 'newton-raphson) 
       
  (print-string-output "test-simple-diode - Damped Newton Raphson - 0.8")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.8")  
  (dc-sweep 0.0 0.1 5.0 'damped-newton-raphson 0.8)  
 
  (print-string-output "test-simple-diode - Damped Newton Raphson - 0.6")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.6")  
  (dc-sweep 0.0 0.1 5.0 'damped-newton-raphson 0.6)

   (print-string-output "test-simple-diode - Damped Newton Raphson - 0.4")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.4")  
  (dc-sweep 0.0 0.1 5.0 'damped-newton-raphson 0.4)
  
   (print-string-output "test-simple-diode - Damped Newton Raphson - 0.2")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.2")  
  (dc-sweep 0.0 0.1 5.0 'damped-newton-raphson 0.2)  
 
  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 2")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 2")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 2)      

  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 3")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 3")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 3)

  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 4")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 4")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 4)
  
  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 5")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 5")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 5)
  
  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 6")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 6")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 6)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson  - 7")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson  - 7")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 7)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson  - 8")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson  - 8")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 8)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 9")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 9")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 9)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 10")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 10")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 10)   (print "end")
  t 
  )


; 5 Two way rectifier
(defun test-two-way-rectifier ()
  (net-clear)
  (EF "E1" 1 0 #'(LAMBDA ( ) (* (SIN *TIME*))))
  (DS2 "D1"  1 2)
  (DS2 "D2"  3 1)
  (DS2 "D3"  0 2)
  (DS2 "D4"  3 0)
  (R "RS2" 2 3 200)  
   
  (print "generating deviced finished")
  (print-string-output "test-simple-diode - Newton Raphson")
  (print-string-iter "test-simple-diode - Newton Raphson")  
  (dc-sweep 0.0 0.1 5.0 'newton-raphson) 
       
  (print-string-output "test-simple-diode - Damped Newton Raphson - 0.8")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.8")  
  (dc-sweep 0.0 0.1 5.0 'damped-newton-raphson 0.8)  
 
  (print-string-output "test-simple-diode - Damped Newton Raphson - 0.6")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.6")  
  (dc-sweep 0.0 0.1 5.0 'damped-newton-raphson 0.6)

   (print-string-output "test-simple-diode - Damped Newton Raphson - 0.4")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.4")  
  (dc-sweep 0.0 0.1 5.0 'damped-newton-raphson 0.4)
  
   (print-string-output "test-simple-diode - Damped Newton Raphson - 0.2")
  (print-string-iter "test-simple-diode- Damped Newton Raphson - 0.2")  
  (dc-sweep 0.0 0.1 5.0 'damped-newton-raphson 0.2)  
 
  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 2")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 2")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 2)      

  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 3")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 3")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 3)

  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 4")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 4")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 4)
  
  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 5")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 5")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 5)
  
  (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 6")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 6")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 6)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson  - 7")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson  - 7")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 7)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson  - 8")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson  - 8")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 8)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 9")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 9")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 9)
  
    (print-string-iter "test-simple-diode -Evolutionary Newton Raphson - 10")    
  (print-string-output "test-simple-diode -Evolutionary Newton Raphson - 10")    
  (dc-sweep 0.0 0.1 5.0 'evolutionary-newton-raphson 10)   (print "end")
  t 
  )

; make complex random matrix
(defun make-random-complex-matrix (i j rng)
  (grid:map-grid  
       :source (complex-matrix-gen i j)
       :element-function #'(lambda (x)  (complex (gsl:sample rng :uniform) (gsl:sample rng :uniform) ))))

; make complex random vector
(defun make-random-complex-vector (i rng)
  (grid:map-grid  
       :source (complex-vector-gen i)
       :element-function #'(lambda (x)  (complex (gsl:sample rng :uniform) (gsl:sample rng :uniform) ))))

;make empty complex matrix
(defun complex-matrix-gen (i j)
  (GRID:MAKE-FOREIGN-ARRAY
	    '(COMPLEX DOUBLE-FLOAT)
	    :dimensions (list i j)))
;make empty complex vector	    
(defun complex-vector-gen (i)
  (GRID:MAKE-FOREIGN-ARRAY
	    '(COMPLEX DOUBLE-FLOAT)
	    :dimensions (list i)))

	    

(defun complex-matrix-test (i)

(LET* ((rng (gsl:MAKE-RANDOM-NUMBER-GENERATOR gsl:+mt19937+ (random 10)))
      (MATRIX (make-random-complex-matrix i i rng))        
	  (VEC (make-random-complex-vector i rng)))
;      (print matrix)
;      (print vec)
      (time (MULTIPLE-VALUE-BIND
	    (MATRIX PERM)
	  (GSL:LU-DECOMPOSITION MATRIX)
	  (GSL:LU-SOLVE MATRIX VEC PERM)))
	  ))



; Linear autogrneration circuit
(defun test-auto-generation ()
  (print "start")
  (net-clear)
  (E "E1" 1 0 5)
  (R "R1" 1 0 100)
  (let ((num-rand-elm 32))
  (loop for i from 1 to num-rand-elm do  ; pro zajisteni soudrznosti
    (R (format nil "R~s" i) i (+ i 1) (random 100d2))) 

;  (R (format nil "R~s" (1+ num-rand-elm)) (1+ num-rand-elm) 0 (random 100d2))

  (loop for i from 1 to num-rand-elm do 
    (let ((n+ (random num-rand-elm))
           (n- (random num-rand-elm)))
    (print (list n+ n-))       
    (R (format nil "Rd~s" i) n- n+ (random 100d2))))
  )
  (print "generating deviced finished")

  (time (dc))

   
  (print "end")
  t 
  )

