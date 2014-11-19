
 (in-package #:clasp)
  
  
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
  (print-string-output "test-7-segment-diode - Newton Raphson")
  (print-string-iter "test-7-segment - Newton Raphson")  
  (dc-sweep 0.0 0.1 5.0 'newton-raphson) 
       
 
  (print "end")
  t 
  )
; Dobes mel dobrou poznamku. V pripade ze neni potreba pocitat proud tekouci 
; danou soucastkou nemusi se tak ta soucastka vubec definovat. 





