 (in-package #:clasp)


;(defparameter matrix-max-index 50)

(defclass class-variables () 
  ((matrix
    :accessor matrix
    :initform nil)
   (matrix-deriv
    :accessor matrix-deriv
    :initform nil)  
   (variable-names
    :accessor variable-names
    :initform nil )
   (time-axis    
    :accessor time-axis
    :initform nil )       
   (time-axis-steps    
    :accessor time-axis-steps
    :initform nil )       
   (size    
    :accessor size
    :initform nil)
    ))


;funkce pro vymazani tabulek
(defmethod clear-symol-var-matrix ((m class-variables))
  (setf (variable-names  m) nil)
  (setf (time-axis       m) nil)  
  (setf (size            m) nil)
  (setf (matrix          m) nil))

(defmethod set-new-symbol-var-matrix ((m class-variables) start step stop var-axis); &optional (tim #(0)))
  (setf (variable-names  m) var-axis)
  (setf (time-axis       m) (make-time-axis start step stop))
;  (setf (step-axis       m)    (make-step-axis start step stop))

;  (print "make-time-axis")
  (setf (time-axis-steps m) (make-array (length (time-axis m)) :initial-element step))
  (setf (size            m) (list (length (time-axis m)) (length var-axis)))
  (setf (matrix          m) (grid:make-foreign-array 'double-float 
                                 :dimensions (size m) 
                                 :initial-element 0.0d0)))
                                 
                                 
;  (setf (matrix-deriv    m) (grid:make-foreign-array 'double-float 
;                                 :dimensions (size m) 
;                                 :initial-element 0.0d0)))


  
; (make-array tlength
;:initial-contents (loop for i from tstart to 80 collect (* 0.1 i)))

;(setf a (floor (/ (- 3 1) 0.9)))
;  (loop for i from 0.0 to 79.0 collect (/ i 10))

;tvrorba casove osy
(defun make-time-axis (start step stop) 
(loop for i from (rationalize start) below (rationalize stop)  by (rationalize step) collect (float i)))


  
  
  ;(make-array (size m) 
  ;                                      :element-type 'double-float 
  ;                                      :initial-element 0.0)))

(defmethod get-matrix-m ((m class-variables))
  (matrix m))

;;navrati pozici prvku v matici, kdyz prvek v matici neni navrati max index a prvek do 
;; matice ulozi

;(defmethod get-symbol-var ((m class-variables) tim var)
;  (let ((var-pos (position-if #'(lambda (x) (equal x var)) (variable-names m)))
;        (time-pos (position-if #'(lambda (x) (equal x tim)) (time-axis      m))))
;          (grid:gref (matrix m) time-pos var-pos)))


;(defmethod get-symbol-var-time-pos ((m class-variables) tim)
;  (position-if #'(lambda (x) (equal x tim)) (time-axis      m)))

(defmethod get-symbol-var-time ((m class-variables) time-pos)
  (nth time-pos (time-axis m)))


(defmethod set-symbol-var-time ((m class-variables) time-pos value)
  (setf (nth time-pos (time-axis m)) value))


;(defmethod set-all-symbols-var-time-pos ((m class-variables) time-pos var-vector)
;  (setf (grid:row (matrix m) time-pos) var-vector)) 


; (defmethod get-all-symbols-var-time-pos ((m class-variables) time-pos)
;  (grid:row (matrix m) time-pos))


; (defmethod set-symbols-var-time-subseq ((m class-variables) time-pos sub-pos var-vector)
;  (setf (subseq (grid:row (matrix m) time-pos) sub-pos) var-vector)) 

(defmethod get-sub-symbol-var-vector ((m class-variables) start-row start-col cols)
  (grid:slice (matrix m) `(,start-row (:range ,start-col ,cols )) :drop t))


(defmethod set-sub-symbol-var-vector ((m class-variables) start-row start-col var-vector)
  (setf (grid:subgrid (matrix m) (list start-row start-col) '(1)) var-vector))


(defmethod set-symbol-var-value ((m class-variables) row col value)
  (setf (grid:gref (matrix m) row col) value))


;(defmethod set-symbol-var ((m class-variables) var tim val)
;  (let ((var-pos (position-if #'(lambda (x) (equal x var)) (variable-names m)))
;        (time-pos (position-if #'(lambda (x) (equal x tim)) (time-axis      m))))
;          (setf 
;            (aref (matrix m) time-pos var-pos)
;              val)))



(defmethod print-string-output (string)
  (with-open-file (stream "output/output.dat" :direction :output  
                                   :if-exists :APPEND
                                   :if-does-not-exist :create)
(format stream "~%~A" string)))  



(defmethod print-string-iter (string)
  (with-open-file (stream "output/dc-iter.dat" :direction :output  
                                   :if-exists :APPEND
                                   :if-does-not-exist :create)
(format stream "~%~A" string)))






(defmethod get-all-symbols ((m class-variables))
  (with-open-file (stream "output/output.dat" :direction :output  
                                   :if-exists :APPEND
                                   :if-does-not-exist :create)
                                   
     
     (format stream "~%~{~15A   ~}"  
     (cons '*time*
     (loop for i from 0 to (1- (second (size m))) collect 
           (aref (variable-names m) i))))


   (loop for i from 0 to (1- (first (size m))) do 
     (format stream "~%~{~15F; ~}" 
      ;(print 
       (cons
         (nth i (time-axis m) )
         (loop for j from 0 to (1- (second (size m))) collect 
           (grid:gref (matrix m) i j)))))))



;(list matrix-max-index matrix-max-index) :initial-element '(+ 0)))

;(defmethod get-symbol-var ((m class-variables) row col)
;  (aref (var m))
;      (get-position m row) 
;      (get-position m col))

;(defmethod set-symbol-var ((m class-variables) row col val);
;  (setf
;    (aref (var m))
;      (get-position m row) 
;      (get-position m col)
;        val))



;(defparameter *symbol-var-matrix* (make-hash-table :test #'equal))

;(defun get-symbol-var (x)
;  (gethash x *symbol-par-hash*))
 
;(defun set-symbol-par (x value)
;  (setf (gethash x *symbol-par-hash*) value))

;(defun print-all-symbol-par ()
 ; (format nil "{~{~{~S => ~S~}~^, ~}}"
;  (format t "~&Name~20TValue~{~{~&~S~20T~S~}~}"
;   (loop for key being the hash-keys of *symbol-par-hash* 
;         for value being the hash-values of *symbol-par-hash*
;         collect (list key value))))


