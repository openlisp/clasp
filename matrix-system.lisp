 (in-package #:clasp)


(defparameter matrix-max-index 2000)


;function for easy variable definition
(defun make-var-node (name num)
  (intern 
    (concatenate 'string
      (string-upcase (format nil "~s" name))
      (string-upcase (format nil "~s" num)))))

(defun make-var-name (name str)
  (intern 
    (concatenate 'string
       (string-upcase (format nil "~s" name))
       (string-upcase (string str)))))



;; tu matici by se vyplailo udelat uz s nulovymi prvky
(defclass class-matrix-system () 
  ((arrays
    :accessor arrays
    :initform (make-instance 'class-arrays))   
   (stack    
    :accessor stack
    :initform (let ((m (make-array matrix-max-index)))(setf (aref m 0)  (make-var-node 'V 0)) (set (aref m 0) 0.0d0) m))
   (rhs-number    
    :accessor rhs-number
    :initform   (make-array matrix-max-index :initial-element  0.0d0))
    
   (rhsl-start    
    :accessor rhsl-start
    :initform   (make-array matrix-max-index :initial-element  0.0d0))

    
   (rhs-equations    
    :accessor rhs-equations
    :initform  (make-array matrix-max-index :initial-element '()))
    
   (equations    
    :accessor equations
    :initform (make-array matrix-max-index :initial-element '()))
    
    
   (linear    
    :accessor linear
    :initform t)
   (differential    
    :accessor differential
    :initform nil)
   (size    
    :accessor size
    :initform 1)
    ))
    
(defclass class-arrays () 
  ((g-array
    :accessor g-array
    :initform (grid:make-foreign-array 'double-float :dimensions (list matrix-max-index  matrix-max-index) :initial-element 0.0d0))
    
   (e-array
    :accessor e-array
    :initform (grid:make-foreign-array 'double-float :dimensions (list matrix-max-index  matrix-max-index) :initial-element 0.0d0))
    
   (z-array
    :accessor z-array
    :initform (grid:make-foreign-array 'double-float :dimensions (list matrix-max-index matrix-max-index) :initial-element 0.0d0))
    
   (d-array
    :accessor d-array
    :initform (make-array (list matrix-max-index matrix-max-index) :initial-element '()))))
    
          
    
;funkce pro vymazani tabulek
;(defmethod clear-matrix ((m class-matrix-system))
;  (setf (arrays m) (make-instance 'class-arrays))
;  (setf (stack  m) (make-array matrix-max-index ))
;  (setf (rhs    m) (make-array matrix-max-index ))
;  (setf (size   m)  0) 
;  t
;  )


;get rhs veccotr
(defmethod get-sub-rhsl-start-vector ((m class-matrix-system) start-col &optional (cols  (- (size m) start-col )))
  (subseq (rhsl-start m) start-col (+ cols start-col)))



;;nastavi hodnotu ; chce to sem zakomponovat specialni plus, kdyz ta hodnota 
;; tj scitani cisel a scitani rovnic resp funkci
(defmethod set-rhsl-start-value ((m class-matrix-system) row op value)
  (setf 
    (aref (rhsl-start m)
      (get-position m row))
    (funcall op
      (aref (rhsl-start m)
        (get-position m row))
     value  )))


;stack vector
(defmethod get-sub-stack-vector ((m class-matrix-system) start-col &optional (cols  (- (size m) start-col )))
  (subseq (stack m) start-col (+ cols start-col)))


;(defmethod get-sub-rhs-vector ((m class-matrix-system) start-col cols)
;  (grid:slice (matrix m) `(,start-row (:range ,start-col ,cols )) :drop t))

;(defmethod set-sub-rhs-vector ((m class-matrix-system) start-row start-col var-vector)
;  (setf (grid:subgrid (rhs m) (list start-row start-col) '(1)) var-vector))


;get rhs veccotr
(defmethod get-sub-rhs-equations-vector ((m class-matrix-system) start-col &optional (cols  (- (size m) start-col )))
  (subseq (rhs-equations m) start-col (+ cols start-col)))



;get rhs veccotr
(defmethod get-sub-rhs-number-vector ((m class-matrix-system) start-col &optional (cols  (- (size m) start-col )))
  (subseq (rhs-number m) start-col (+ cols start-col)))



;;nastavi hodnotu ; chce to sem zakomponovat specialni plus, kdyz ta hodnota 
;; tj scitani cisel a scitani rovnic resp funkci
(defmethod set-rhs-number-value ((m class-matrix-system) row op value)
  (setf 
    (aref (rhs-number m)
      (get-position m row))
    (funcall op
      (aref (rhs-number m)
        (get-position m row))
     value  )))



;;nastavi hodnotu ; chce to sem zakomponovat specialni plus, kdyz ta hodnota 
;; tj scitani cisel a scitani rovnic resp funkci
(defmethod set-rhs-equations-value ((m class-matrix-system) row value)
 (push 
    value
    (aref (rhs-equations m) (get-position m row))))
 
 

;;nastavi hodnotu ; chce to sem zakomponovat specialni plus, kdyz ta hodnota 
;; tj scitani cisel a scitani rovnic resp funkci
(defmethod set-equations-value ((m class-matrix-system) row value)
  (push 
    value
    (aref (equations m) (get-position m row))))
    
      
(defmethod get-sub-equations-vector ((m class-matrix-system) start-col &optional (cols  (size m)))
  (subseq (equations m) start-col (+ cols start-col)))


    
;;navrati pozici prvku v matici, kdyz prvek v matici neni navrati max index a prvek do 
;; matice ulozi
(defmethod get-position ((m class-matrix-system) value)
; (print value)
  (let ((old-pos (position-if #'(lambda (x) (equal x value)) (stack m) :end (size m)))
        (new-pos (size m)))
    (cond (old-pos old-pos) 
          (t
            (set value 0)
            (setf (aref (stack m) new-pos) value)
            (setf (size m) (+ (size m) 1)) 
            new-pos))))
      

 
;;nastavi hodnotu ; chce to sem zakomponovat specialni plus, kdyz ta hodnota 
;; tj scitani cisel a scitani rovnic resp funkci
(defmethod set-g-value ((m class-matrix-system) row col op value)
;  (print row)
;  (print col)
;  (print value)
  (setf 
    (grid:gref (g-array (arrays m))
         (get-position m row) 
         (get-position m col))
    (funcall op 
      (grid:gref (g-array (arrays m)) 
          (get-position m row) 
          (get-position m col))
        value )))
 
    
;;navrati danou hodnotu
(defmethod get-g-array ((m class-matrix-system))
  (g-array (arrays m)))
  
(defmethod get-sub-g-array ((m class-matrix-system) start-row start-col rows cols)
  (grid:subgrid (g-array (arrays m)) (list rows cols) (list start-row start-col)))




;;nastavi hodnotu ; chce to sem zakomponovat specialni plus, kdyz ta hodnota 
;; tj scitani cisel a scitani rovnic resp funkci
(defmethod set-e-value ((m class-matrix-system) row col op value)
  (setf 
    (grid:gref (e-array (arrays m))
         (get-position m row) 
         (get-position m col))
    (funcall op 
      (grid:gref (e-array (arrays m)) 
          (get-position m row) 
          (get-position m col))
      value )))
 
    
;;navrati danou hodnotu
(defmethod get-e-array ((m class-matrix-system))
  (e-array (arrays m)))

;;navrati danou hodnotu
(defmethod get-sub-e-array ((m class-matrix-system) start-row start-col rows cols)
  (grid:subgrid (e-array (arrays m)) (list rows cols) (list start-row start-col)))



;; funkce ktera bude pracovat s foreing array gsd selector a vybere si potrebny kousek matice;



;;nastavi hodnotu ; chce to sem zakomponovat specialni plus, kdyz ta hodnota 
;; tj scitani cisel a scitani rovnic resp funkci
(defmethod set-z-value ((m class-matrix-system) row col op value)
  (setf 
    (grid:gref (z-array (arrays m))
         (get-position m row) 
         (get-position m col))
    (funcall op 
      (grid:gref (z-array (arrays m)) 
          (get-position m row) 
          (get-position m col))
      value  )))

 
    
;;navrati danou hodnotu
(defmethod get-z-array ((m class-matrix-system))
  (z-array (arrays m)))

(defmethod get-sub-z-array ((m class-matrix-system) start-row start-col rows cols)
  (grid:subgrid (z-array (arrays m)) (list rows cols) (list start-row start-col)))     
     
     
;;nastavi hodnotu ; chce to sem zakomponovat specialni plus, kdyz ta hodnota 
;; tj scitani cisel a scitani rovnic resp funkci
(defmethod set-d-value ((m class-matrix-system) row col value)
  (setf (linear m) nil)
    (push 
      value
      (aref (d-array (arrays m)) 
        (get-position m row) 
        (get-position m col))))
      
 
 
    
;;navrati danou hodnotu
(defmethod get-d-array ((m class-matrix-system))
  (d-array (arrays m)))     
     
(defmethod get-sub-d-array ((m class-matrix-system) start-row start-col rows cols)
  (grid:subgrid (d-array (arrays m)) (list rows cols) (list start-row start-col)))     
     

    



     
