# CLASP

CLASP is an unusual and efficient usage of functional programming language Common LISP as simulation program (CLASP) for electronic circuits. The principle of automatic self-modifying program has enabled complete freedom in definition of methods for optimized solution of any problem and speeding up the entire process of simulation. 




# Change Log

## 22.4. 2012  


### New loading sequence

All simualtor is loaded by ./clasp.sh (without rlwrap) or clasp-wrap (with rlwrap).




## 15.4. 2013  

### Loading Simulator - deprecated

  
For initial simulator load use:
    
    (asdf:operate 'asdf:load-op :clasp)
    (ql:quickload "gsll")
    (ql:quickload "grid")
    (ql:quickload "foreign-array")

### Commands

To get updated all dist

    (ql:update-all-dists)

To update ql client

    (ql:update-client)

Matrix definition

    (defparameter m1 #m(1 2 3 ^ 0 6 8))
