

# CLASP

CLASP is an unusual and efficient usage of functional programming language Common LISP as simulation program (CLASP) for electronic circuits. The principle of automatic self-modifying program has enabled complete freedom in definition of methods for optimized solution of any problem and speeding up the entire process of simulation. 




loading agter install
    (load "~/quicklisp/setup.lisp")


# Change Log


3.11. 2014



sudo apt-get install libgsl0ldbl



Installation of quick lisp to new computer
curl -O http://beta.quicklisp.org/quicklisp.lisp
curl -O http://beta.quicklisp.org/quicklisp.lisp.asc
gpg --verify quicklisp.lisp.asc
sbcl --load quicklisp.lisp

(quicklisp-quickstart:install)


(ql:quickload "gsll")
(ql:quickload "grid")
(ql:quickload "foreign-array")

(ql:add-to-init-file)

(ql:quickload "antik")

(load "~/quicklisp/setup.lisp")





OBSOLETE; GSD has been replaced by Antik

Operations on data structured as a grid, i.e., generalized arrays. After cloning, see the manual in documentation/grid/index.html. 




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
