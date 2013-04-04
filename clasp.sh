#!/bin/sh
BREAK_CHARS="(){}[],^%$#@\"\";''|\\!?"



#RLWRAP=
#if [ $TERM == "dumb" ]; then  # slime
#  RLWRAP=
#else
RLWRAP="rlwrap --remember --history-filename=$HOME/.sbcl_history --histsize=1000000 -c -b $BREAK_CHARS -i -f $HOME/.sbcl_completions_list "
#fi
#if [ $# -eq 0 ]; then

#sbcl --script "$HOME/.sbcl_completions" |

#exec $RLWRAP sbcl 
#else # permits #!/usr/bin/env sbcl , but breaks sbcl --help, etc.
#  exec sbcl --script $*
#fi


exec $RLWRAP sbcl --load "clasp-init.lisp"

