echo "+------------------------------------------------------------------------+"
echo "| Please make sure you have installed the following modules from our     |"
echo "| project sites and set up ASDF to find them:                            |"
echo "|  * HEXAMETER for LISP (:hexameter)                                     |"
echo "|  * HRL (:hrl)                                                          |"
echo "+------------------------------------------------------------------------+"
echo
echo "Now running Platon's mind..."
echo
sbcl --load ../Sources/Lisp/platonsmindrescue.lisp --end-toplevel-options localhost:55559
# script doesn't work because then we cannot laod any library, but maybe we can fix this in the future
#sbcl --script ../Sources/Lisp/platonsmind.lisp
