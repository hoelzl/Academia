echo "Please run ../Sources/Lisp/platonsmind.lisp in hexameter-lisp! (And accept the compile error from `cc`)"
# Loading pzmq currently crashes in my machine, but can be saved by accepting the error in slime
#sbcl --script ./platonsmind.lisp # this is how this should work in the end