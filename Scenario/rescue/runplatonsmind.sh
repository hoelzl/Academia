echo "Now running Platon's mind... (Please bare with me through all the compiling!)"
echo
sbcl  --noinform --disable-ldb  --lose-on-corruption --no-sysinit --disable-debugger --load ./platonsmind.lisp --end-toplevel-options localhost:55559 ./model-01.lisp
# script option doesn't work because then we cannot load (quicklisp) library dependencies then
#sbcl --script ./platonsmindrescue.lisp
