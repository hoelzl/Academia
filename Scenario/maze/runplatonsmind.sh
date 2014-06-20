echo "Now running Platon's mind... (Compilation may take a few seconds)"
echo
sbcl --noinform --disable-ldb --lose-on-corruption --no-sysinit --disable-debugger --load ./platonsmind.lisp --end-toplevel-options localhost:55559
