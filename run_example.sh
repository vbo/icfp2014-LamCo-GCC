python ./macro/preprocess.py examples/$1.gcc examples/include > .input.gcc.tmp && \
vm/build.sh && vm/Main +RTS -xc -RTS .input.gcc.tmp
