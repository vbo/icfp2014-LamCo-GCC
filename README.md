LamCo GCC (ICFP Contest 2014)
=============================

This is a basic software implementation of LamCo GCC (also known
as "Lambda-Man CPU") - sophisticated hardware co-processor from ICFP Contest 2014.

Specification can be found here: http://icfpcontest.org/specification.html

Actual "virtual machine" able to execute GCC assembly instructions is written in Haskell.

    Building: ./vm/build.sh
        requires ghc
        deps: cabal install pretty-show

    Usage: ./vm/Main raw.gcc
        Loads assembly code from raw.gcc and starts a kind of "debugging session"
        where you can execute instructions step-by-step (st)
        and watch how machine state changes.
        
        Alternatively you can continue to the next breakpoint (co).

I also found it nice to have a very basic macro pre-processor available to simplify assembly coding.
It is python-based.

    Usage: python macro/preprocess.py path/to/main.gcc include/path

`examples` directory contains some example programs you can play with. Simplest
way to run example program is to use use `run_example.sh`.

    Usage: ./run_example.sh example_prog_name

This script launches a macro pre-processor first and then compiles and runs vm over resulting assembly.

Notes
-----
Implementation progress.

Done
 - Core: STOP, LDC, LD, ADD, SUB, MUL, DIV, CEQ, CGT, CGTE, ATOM, CONS, CAR, CDR, SEL, JOIN, LDF, AP, RTN
 - Tail-call: TSEL
 - Debug: BRK
 - Garbage collector

To-do
 - Core: DUM, RAP
 - Tail-call: TAP, TRAP
 - Debug: DBUG
 - Imperative: ST
