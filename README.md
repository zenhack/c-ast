C-Ast is a GNU Guile library for generating C code. There are some
examples in the example directory, but briefly, it lets you turn
s-expresions like:

    (for ((= i 0) (< i limit) (+= i 1))
        (call printf "i = %d\n" i))

Into C source code:

    for (i = 0; i < limit; i += 1) {
        printf("i = %d\n", i);
    }

...which can then be fed through a C compiler.

This is interesting because it allows use of scheme to easily manipulate
C programs -- many the same benefits of a proper lisp-style macro system
can be used to write low-level code.

The intention is for this to be a building block for building other
tools.

C-Ast is still in a *very* early stage of development -- you can write
hello world, but it's missing a lot of important features and some
documentation, and probably isn't packaged the way a guile library
should be -- I'm still very new to the language.

# Requirements

To run the test suite, you will need an implementation of srfi-64. The
one I use is: <https://gitorious.org/srfi-64-port>.
