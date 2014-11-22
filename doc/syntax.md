This document describes the format of s-expressions accepted by the
c-ast library, and their translations.

# Top-level forms

Several categories of forms are allowed at top-level. Specifically:

* Preprocessor directives
* Variable/function declarations/definitions

The following are not yet implemented, but are planned:

* typedefs

In addition, the form `(begin one two ...)` can be used to group
multiple top-level forms without changing their output. This is
useful for generating files programmatically.

## Preprocessor directives

The general convention is that #foo becomes !foo. So far we support:

* `(!include<> header-name)` for `#include <header-name>`, where
  `header-name`` is a string.
* `(!include header-name)` for `(#include "header-name")`, where
  `header-name` is a string.
* `!define FOO` for `#define FOO`. `FOO` should be a symbol.

   Note that no syntax for `#define FOO "bar"` exists. Something
   like this will be supported eventually, but the design is somewhat
   subtle -- since `#define` works at the lexical level, it's not
   totally clear what should be done with the argument -- expression?
   statement? Raw lexeme?

# Declarations

A declaration looks like:

    (decl var type)

Where `var` is a symbol representing the variable  or function name,
and `type` is the type (see the section on type syntax).

Qualifiers, e.g. the `static` keyword, are not currently supported, but
are planned.

Some exmaples:

    (decl x int) --> int x;
    (decl main (func void void)) --> void main(void);
