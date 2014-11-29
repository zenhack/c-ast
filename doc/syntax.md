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

# Definitions

Definitions are similar to declarations, but also include one or more
"values":

    (def var typ val...)

In the case of variable types, only one value is allowed, which is
an expression to be assigned to the variable. In the case of functions,
There may be any number of values, each of which must be a statement.
Together the values make up the body of the function.

Some examples:

    (def x int 7) --> int x = 7;
    (def main (func void void) (return 0)) --> void main(void) { return 0; }

# Types

Declarations and definitions require the specification of types. A type
may be any of the following:

1. A list starting with the symbol `func`, whose second element, and zero
or more additional elements, are types. The second element is the type
of the return value, and the remaining elements are the types of the
arguments. Some examples:

    (decl name (func void void)) --> void name(void);
    (decl name (func void)) --> void name();
    (decl name (func int char double)) -> int name(char, double);

2. A three element list,

 * whose first element is the symbol `decl`,
 * whose second element is a symbol, and
 * whose third element is a type.

The `(decl ...)` form has the effect of naming the type that it wraps.
This is mainly of interest in cases where an argument inside of a larger
declaration (or, more commonly, definition), must have a name. For example:

    (def sum (func int (decl x int) (decl y int))
      (return (+ x y)))

Yields:

    int sum(int x, int y) {
        return x + y;
    }

TODO/IMPLEMENTATION NOTE: The semantics of decl are a bit sublter than
this, and used internally for various things. This should be explained
in more detail.

3. A list of the form `(ptr <type>)` where `<type>` is another type.
This denotes a pointer to the wrapped type.

4. A two element list, the first of which is a symbol, the second of
which is a type. This is a shorthand for the `decl` form described
above. The `sum` example could also be written as:

    (def sum (func int (x int) (y int))
      (return (+ x y)))

Note well: users should be careful not to use this form with a variable
name that carries special meaning to the library -- as an example,
`(ptr int)` will be intepreted as "pointer to int," not "int named
'ptr'". This shorthand should be avoided where collisions are possible.

5. A literal symbol, which will be interpreted as the name of the type,
e.g. `int`, `float`, `uint32_t`, `myType`. Note that the library knows
nothing about what types are available -- it is the user's
responsibility to ensure that the specified types are declared
somewhere. Otherwise, the error may not be caught until the C compiler
is invoked.
