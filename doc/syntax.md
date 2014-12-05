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

Also note: the current implementation does not check that the symbol is
a valid C identifier; if it is not, the error will not be caught before
the C compiler is invoked.

# Statements

A statement may be any of the following:

* A list beginning with the symbol `while`, with one or more additional
  elements. The first element must be an expression, which will be the
  test in the while statement. Any remaining elements must be
  statements, which will comprise the body of the while loop.

  Examples:

    (while 1) --> while(1);

    (while (== x 0)) -> while(x == 0);

    (while (< x y)
       (/= y 2))
    -->
    while(x < y)
        y /= 2;

    (while more_to_do
        (call do_thing)
        (-- more_to_do))
    -->
    while(more_to_do) {
        do_thing();
        --more_to_do;
    }

* A list beginning with the symbol `begin`, whose remaining elements are
  statements. This produces a block statement:

    (begin
        (= x 1)
        (call printf "%d\n" x))
    -->
    {
        x = 1;
        printf("%\dn", x);
    }

* A list containing the symbol `do-while`, a statement, and an
  expression (in that order). the statement will be the body of a
  do-while loop, and the expresion will be the test. Right now only one
  statement is supported, which is inconsistent with the while statement
  This will change in the future, in a backwards-compatible way. The
  `begin` statement may be used for multi-statement bodies.

  Examples:

    (do-while (begin) 1) --> do {} while(1);

    (do (begin) (== x 0)) --> do {} while(x == 0);

    (do
      (/= y 2)
    (< x y))
    -->
    do {
        y /= 2;
    } while(x < y);

* An s-expression of the form:

    (for (init test incr)
        body)

  Where

  * `for` is the literal symbol `for`
  * `init`, `test`, and `incr`, which are expressions, corresponding to
    the usual parts of a for loop, and
  * body is a statement, corresponding to the body of the for loop.
    Much like the do-while loop, this will be extended to support
    multiple statements directly in the future, but `begin` may be used
    as a workaround.

  Examples:

    (for ((= i 0) (< i limit) (++ i))
        (call printf "%d\n" i))
    -->
    for(i = 0; i < limit; ++i) {
        printf("%d\n, i);
    }

* A list beginning with the symbol `if`, and having two or more
  additional elements. It is easier to demonstrate than explain:


    (if 1 (= x 0)) --> if(1) x = 0;

    (if 1 (= x 0)
          (= x 7))
    -->
    if(1)
        x = 0;
    else
        x = 7;

    (if 1 (= x 0)
        2 (= x 7))
    -->
    if(1)
        x = 0;
    else if(2)
        x = 7;

    (if 1 (= x 0)
        2 (= x 7)
          (= x 3))
    -->
    if(1)
        x = 0;
    else if(2)
        x = 7;
    else
        x = 3;


  More generally,

    (if test stmt) --> if(test) stmt;

    (if test stmtTrue stmtFalse)
    -->
    if(test)
        stmtTrue;
    else
        stmtFalse


  and,

    (if test1
      stmt1
    test2
      stmt2
    . rest)

  is the same as:

    (if test1
      stmt1
    (if test2
      stmt2
    . rest))

* A list starting with the symbol `switch` and an expression, and
  containing zero or more "cases," each of which is a list starting with
  either an expression or the symbol `default`, and containing zero or
  more statements.

  TODO: finish explaining this, and add some examples.
