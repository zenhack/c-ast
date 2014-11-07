;; Copyright 2014 Ian Denhardt <ian@zenhack.net>
;;
;; This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>
(define-module (compiler)
  #:export (cc ld))

(use-modules (c-ast)
             (iol))

(define (ast->srcfile name tree)
  (with-output-to-file (string-append name "_gen.c")
    (lambda ()
      (display-iol (map (lambda (ast) (ast->iol 'toplevel ast)) tree)))))

(define (cc name . ast)
  "Compile the ast into a source/object file pair named name_gen.[co].

i.e., if name is the string \"hello\", then the c source code for ast
will be placed in the file hello_gen.c, which will be compiled to the
object file hello_gen.o.

The remaining arguments should each be a \"toplevel\" ast, such as a
preprocessor directive, (!include, !define, ...), function/variable
definition, or TODO define this better and document it.
"
  (ast->srcfile name ast)
  (system* "cc" "-c"
           "-o" (string-append name "_gen.o")
           (string-append name "_gen.c"))
  (string-append name "_gen.o"))

(define (ld out . objects)
  "link the object files listed in \"objects\" into an executable"
  (display objects)
  (apply system* (append (list "cc" "-o" out)
                         objects)))
