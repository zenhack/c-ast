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
(define-module (c-ast)
  #:export (ast->iol
            c-escape-string))

(use-modules (ice-9 match)
             (iol))

(define c-escape-char
  (match-lambda
    ;; This needs to escape both ' and " always, since it may be used for either
    ;; char literals or string literals.
    (#\"       "\\\"")
    (#\'       "\\'")
    (#\newline "\\n")
    (#\tab     "\\t")
    (#\\       "\\\\")
    (other     other)))

(define (c-escape-string s)
  (list "\"" (map c-escape-char (string->list s)) "\""))

(define (parens . expr) `("(" ,expr ")"))

(define expr->iol
  (match-lambda
    (('if test true false)
     (parens (expr->iol test) "?"
             (expr->iol true) ":"
             (expr->iol false)))
    (('call func . args)
     (list (parens (expr->iol func))
           (parens (join-iol ", " (map expr->iol args)))))
    (('@ arr index)
     (list (expr->iol arr) "[" (expr->iol index) "]"))
    ((op lhs rhs)
     (parens (expr->iol lhs) op (expr->iol rhs)))
    ((op arg)
     (parens op (parens arg)))
    (expr
      (cond
        ((string? expr) (c-escape-string expr))
        ((char? expr) (list "'" (c-escape-char expr) "'"))
        ;; We're assuming that anything else is going to be a raw atom that we
        ;; can just print directly:
        (#t expr)))))

(define (decl->iol var ast)
  (match ast
    (('decl newvar newast) (decl->iol newvar newast))
    (('ptr ('decl newvar type))
     (decl->iol newvar `(ptr ,type)))
    (('ptr ('func . args)) (decl->iol (list "(*" var ")") (cons 'func args)))
    (('ptr type) (decl->iol (list "*" var) type))
    (('func out-type . in-types)
     (list (decl->iol "" out-type) var "("
           (join-iol "," (map (lambda (type) (decl->iol "" type)) in-types))
           ")"))
    ((name type) (decl->iol name type))
    (sym (list sym " " var))))

(define def->iol
  (match-lambda
    (('def var ('func . ret/args) . body)
     (list (decl->iol var (cons 'func ret/args)) (stmt->iol (cons 'begin body))))
    (('def var type val)
     (list (decl->iol var type) " = " (expr->iol val) ";"))))

(define stmt->iol
  (match-lambda
    (('while test body)
     (list " while " (parens (expr->iol test)) (stmt->iol body)))
    (('while test)
     (list " while " (parens (expr->iol test)) ";"))
    (('begin . body)
     (list "{" (map stmt->iol body) "}"))
    (('do-while body test)
     (list " do " (stmt->iol body) " while " (stmt->iol test) ";"))
    (('for (init test incr) body)
     (list
       " for(" (expr->iol init) ";"
               (expr->iol test) ";"
               (expr->iol incr) ")"
        (stmt->iol body)))

    (('if test true)
     (list " if " (expr->iol test)
           (stmt->iol true)))
    (('if test true false)
     (list " if " (expr->iol test)
            (stmt->iol true)
           " else "
            (stmt->iol false)))
    (('if test-1 action-1 test-2 action-2 . rest)
     (begin
       (write `(if ,test-1 ,action-1 ,test-2 ,action-2 ,@rest))
       (exit)
      (stmt->iol
        ('if test-1
          (stmt->iol action-1)
            (stmt->iol
              `(if ,test-2 ,action-2 ,@rest))))))

    (('switch expr . cases)
     (list " switch (" (expr->iol expr) ") {"
           (map case->iol cases)
           "}"))
    (('goto label-name) (list " goto " label-name ";"))
    (('label label-name) (list label-name ":"))
    (('return expr) (list "return " (expr->iol expr) ";"))

    ('break "break;")
    ('continue "continue;")
    (('decl . rest) (list (decl->iol "" (cons 'decl rest)) ";"))
    (('def . rest) (def->iol (cons 'def rest)))

    (expr (list (expr->iol expr) ";"))))

(define case->iol (match-lambda
  (('default . body)
   (list " default: "
    (stmt->iol (cons 'begin body))))
  ((expr . body)
   (list " case "
    (expr->iol expr) ": " (stmt->iol (cons 'begin body))))))

(define toplevel->iol (match-lambda
  (('begin . toplevels) (map toplevel->iol toplevels))
  (('!define sym) (list "\n#define " sym))
  (('!include<> filename) (list "\n#include <" filename ">\n"))
  (('!include filename) (list "\n#include \"" filename "\""))
  (('decl . rest) (decl->iol "" (cons 'decl rest)))
  (('def . rest) (def->iol (cons 'def rest)))))

(define (ast->string ->iol ast) (iol->string (->iol ast)))
(define (stmt->string stmt) (ast->string stmt->iol stmt))
(define (expr->string expr) (ast->string expr->iol expr))

(define (ast->iol type ast)
  ((match type
    ('toplevel toplevel->iol)
    ('stmt stmt->iol)
    ('expr expr->iol)
    ('decl (lambda (ast) (decl->iol "" ast)))
    ('def def->iol))
   ast))
