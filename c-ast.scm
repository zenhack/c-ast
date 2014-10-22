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
  #:export (ast->iol))

(use-modules (ice-9 match)
             (iol))

(define (parens . expr) `("(" ,expr ")"))

(define expr->iol
  (match-lambda
    (('if test true false)
     (parens (expr->iol test) "?"
             (expr->iol true) ":"
             (expr->iol false)))
    ((op lhs rhs)
     (parens (expr->iol lhs) op (expr->iol rhs)))
    ;; We're assuming that anything else is going to be a raw atom that we can
    ;; just print directly:
    (expr expr)))

(define (decl->iol var ast)
  (match ast
    (('decl newvar newast) (decl->iol newvar newast))
    (('ptr ('decl newvar type))
     (decl->iol newvar `(ptr ,type)))
    (('ptr ('func . args)) (decl->iol (list "(*" var ")") (cons 'func args) ))
    (('func out-type . in-types)
     (list (decl->iol "" out-type) var "("
           (join-iol "," (map (lambda (type) decl->iol "" type) in-types))
           ")"))
    (sym (list sym " " var))))

(define stmt->iol
  (match-lambda
    (('while test body)
     (list " while " (expr->iol test) (stmt->iol body)))
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
      (stmt->iol
        ('if test-1
          (stmt->iol action-1)
            (stmt->iol
              `(if ,test-2 ,action-2 ,@rest)))))

    (('switch expr . cases)
     (list " switch (" (expr->iol expr) ") {"
           (map case->iol cases)
           "}"))
    (('goto label-name) (list " goto " label-name ";"))
    (('label label-name) (list label-name ":"))

    ('break "break;")
    ('continue "continue;")

    (expr (list (expr->iol expr) ";"))))

(define case->iol (match-lambda
  (('default . body)
   (list " default: "
    (stmt->iol (cons 'begin body))))
  ((expr . body)
   (list " case "
    (expr->iol expr) ": " (stmt->iol (cons 'begin body))))))

(define (ast->string ->iol ast) (iol->string (->iol ast)))
(define (stmt->string stmt) (ast->string stmt->iol stmt))
(define (expr->string expr) (ast->string expr->iol expr))

(define (ast->iol type ast)
  ((match type
    ('stmt stmt->iol)
    ('expr expr->iol)
    ('decl (lambda (ast) (decl->iol "" ast))))
   ast))
