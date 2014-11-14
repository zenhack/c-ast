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
(use-modules (c-ast)
             (iol)
             (ice-9 rdelim)
             (ice-9 popen))

(define workdir (format #f "_test_work_~a" (getpid)))
(mkdir workdir)
(chdir workdir)
(format #t "test working directory: ~a\n" workdir)

(define *next-file* 0)
(define (next-file)
  (let ((ret *next-file*))
    (set! *next-file* (+ 1 ret))
    ret))

(define (read-port port)
  "Read the whole input from port, returning it as a string."
  (define (read-port* buf)
    (let ((line (read-line port 'concat)))
      (if (eof-object? line)
        buf
        (read-port* (list buf line)))))
  (iol->string (read-port* "")))

(define (indent-iol iol)
  "Run gnu indent on iol, returning the result."
  (let ((filename (format #f "~a" (next-file))))
    (with-output-to-file filename (lambda ()
      (display-iol iol)))
    (let* ((pipe (open-pipe* OPEN_READ "indent" "-br" "-st" filename))
           (text (read-port pipe)))
        (close-pipe pipe)
        text)))

(define-syntax ast-eq-tests
  (syntax-rules ()
    ((ast-eq-tests (type str ast) ...)
     (list
       (lambda ()
         (let ((istr (indent-iol str))
               (iast (indent-iol (ast->iol 'type ast))))
           (if (equal? iast istr)
             'pass
             `(ast-eq
                (input-ast ,ast)
                (expected ,istr)
                (actual ,iast))))) ...))))

(define-syntax iol-eq-tests
  (syntax-rules ()
    ((iol-eq-tests (expected actual) ...)
     (list (lambda ()
             (let ((expected-string (iol->string actual))
                   (actual-string (iol->string actual)))
               (if (equal? expected-string actual-string)
                 'pass
                 `(iol-eq
                    (input ,actual)
                    (expected ,expected-string)
                    (actual ,actual-string))))) ...))))

(define *tests*
  (append
    (ast-eq-tests
      (stmt
        "(x = 1);"
        '(= x 1))
      (stmt
        "goto fail;"
        '(goto fail))
      (stmt
        "return -1;"
        '(return -1))
      (stmt
       "while ((x>1)) {
           if ((x % 2) == 0) {
               4;
               (x = 82);
           } else
               7;
       }"
      '(while (> x 1)
        (begin
          (if (== (% x 2) 0)
              (begin
                4
                (= x 82))
              7))))
      (stmt
        "while (1);"
        '(while 1))
      (stmt
        "while (1) {
        }"
        '(while 1 (begin)))
      (stmt
        "switch (x) {
           case 4: {
             (x = 1);
             break;
           } default: {
             (x = 4);
           }
         }"
         '(switch x
            (4
              (= x 1)
              break)
            (default
              (= x 4))))
        (stmt
          "int x = 1;"
          '(def x int 1))
        (stmt
          "int x;"
          '(decl x int))
      (decl
        "int"
        'int)
      (decl
        "int x"
        '(decl x int))
      (decl
        "int main(void)"
        '(decl main (func int void)))
      (decl
        "int max(int, int)"
        '(decl max (func int int int)))
      (decl
        "int *x"
        '(decl x (ptr int)))
      (decl
        "char **argv"
        '(decl argv (ptr (ptr char))))
      (decl
        "void *malloc()"
        '(decl malloc (func (ptr void))))
      (decl
        "void ptrarg(void *)"
        '(decl ptrarg (func void (ptr void))))
      (decl
        "int main(int argc, char **argv)"
        '(decl main (func int (argc int) (argv (ptr (ptr char))))))
      (expr "4" 4)
      (expr "'c'" #\c)
      (expr "'\\n'" #\newline)
      (expr
        "(x = 1)"
        '(= x 1))
      (expr
        "(1?2:3)"
        '(if 1 2 3))
      (expr
        "(x += 4)"
        '(+= x 4))
      (expr
        "(*(x))"
        '(* x))
      (expr
        "(&(x))"
        '(& x))
      (expr
        "argv[2]"
        '(@ argv 2))
      (def
        "int x = 1;"
        '(def x int 1))
      (def
        "int *x = NULL;"
        '(def x (ptr int) NULL))
      (def
        "int main() {}"
        '(def main (func int)))
      (def
        "int main() {
          (x = 1);
         }"
         '(def main (func int)
               (= x 1)))
      (def
        "int main(void) {
          return 0;
        }"
        '(def main (func int void)
              (return 0)))
      (def
        "int main(void) {
           (x = 0);
           (y = 1);
         }"
         '(def main (func int void)
               (= x 0)
               (= y 1)))
    )
    (iol-eq-tests
      ("a,b,c" (join-iol "," '("a" "b" "c")))
    )))

(write (filter (lambda (ret) (not (equal? ret 'pass)))
               (map (lambda (f) (f)) *tests*)))
