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

(define-syntax tests
  (syntax-rules ()
    ((tests (type str ast) ...)
     (list
       (lambda ()
         (let ((istr (indent-iol str))
               (iast (indent-iol (ast->iol 'type ast))))
           (if (equal? iast istr)
             'pass
             `(ast-format-stmt
                (input-ast ,ast)
                (expected ,istr)
                (actual ,iast)))) ...)))))

(define *tests*
  (tests
    (stmt
     "while (x>1) {
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
  ))


(write (map (lambda (f) (f)) *tests*))
