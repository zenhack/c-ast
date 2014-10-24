
(define-module (compiler)
  #:export (cc ld))

(use-modules (c-ast)
             (iol))

(define (ast->srcfile name tree)
  (with-output-to-file (string-append name "_gen.c")
    (lambda ()
      (display-iol (ast->iol 'def tree)))))

(define (cc name ast)
  (ast->srcfile name ast)
  (system* "cc" "-c"
           "-o" (string-append name "_gen.o")
           (string-append name "_gen.c"))
  (string-append name "_gen.o"))

(define (ld out . objects)
  (display objects)
  (apply system* (append (list "cc" "-o" out)
                         objects)))
