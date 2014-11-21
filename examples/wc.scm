
(use-modules (ice-9 match)
             (compiler))

(define (decl-chartype var)
  `(begin (decl ,var int)
          (decl ,(symbol-append 'print_ var) int)))

(define check-flag
  (match-lambda
    ((var switch)
     `((== (call strcmp (@ argv i) ,switch) 0) (= ,(symbol-append 'print_ var) 1)))))

(ld "wc"
    (cc "wc"
      `(begin

        (!include<> "stdio.h")
        (!include<> "string.h")
        
        ,@(map decl-chartype '(chars words lines))

        (def wc (func void (input (ptr FILE)))
             (def c int 0)
             (= c (call fgetc input))
             (while (!= c EOF)
                 (begin
                    (switch c
                      (#\newline (++ lines) (break))
                      (#\space (++ words) (break)))
                    (++ chars)
                    (= c (call fgetc input)))))

        (def show (func void)
             (if (! (|| print_chars (|| print_words print_lines)))
               (call printf "chars: %d\nwords: %d\nlines: %d\n"
                     chars words lines)
               (begin
                 (if print_chars
                   (call printf "chars: %d\n"))
                 (if print_words
                   (call printf "words: %d\n"))
                 (if print_lines
                   (call printf "lines: %d\n")))))


        (def main (func int (argc int) (argv (ptr (ptr char))))
             (decl i int)
             (def have_file int 0)
             (for ((= i 0) (< i argc) (++ i))
               (begin
                 (if ,@(apply append (map check-flag `((chars "-c")
                                                       (words "-w")
                                                       (lines "-l"))))
                       (begin
                         (= have_file 1)
                         (decl input (ptr FILE))
                         (= input (call fopen (@ argv i) "r"))
                         (call wc input)
                         (call fclose input)))))
             (if (! have_file)
               (call wc stdin))

             (call show)
             (return 0)))))

