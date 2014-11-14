(use-modules (compiler))
(ld "cat"
    (cc "cat"

        '(!include<> "stdio.h")
        '(!include<> "string.h")

        '(def copy (func int (in (ptr FILE)) (out (ptr FILE)))
              (decl c int)

              (while 1
                (begin
                  (= c (call fgetc in))
                  (if (== c EOF)
                    (return 0))
                  (call fputc c out))))

        '(def main (func int (argc int) (argv (ptr (ptr char))))
              (decl i int)

              (if (<= argc 1)
                (begin
                  (call copy stdin stdout)
                  (return 0)))

              (for ((= i 1) (< i argc) (++ i))
                (if (== (call strcmp (@ argv i) "-") 0)
                  (call copy stdin stdout)
                  (begin
                    (def file (ptr FILE) (call fopen (@ argv i) "r"))
                    (call copy file stdout)
                    (call fclose file))))
              (return 0))))
