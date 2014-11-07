(use-modules (compiler))

(ld "hello-world"
    (cc "hello-world"
        '(!include<> "stdio.h")

        '(def main (func int void)
              (call printf "Hello, World!\n")
              (return 0))))
