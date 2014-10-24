(use-modules (compiler))

;; Compile and link the simplest possible correct program:
(ld "hello" (cc "hello"
        '(def main (func int void)
              (return 0))))
