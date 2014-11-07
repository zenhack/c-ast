(use-modules (compiler))

;; Compile and link the simplest possible correct program:
(ld "empty" (cc "emtpy"
        '(def main (func int void)
              (return 0))))
