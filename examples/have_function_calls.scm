(use-modules (compiler))

(cc "have_function_calls"
    '(!include<> "stdio.h")
    '(def have_function_calls (func int void)
           (return 0)))
