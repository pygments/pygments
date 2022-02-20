#!r6rs

#|

   The FACT procedure computes the factorial

   of a non-negative integer.

   #| These comments can be nested too. |#

|#

(define fact

  (lambda (n)

    ;; base case

    (if (= n 0)

        #;(= n 1)
        #;(= n [1+ (eval '(n))])1
        #;[= n (1+ [eval '[n]])];; another comment
        #;1
        ; identity of *

        (* n (fact (- n 1))))))
