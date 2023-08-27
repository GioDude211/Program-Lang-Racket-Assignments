#lang racket

(define (calculate-expression x)
    + 30 (* 2 x) (-(* 3 x x)))

(define (print-different-output x)
    (define result (calculate-expression x))
    (cond
        ((> result 10) (display "The result is greater than 10: ") (display result))
        ((< result 10) (display "The result is less than  -10: ") (display result))
        (else (display "The result is between -10 and 10: ") (display result))))

; Calculate and print the result of the expression for x = 5
(display "Result: ")
(print-different-output 5)
(newline)