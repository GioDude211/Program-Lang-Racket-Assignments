;Programmer: Giovanni Vecchione
;Date: 9/12/23
;Asg 04
;
;Objective:Write two recursive functions that calculate the mercator series to 100 terms.
;In the first version, determine the sign of each term with a named subprogram.
;The second version uses a lambda function instead of the named subprogram.
;Create a .rkt file that contains the definitions and tests the functions.
;
;x - x2/2 + x3/3 - x4/4 ... = ln(x+1)

#lang racket


; Named recursive function for mercator
(define (mercator-named n)
  (define (sign i)
    (if (even? i) 1 -1))
  
  (define (term i)
    (/ (sign i) (exact->inexact i)))
  
  (define (mercator-recursive n i sum)
    (if (= i n)
        sum
        (mercator-recursive n (+ i 1) (+ sum (term i)))))
  
  (mercator-recursive n 1 0))

; Un-named recursive function for mercator
(define (mercator-lambda n)
  (define (term i)
    (/ ((if (even? i) (lambda (x) x) (lambda (x) (- x))) i) (exact->inexact i)))
  
  (define (mercator-recursive n i sum)
    (if (= i n)
        sum
        (mercator-recursive n (+ i 1) (+ sum (term i)))))
  
  (mercator-recursive n 1 0))

(provide mercator-named mercator-lambda)

; Testing functions
(define (test-mercator function-name)
  (display (format "Testing ~a...\n" function-name))
  (define result (function-name 100))
  (display (format "Mercator(100) = ~a\n" result)))

(test-mercator mercator-named)
(test-mercator mercator-lambda)
