;Programmer: Giovanni Vecchione
;Date:9/5/23
;
;Subject: Write a recursive function that calculates the  1 / (x ^ 4) to 100 terms. 

#lang racket

(define (sumFunction x) ;defining the function
    (if (< x 2)      ;checks if x is less then 2, otherwise runs statement
        (/ 1 (expt 1 4)) ;initial expression of 1 / 1 ^ 4 which is the formula
        (+ (/ 1 (expt x 4)) (sumFunction (- x 1))))) ; recursion occurs here we run the expression 1 / x ^ 4
; the (sumFunction - x 1) recursively calls the function x - 1 times meaning x is the count or argument passed

(exact->inexact (sumFunction 100))

