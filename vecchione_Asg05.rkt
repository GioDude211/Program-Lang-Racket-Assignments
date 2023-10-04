;Programmer: Giovanni Vecchione
;Date: 9/25/23
;Subject: Asg 05

#lang racket

(define (secant-method f x0 x1 accuracy)
    (let* ([fx0 (f x0)] ; Compute f(x0)
         [fx1 (f x1)] ; Compute f(x1)
         [x2 (- x1 (/ (* fx1 (- x1 x0)) (- fx1 fx0)))] ; Compute the next approximation
         [error (abs (- x2 x1))]) ; Compute the error
    (if (< error accuracy)
        x2 ; Return x2 if the error is within the desired accuracy
        (secant-method f x1 x2 accuracy)))) ; Otherwise, recur

; Example usage:
(define (poly-fn x)
    (- (* 9 x x) (* 3 x) 25)) ; Defining the polynomial function 9x^2 - 3x - 25

(define (poly-fn2 x)
    (- (* 11 x x) (* 2 x) 50)) ; Defining the polynomial function 11x^2 - 2x - 50

(define root (secant-method poly-fn 0.0 4.0 .00001)) ; Find the root with initial approximations 0.0 and 5.0 and desired accuracy 1e-9
(define root2 (secant-method poly-fn2 -5.0 1 .00001 )) ; starting values -5 and 1

(displayln (number->string root)) ; Display the found root
(displayln (number->string root2)) ; Display the found root