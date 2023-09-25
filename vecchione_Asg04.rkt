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
;CHECK

; Named recursive function for mercator
(define (findSign i)
  (if (= (remainder i 2) 0)
      -1.0
      1.0))

; Un-named recursive function for mercator
(define (mercator i x)
  (if (< i 2)
      x
      (+ (/ (* (findSign i) (expt x i)) i)
         (mercator (- i 1) x))))

(define (mercator-unamed i x)
  (if (< i 2)
      x
      (+ (/ (* ((lambda (i) (if (= (remainder i 2) 0) -1.0 1.0)) i)
              (expt x i))
         i)
         (mercator (- i 1) x))))

(mercator 100 0.5) 

(mercator 100 -0.5) 

(mercator-unamed 100 0.5)
(mercator-unamed 100 -0.5)