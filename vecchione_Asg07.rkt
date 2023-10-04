; Programmer: Giovanni Vecchione
; Date: 10/3/23
; Subject: Asg 07
;Write a recursive function that takes a list of numbers as input and returns a list of the numbers in ascending order.
;Use the method described in class.

#lang racket

;Function: myBadSort
;Example of using a poor sorting algorithm with recursion

(define (myBadSort lst)
  (define (findMax list)
    (if (empty? (rest list))
        (first list)
        (max (first list) (findMax (rest list)))))
  (define (myRemove item list)
    (cond [(empty? list) '()]
          [(= item (first list)) (rest list)]
          [else (cons (first list) (myRemove item (rest list)))]))
  (if (empty? lst)
      '()
      (let ((max-val (findMax lst)))
        (append (myBadSort (myRemove max-val lst)) (list max-val)))))

;; Example usage:
(myBadSort '(20 13 74 5 12 9 22 95 22 6 101 72 3 53 33 21 96))
