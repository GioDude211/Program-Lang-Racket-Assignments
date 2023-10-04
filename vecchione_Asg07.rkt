; Programmer: Giovanni Vecchione
; Date: 10/3/23
; Subject: Asg 07
;Write a recursive function that takes a list of numbers as input and returns a list of the numbers in ascending order.
;Use the method described in class.

#lang racket

;Function: myBadSort
;Example of using a poor sorting algorithm with recursion

(define (myBadSort lst)
  (define (findMax list) ; finds the maximum value in 'lst' which is the list
    (if (empty? (rest list)) ; if its empty it skips
        (first list) ; if theres only one number it returns it
        (max (first list) (findMax (rest list))))) ; Recursive case: return the max of the first item and the max of the rest of the list
  (define (myRemove item list) ; removes the first instance of the 'item' which is the element from the 'lst'
    (cond [(empty? list) '()] ; if emtpy returns empty lst
          [(= item (first list)) (rest list)] ; if 'item' is equal to the first item of 'lst', return rest of 'lst'
          [else (cons (first list) (myRemove item (rest list)))])) ; otherwise, keep first item and remove 'item' from rest of list using recur
  (if (empty? lst) ; if empty returns empty list
      '()
      (let ((max-val (findMax lst))) ; finds max val in the list 'lst' and uses findMax
        (append (myBadSort (myRemove max-val lst)) (list max-val))))) ;Recursively sort lst without its max val using myBadSort and myRemove

;; Test Run:
(myBadSort '(20 13 74 5 12 9 22 95 22 6 101 72 3 53 33 21 96))
; returns '(3 5 6 9 12 13 20 21 22 22 33 53 72 74 95 96 101)
