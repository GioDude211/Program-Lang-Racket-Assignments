
;Programmer: Giovanni Vecchione
;Date: 9/25/23
;Subject: Asg 06
;1.Write a recursive function listLength that counts the number of items in a list.
;You may not use the racket functions ‘length’, ‘set!’, ‘filter’, ‘takef’, ‘dropf’ or ‘flatten’.
;2.Write a recursive function deepListLength that return the number of atoms (things that are not lists) in a list.
;Your functions are not required to be tail recursive



#lang racket

(define (listLength lst)
  (cond
    [(null? lst) 0] ; if the list is empty, return 0
    [else (+ 1 (listLength (cdr lst)))])) ; otherwise, count the current item and recur with the rest of the list

(define (deepListLength lst)
  (cond
    [(null? lst) 0] ; if the list is empty, return 0
    [(not (list? (car lst))) (+ 1 (deepListLength (cdr lst)))] ; if the first element is not a list, count it and recur with the rest of the list
    [else (+ (deepListLength (car lst)) (deepListLength (cdr lst)))])) ; if the first element is a list, recur with both the first element and the rest of the list

(display "List Length of List 1: ")
(display (listLength '(1 2 3 4))) ; Output: 4
(newline)
(display "List Length of List 2: ")
(display (listLength '(1 (2 3) (4 (5 6))))) ; Output: 3
(newline)


(display "Deep List Length of List 3: ")
(display (deepListLength '(1 2 3 4))) ; Output: 4
(newline)
(display "Deep List Length of List 4: ")
(display (deepListLength '(1 (2 3) (4 (5 6))))) ; Output: 6
(newline)