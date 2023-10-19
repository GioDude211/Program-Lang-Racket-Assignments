; Programmer: Giovanni Vecchione
; Date: 10/10/23
; Subject: Asg 08
;Write a recursive function that given a number and a sorted binary tree, returns a sorted tree with the number insertd.
;Write a recursive function that given a list of numbers and a tree, returns a tree with all of the numbers inserted.
;Write a recursive function that given a sorted tree, returns a list of numers in ascending order.
;Details in the slides.

#lang racket

; Check if the tree is empty
(define (empty-tree? tree)
  (null? tree))

; Inserts a value into the tree
(define (tree-insert val tree)
  (cond
    [(empty-tree? tree) (list val)] ; Return the value as a list if tree is empty
    
    [(= (length tree) 1) ; If tree is a single value
     (if (< val (car tree))
         (list (car tree) (list val)) ; Insert to the left
         (list (car tree) '() (list val)))] ; Insert to the right

    [(= (length tree) 2) ; If tree has left child but no right child
     (list (car tree) (cadr tree) (list val))] ; Insert to the right

    [else ; If tree has both left and right child
     (list (car tree) (cadr tree) (tree-insert val (caddr tree)))] ; Insert to the right subtree
  )
)

; Test the function
(display (tree-insert 8 '())) ; should display '(8)
(newline)
(display (tree-insert 12 '(8))) ; should display '(8 () (12))
(newline)
(display (tree-insert 3 '(8))) ; should display '(8 (3))
(newline)
(display (tree-insert 12 '(8 (3)))) ; should display '(8 (3) (12))
(newline)
(display (tree-insert 4 '(8 (3) (12)))) ; should display '(8 (3 (4)) (12))
(newline)

;Issue: Works when empty but not when there is a value in place

