; Programmer: Giovanni Vecchione
; Date: 10/10/23
; Subject: Asg 08
;1 - Write a recursive function that given a number and a sorted binary tree, returns a sorted tree with the number insertd.
;2 - Write a recursive function that given a list of numbers and a tree, returns a tree with all of the numbers inserted.
;3 - Write a recursive function that given a sorted tree, returns a list of numers in ascending order.
;Details in the slides.

#lang racket

; FUNCTION 1
(define (tree-insert val tree)
  (cond
    [(null? tree) (list val)] ; Return the value as a list if tree is empty
    
    [(equal? (cdr tree) '()) ; if only a root node, create subnodes
      (cond
        [(<= val (car tree)) (list (car tree) (list val) '())]  ;#1 If value is less than or equal to, go left 
        [else (list (car tree) '() (list val))])]               ;#1 else go right
      
    [(<= val (car tree))                               ;#2 If the number is less than or equal to the root, recurse on the left subtree
      (list (car tree) (tree-insert val (cadr tree))
        (if (pair? (cddr tree)) (caddr tree) '()))]

      [else (list (car tree)                          ;#2 else recurse on the right
                (if (pair? (cdr tree))
                    (cadr tree) '())
                (tree-insert val (if (pair? (cddr tree)) (caddr tree) '())))]

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

