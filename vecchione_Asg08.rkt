; Programmer: Giovanni Vecchione
; Date: 10/10/23
; Subject: Asg 08
;Write a recursive function that given a number and a sorted binary tree, returns a sorted tree with the number insertd.
;Write a recursive function that given a list of numbers and a tree, returns a tree with all of the numbers inserted.
;Write a recursive function that given a sorted tree, returns a list of numers in ascending order.
;Details in the slides.

#lang racket

; Define a node structure
(struct node (val left right))

; Recursive function to insert a key into a BST
(define (insert root key)
  (cond
    [(null? root) 
     (node key #f #f)]
    [(< key (node-val root)) 
     (node (node-val root)
           (if (node-left root)
               (insert (node-left root) key)
               (node key #f #f))
           (node-right root))]
    [else
     (node (node-val root)
           (node-left root)
           (if (node-right root)
               (insert (node-right root) key)
               (node key #f #f)))]))

; Function to perform an inorder traversal of a BST
(define (inorder root)
  (if (not root)
      '()
      (append (inorder (node-left root)) (list (node-val root)) (inorder (node-right root)))))

; Function to print the BST using inorder traversal
(define (print-tree root)
  (display "Tree: ")
  (display (inorder root))
  (newline))

; Function to insert a key and print the BST before and after insertion
(define (insert-and-print-tree root key)
  (display "Before Inserting: ")
  (print-tree root)
  (let ([new-root (insert root key)])
    (display "After Inserting ")
    (display key)
    (display ": ")
    (print-tree new-root)
    new-root))

; Example usage:
; Construct a basic tree for testing
;       20
;      /  \
;    15    25
;   / \
;  10  18
(define root (node 20 (node 15 (node 10 #f #f) (node 18 #f #f)) (node 25 #f #f)))

; Insert a new node and print tree before and after insertion
(define new-root (insert-and-print-tree root 17))

