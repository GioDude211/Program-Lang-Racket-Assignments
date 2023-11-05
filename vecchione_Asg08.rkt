; Programmer: Giovanni Vecchione
; Date: 10/10/23
; Subject: Asg 08
;1 - Write a recursive function that given a number and a sorted binary tree, returns a sorted tree with the number insertd.
;2 - Write a recursive function that given a list of numbers and a tree, returns a tree with all of the numbers inserted.
;3 - Write a recursive function that given a sorted tree, returns a list of numers in ascending order.
;Details in the slides.

#lang racket

; FUNCTION 1 - DONE
(define (tree-insert val tree)
  
  (cond

    ; if the tree is empty, return a new list with the number
    [(null? tree) (list val)]

    ; check if the number is greater than the root (car tree)
    [(> val (car tree))
     
     ; is the right subtree empty?
     (if (equal? (cdr tree) '())

         ; yes, right subtree is empty
         (list (car tree) (list '() (tree-insert val '())))

         ; checking if the left subtree of the right subtree is empty,  
         (if (equal? (cadr tree) '())

             ; if it is -
             (list (car tree) (list (cadr tree) (tree-insert val (caddr tree))))

             ; if not empty
             (list (car tree) (list (caadr tree) (tree-insert val (cadadr tree))))))]

    ; the number is less than the root
    [else

     ; check if root is only element in tree
     (if (equal? (cdr tree) '())

         ; if it is.. return list with root
         (list (car tree) (list (tree-insert val '()) '()))

         ; checks if root node contains subtrees
         (if (equal? (cadr tree) '())
             ; root node doesn't contain subtrees
             (list (car tree) (list (tree-insert val '()) (caddr tree)))

             ; root node does have subtrees
             (list (car tree) (list (tree-insert val (caadr tree)) (cadr (cadr tree))))))]))



; FUNCTION 2 - DONE
(define (list-to-tree lst1 tree2)
    (if (null? lst1) tree2
      (list-to-tree (cdr lst1) (tree-insert (car lst1) tree2)))
)



;Function 3 - DONE
(define (tree-to-list tree)
  (cond
    [(null? tree) '()] ; Tree is empty, return an empty list
    [(null? (cdr tree)) (list (car tree))] ; Tree is just a root node, return list with root value
    [else ; Tree has subtrees
     (append 
       (tree-to-list (caadr tree)) ; Process the left subtree
       (list (car tree)) ; Include the root
       (tree-to-list (cadadr tree)))])) ; Process the right subtree





; Testing Function 1
(display (tree-insert 8 '()))
(newline)
(display (tree-insert 12 '(8)))
(newline)
(display (tree-insert 3 '(8)))
(newline)
(display (tree-insert  12 '(8 ((3) ()))))
(newline)
(display (tree-insert 4 '(8 ((3) (12)))))
(newline)

; Testing Function 2
(displayln (list-to-tree '(22 25 7 16 8 34 67 7 32 17 8 4 5 3) '())) 

; Testing Function 3
(tree-to-list (list-to-tree '(22 25 7 16 8 34 67 7 32 17 8 4 5 3) '()))