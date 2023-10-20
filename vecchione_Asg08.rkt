; Programmer: Giovanni Vecchione
; Date: 10/10/23
; Subject: Asg 08
;1 - Write a recursive function that given a number and a sorted binary tree, returns a sorted tree with the number insertd.
;2 - Write a recursive function that given a list of numbers and a tree, returns a tree with all of the numbers inserted.
;3 - Write a recursive function that given a sorted tree, returns a list of numers in ascending order.
;Details in the slides.

#lang racket

; FUNCTION 1 - DONE
;Side Note - Had some severe issues keeping track of the syntax, however after learning a better visual
;I was able to format it similar to functions I've worked with in other languages.

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

; FUNCTION 2 - DONE
;SideNote - Not exact to the output however, output matches list positioning
; Expected output: '(22 ((7 ((7 ((4 ((3) (5))) ())) (16 ((8 ((8) ())) (17))))) (25 (() (34 ((32) (67))))))
(define (list-to-tree lst1 tree2)
    (if (null? lst1) tree2
      (list-to-tree (cdr lst1) (tree-insert (car lst1) tree2)))
)


;FUNCTION 3 - Done
(define (tree-to-list tree3)
  (cond
    [(null? tree3) '()] ; checks if empty, otherwise returns empty tree

    [(equal? (cdr tree3) '()) (list (car tree3))];if only root, return first element

    [else (append (tree-to-list (cadr tree3)) (list (car tree3)) (tree-to-list (caddr tree3)))]
    ; ^ else add the tree-to-list of cadr tree to the first node, also add to the tree-to-list of caddr of tree
  )
)



; Testing Function 1
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

; Testing Function 2
(displayln (list-to-tree '(22 25 7 16 8 34 67 7 32 17 8 4 5 3) '())) 

; Testing Function 3
(tree-to-list (list-to-tree '(22 25 7 16 8 34 67 7 32 17 8 4 5 3) '()))