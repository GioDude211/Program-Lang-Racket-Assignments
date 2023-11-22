#lang racket/gui

(require racket/draw)
(require colors)

;Set up dimensions for image
(define imageWidth 2048)
(define imageHeight 1152)

(define numPoly 0)                ; variable to keep track of the polygons drawn
(define startX1 (/ imageWidth 2)) ;Start point for X
(define startY1 imageHeight);Start point for Y

;establish the dimensions of the polygons
(define baseLength 50)

; Create a new bitmap of size 2048 x 1152
(define my-bitmap (make-bitmap imageWidth imageHeight))
; Create a drawing context for the bitmap
(define my-dc (new bitmap-dc% [bitmap my-bitmap]))

; Set the brush and pen for the drawing context
(send my-dc set-pen "white" 2 'solid)
(send my-dc set-brush "gray" 'solid)

;background color
(send my-dc draw-rectangle 0 0 imageWidth imageHeight)

;Up until this point code is good ----------^
; Do-loop in racket works like a for-loop,  (do ([i 0 (+ i 1)])

; Start position and parameters for the fractal
; Adjust the initial X2 and Y2 translation amounts for the starting branch
(define rotateAmount (/ pi 6))
(define depth 7) ; depth of recursion
(define scaleFactor 1)

#|
Was unable to use polygons in this assignment
;drawToScreen FUNCTION -NOTHING WRONG HERE
(define (drawToScreen dc myPolygon)
  (let ([xTrans 800]
        [yTrans 800]
        [xScale .1]
        [yScale .1])
    ; Convert the polygon to screen coordinates
    (send myPolygon scale xScale yScale)
    (send myPolygon translate xTrans yTrans)
    
    ; Draw the polygon in screen coordinates
    (send dc set-pen "black" 2 'solid) ; set the line color 
    (send dc set-brush (make-color 25 22 16) 'solid) ; set color for filling
    (send dc draw-path myPolygon)
    
    ; Convert the polygon back to world coordinates
    (send myPolygon translate (- xTrans) (- yTrans))
    (send myPolygon scale (/ 1.0 xScale) (/ 1.0 yScale))))
|#



;Function creates the image (MAIN)
(define (create-fractal-image depth rotateAmount x1 y1 scaleFactor)

  (when (> depth 0) ;; loop on polygons. move it a little, then draw it.
    
     ; Calculate new X and Y
    ;SCALE
    (define scaled_X (* x1 scaleFactor))
    (define scaled_Y (* y1 scaleFactor))
    ;ROTATE
    (define rx (scaled_X (- rotateAmount)))
    (define ry (scaled_Y (- rotateAmount)))
    ;TRANSLATE
    (define new_X (+ rx x1))
    (define new_Y (+ ry y1))
    
    (send my-dc draw-line x1 y1 new_X new_Y)

   
   

    ;Do-Loop determines how many new branches
    (do ([i 0 (+ i 1)])
        ((= i 2))
      
      ;adjust factors for length
      (let* ([scaleFactor (cond [(= i 0) .9]    
                                  [(= i 1) .8]   
                                  [(= i 2) .6])]
             
             ;rotational change (angle)
              [rotateFactor (cond [(= i 0) -1.3]     
                                  [(= i 1) .8]     
                                  [(= i 2) .3])])
        
    ; Recursive call for "left" branch"
    (create-fractal-image (- depth 1) (* rotateFactor (+ rotateAmount (/ pi 6)))  scaled_X scaled_Y  scaleFactor)
    ; Recursive call for "right" branch"
    (create-fractal-image (- depth 1) (* rotateFactor (- rotateAmount (/ pi 6)))  scaled_X scaled_Y  scaleFactor)))))




(create-fractal-image depth rotateAmount startX1 startY1 scaleFactor) ; CALL CREATE_FRACTAL_IMAGE FUNCTION

(display "Number of polygons drawn: ")
(display numPoly)
(newline)

my-bitmap

;NOTES:The image output is spiral in nature for some reason. The drawToScreen function appears to be not affecting this, furthermore I added a way to correctly
;track the transformations to keep track of the new x and y coordinates. For some reason I believe this is where the issue resides. I don't think its because of
;the duplicate poly function

;ISSUE 2: Okay i was able to figure out the form of the tree, next step is to work on the x and y coordinates still

;IT IS CLOSE BUT IT IS NOT TRANSLATING TO THE CORRECT LOCATIONS