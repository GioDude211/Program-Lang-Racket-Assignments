#lang racket/gui

(require racket/draw)
(require colors)

;Set up dimensions for image
(define imageWidth 2048)
(define imageHeight 1152)

(define numPoly 0)                ; variable to keep track of the polygons drawn
(define startX1 (/ imageWidth 2)) ;Start point for X
(define startY1 (/ imageHeight 2));Start point for Y

;establish the dimensions of the polygons
(define baseLengthX 100)
(define baseLengthY 100)

; Create a new bitmap of size 2048 x 1152
(define my-bitmap (make-bitmap imageWidth imageHeight))
; Create a drawing context for the bitmap
(define my-dc (new bitmap-dc% [bitmap my-bitmap]))

; Set the brush and pen for the drawing context
(send my-dc set-pen "white" 2 'solid)
(send my-dc set-brush "purple" 'solid)

;background color
(send my-dc draw-rectangle 0 0 imageWidth imageHeight)

; create starting polygon
(define myPolygon (new dc-path%)) 
(send myPolygon move-to startX1 startY1) ; input points (works like x-axis and y-axis)
(send myPolygon line-to (- startX1 baseLengthX) startY1)
(send myPolygon line-to (- startX1 baseLengthX) (- startY1 baseLengthY))
(send myPolygon line-to startX1 (- startY1 baseLengthY))
(send myPolygon close)

;Up until this point code is good ----------^
; Do-loop in racket works like a for-loop,  (do ([i 0 (+ i 1)])

; Start position and parameters for the fractal
; Adjust the initial X2 and Y2 translation amounts for the starting branch
(define rotateAmount (/ pi 2))
(define depth 7) ; depth of recursion
(define scaleFactor 1)

;drawToScreen FUNCTION -NOTHING WRONG HERE
(define (drawToScreen dc myPolygon)
  (let ([xTrans 979.2]
        [yTrans 570.8]
        [xScale 0.4]
        [yScale 0.4])
    ; Convert the polygon to screen coordinates
    (send myPolygon scale xScale yScale)
    (send myPolygon translate xTrans yTrans)
    
    ; Draw the polygon in screen coordinates
    (send dc draw-path myPolygon)
    
    ; Convert the polygon back to world coordinates
    (send myPolygon translate (- xTrans) (- yTrans))
    (send myPolygon scale (/ 1.0 xScale) (/ 1.0 yScale))))



; DUPLICATE POLYGON FUNCTION
(define (duplicatePolygon inPoly)
  (let ((newPolygon (new dc-path%))) ; create a new polygon
    
    ; Add the same points to the new polygon
    (send newPolygon move-to startX1 startY1) ; input points (works like x-axis and y-axis)
    (send newPolygon line-to (+ startX1 baseLengthX) startY1)
    (send newPolygon line-to (+ startX1 baseLengthX) (- startY1 baseLengthY))
    (send newPolygon line-to startX1 (- startY1 baseLengthY))
    (send newPolygon close)
    newPolygon)) ; return the new polygon

;Function creates the image (MAIN), change this so it is primarily for branches instead - POSSIBLE ISSUE with overall rotation and x,y coordinates
(define (create-fractal-image depth rotateAmount inputPolygon x1 y1 scaleFactor baseLengthX baseLengthY)

  (when (> depth 0) ;; loop on polygons. move it a little, then draw it.
    ;Calculate the new points for the branches
    ; Scale the translation distances by the current scaleFactor
    (define scaledDistanceX (* baseLengthX scaleFactor))
    (define scaledDistanceY (* baseLengthY scaleFactor))
    
    ;Take into account the rotation and translation
    (define x2 (+ x1 (* scaledDistanceX (cos rotateAmount))))
    (define y2 (- y1 (* scaledDistanceY (sin rotateAmount))))
    
    (define myPolygon (new dc-path%)) ; create polygon
    (send myPolygon move-to x1 y1) ; input points (works like x-axis and y-axis)
    (send myPolygon line-to (- x1 baseLengthX) y1)
    (send myPolygon line-to (- x1 baseLengthX) (- y1 baseLengthY))
    (send myPolygon line-to x1 (- y1 baseLengthY))
    (send myPolygon close)
    
    ; draw polygon
    (drawToScreen my-dc inputPolygon)                 
    (set! numPoly (+ 1 numPoly))

    ;Transform current polygon - REMOVING THIS CAUSES A TREE LIKE STRUCTURE TO APPEAR - ISSUE 1
    ;Check and see what happens if you remove the translate
    (send inputPolygon scale 0.9 0.9)           ; polygon scale
    (send inputPolygon rotate rotateAmount)     ; polygon rotate in radians
    (send inputPolygon translate x1 y1)         ; translates polygon to new location


    ;Do-Loop determines how many new branches
    (do ([i 0 ( + i 1)])
      ((>= i 3)) ;this changes number of branching
    ; Recursive call for "left" branch"
    (create-fractal-image (- depth 1) (- rotateAmount (/ pi 12)) (duplicatePolygon inputPolygon) x2 y2 (* scaleFactor 0.9) scaledDistanceX scaledDistanceY)
    ; Recursive call for "right" branch"
    (create-fractal-image (- depth 1) (+ rotateAmount (/ pi 12)) inputPolygon x2 y2 (* scaleFactor 0.9) scaledDistanceX scaledDistanceY) ) ))




(create-fractal-image depth rotateAmount myPolygon  startX1 startY1 scaleFactor baseLengthX baseLengthY) ; CALL CREATE_FRACTAL_IMAGE FUNCTION

(display "Number of polygons drawn: ")
(display numPoly)
(newline)

my-bitmap

;NOTES:The image output is spiral in nature for some reason. The drawToScreen function appears to be not affecting this, furthermore I added a way to correctly
;track the transformations to keep track of the new x and y coordinates. For some reason I believe this is where the issue resides. I don't think its because of
;the duplicate poly function

;ISSUE 1 IDEA: It seems that removing the transformations in the main function left a tree like strucutre, could it be because theres a double transformation occuring
;in the duplicate poly function????

;IT IS CLOSE BUT IT IS NOT TRANSLATING TO THE CORRECT LOCATIONS