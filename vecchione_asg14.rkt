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
(define baseLengthX 50)
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
  (let ([xTrans 200]
        [yTrans 222]
        [xScale .8]
        [yScale .8])
    ; Convert the polygon to screen coordinates
    (send myPolygon scale xScale yScale)
    (send myPolygon translate xTrans yTrans)
    
    ; Draw the polygon in screen coordinates
    (send dc draw-path myPolygon)
    
    ; Convert the polygon back to world coordinates
    (send myPolygon translate (- xTrans) (- yTrans))
    (send myPolygon scale (/ 1.0 xScale) (/ 1.0 yScale))))


; DUPLICATE POLYGON FUNCTION
(define (duplicatePolygon inPoly x2 y2 scaledDistanceX scaledDistanceY)
  (let ((newPolygon (new dc-path%))) ; create a new polygon
    
    ; Add the same points to the new polygon
    (send newPolygon move-to x2 y2) ; input points (works like x-axis and y-axis)
    (send newPolygon line-to (- x2 scaledDistanceX) y2)
    (send newPolygon line-to (- x2 scaledDistanceX) (- y2 scaledDistanceY))
    (send newPolygon line-to x2 (- y2 scaledDistanceY))
    (send newPolygon close)
    newPolygon)) ; return the new polygon

;Function creates the image (MAIN)
(define (create-fractal-image depth rotateAmount inputPolygon x1 y1 scaleFactor baseLengthX baseLengthY)

  (when (> depth 0) ;; loop on polygons. move it a little, then draw it.
    
    ;Calculate the new points for the branches
    ; Scale the translation distances by the current scaleFactor
    (define scaledDistanceX (* baseLengthX scaleFactor))
    (define scaledDistanceY (* baseLengthY scaleFactor))
    
    ;Take into account the rotation and translation
    (define x2 (+ x1 (* scaledDistanceX (cos rotateAmount))))
    (define y2 (- y1 (* scaledDistanceY (sin rotateAmount))))
    
    ; draw polygon
    (drawToScreen my-dc inputPolygon)                 
    (set! numPoly (+ 1 numPoly))
    
    ;Do-Loop determines how many new branches
    (do ([i 0 (+ i 1)])
        ((= i 3))
      
      ;adjust factors for length
      (let* ([scaleFactor (cond [(= i 0) 0.9]    
                                  [(= i 1) 0.3]   
                                  [(= i 2) 0.6])]  
             ;rotational change (angle)
              [rotateFactor (cond [(= i 0) -.9]     
                                  [(= i 1) 1.6]     
                                  [(= i 2) 0.55])])
        
    ; Recursive call for "left" branch"
    (create-fractal-image (- depth 1) (- rotateAmount(* rotateFactor (/ pi 6))) (duplicatePolygon inputPolygon x2 y2 scaledDistanceX scaledDistanceY) x2 y2 scaleFactor scaledDistanceX scaledDistanceY)
    ; Recursive call for "right" branch"
    (create-fractal-image (- depth 1) (+ rotateAmount (* rotateFactor (/ pi 6)))  (duplicatePolygon inputPolygon x2 y2 scaledDistanceX scaledDistanceY) x2 y2 scaleFactor scaledDistanceX scaledDistanceY)))))




(create-fractal-image depth rotateAmount myPolygon  startX1 startY1 scaleFactor baseLengthX baseLengthY) ; CALL CREATE_FRACTAL_IMAGE FUNCTION

(display "Number of polygons drawn: ")
(display numPoly)
(newline)

my-bitmap

;NOTES:The image output is spiral in nature for some reason. The drawToScreen function appears to be not affecting this, furthermore I added a way to correctly
;track the transformations to keep track of the new x and y coordinates. For some reason I believe this is where the issue resides. I don't think its because of
;the duplicate poly function

;ISSUE 2: Theres a problem with the rotation of the branches

;IT IS CLOSE BUT IT IS NOT TRANSLATING TO THE CORRECT LOCATIONS