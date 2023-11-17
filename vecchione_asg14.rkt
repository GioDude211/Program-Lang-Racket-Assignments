#lang racket/gui

(require racket/draw)
(require colors)

(define imageWidth 2048)
(define imageHeight 1152)
(define numPoly 0)        ; variable to keep track of the polygons drawn
(define startX1 (/ imageWidth 2))
(define startY1 (/ imageHeight 2))

; Create a new bitmap of size 2048 x 1152
(define my-bitmap (make-bitmap imageWidth imageHeight))
; Create a drawing context for the bitmap
(define my-dc (new bitmap-dc% [bitmap my-bitmap]))

; Set the brush and pen for the drawing context
(send my-dc set-pen "white" 2 'solid)
(send my-dc set-brush "purple" 'solid)

;background color
(send my-dc draw-rectangle 0 0 imageWidth imageHeight)



(define myPolygon (new dc-path%)) ; create polygon
(send myPolygon move-to startX1 startY1) ; input points (works like x-axis and y-axis)
(send myPolygon line-to (- startX1 100) startY1)
(send myPolygon line-to (- startX1 100) (- startY1 50))
(send myPolygon line-to startX1 (- startY1 50))
(send myPolygon close)

;Up until this point code is good ----------^
; Do-loop in racket works like a for-loop,  (do ([i 0 (+ i 1)])

; Start position and parameters for the fractal
; Adjust the initial X2 and Y2 translation amounts for the starting branch
(define rotateAmount  (/ pi 2))
(define depth 16) ; depth of recursion
(define center-imageX (/ imageWidth 2))

;FIX THIS
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
    (send newPolygon line-to (+ startX1 100) startY1)
    (send newPolygon line-to (+ startX1 100) (- startY1 50))
    (send newPolygon line-to startX1 (- startY1 50))
    (send newPolygon close)
    newPolygon)) ; return the new polygon

;Function creates the image (MAIN), change this so it is primarily for branches instead
(define (create-fractal-image depth rotateAmount inputPolygon x1 y1)

  (when (> depth 0) ;; loop on polygons. move it a little, then draw it.

    ;Transform current polygon
    (send inputPolygon scale 0.9 0.9)           ; polygon scale
    (send inputPolygon rotate rotateAmount)     ; polygon rotate in radians
    (send inputPolygon translate x1 y1)

    ; draw polygon
    (drawToScreen my-dc inputPolygon)                 
    (set! numPoly (+ 1 numPoly))

    
    (define x2 (+ x1 (* 100 (cos rotateAmount))))
    (define y2 (- y1 (* 50 (sin rotateAmount))))
    
    ; Recursive call for "left" branch
    (create-fractal-image (- depth 1) (- rotateAmount) (duplicatePolygon inputPolygon) x2 y2)
    ; Recursive call for "right" branch
    (create-fractal-image (- depth 1) (+ rotateAmount) inputPolygon x2 y2)))




(create-fractal-image depth rotateAmount myPolygon  startX1 startY1) ; CALL CREATE_FRACTAL_IMAGE FUNCTION

(display "Number of polygons drawn: ")
(display numPoly)
(newline)

my-bitmap

;NOTES: 1. Separate the create-fractal-image function into two, one just for the trunk / other for teh branches
;2. Ensure polygon duplication is not affecting image, use point system/using temp polygons
;3. Possible need to convert the world metrics to screen metrics. 
;However since everything is assumed is already in pixel format this may not be needed.

;IT IS CLOSE BUT IT IS NOT TRANSLATING TO THE CORRECT LOCATIONS