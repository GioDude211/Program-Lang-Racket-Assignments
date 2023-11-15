#lang racket/gui

(require racket/draw)
(require colors)

(define imageWidth 2048)
(define imageHeight 1152)
(define numPoly 0)        ; variable to keep track of the polygons drawn

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
(send myPolygon move-to 0 0) ; input points (works like x-axis and y-axis)
(send myPolygon line-to 50 0)
(send myPolygon line-to 50 100)
(send myPolygon line-to 0 100)
(send myPolygon close)

;In this case create the trunk of the tree sepaate from the branches (create-fractal-image function)
;define create-trunk-fractal-image



;Up until this point code is good ----------^
; Do-loop in racket works like a for-loop,  (do ([i 0 (+ i 1)])

; Start position and parameters for the fractal
; Adjust the initial X and Y translation amounts for the starting branch
(define rotateAmount (- (/ pi 2)))
(define depth 16) ; depth of recursion
(define initial-baseX (/ imageWidth 2))
(define initial-baseY imageHeight) ; Start from the bottom of the screen

;drawToScreen Function
(define (drawToScreen inputPolygon x2 y2)
  ; Set the brush and pen for the drawing context
  (send my-dc set-pen "white" 2 'solid)
  (send my-dc set-brush "purple" 'solid)
  (send inputPolygon translate x2 y2) ;  translate
  ; Test screen-to-world conversion
  (send my-dc draw-path inputPolygon)
)


; DUPLICATE POLYGON FUNCTION
(define (duplicatePolygon inputPolygon)
  (let ((newPolygon (new dc-path%))) ; create a new polygon
    ; Add the same points to the new polygon
    (send newPolygon move-to 0 0) ; input points (works like x-axis and y-axis)
    (send newPolygon line-to 50 0)
    (send newPolygon line-to 50 100)
    (send newPolygon line-to 0 100)
    (send newPolygon close)
    newPolygon)) ; return the new polygon

;Function creates the image (MAIN), change this so it is primarily for branches instead
(define (create-fractal-image depth rotateAmount inputPolygon x1 y1)

  (when (> depth 0) ;; loop on polygons. move it a little, then draw it.

    ;Transform current polygon
    (send inputPolygon scale 0.9 0.9)           ; polygon scale
    (send inputPolygon rotate rotateAmount)     ; polygon rotate in radians

    ;Calculate the new x and y (x2, y2)
    (define x2 (+ x1 (* 100 (cos rotateAmount))))
    (define y2 (+ y1 (* 50 (sin rotateAmount))))

    ; draw polygon
    (drawToScreen inputPolygon x2 y2)                 
    (set! numPoly (+ 1 numPoly))

    ; Recursive call for "left" branch
    (create-fractal-image (- depth 1) (- rotateAmount) (duplicatePolygon inputPolygon) x2 y2)
    ; Recursive call for "right" branch
    (create-fractal-image (- depth 1) rotateAmount inputPolygon x2 y2)))




(create-fractal-image  depth rotateAmount myPolygon initial-baseX initial-baseY) ; CALL CREATE_FRACTAL_IMAGE FUNCTION

(display "Number of polygons drawn: ")
(display numPoly)
(newline)

my-bitmap

;NOTES: 1. Separate the create-fractal-image function into two, one just for the trunk / other for teh branches
;2. Ensure polygon duplication is not affecting image, use point system/using temp polygons
;3. Possible need to convert the world metrics to screen metrics. 
;However since everything is assumed is already in pixel format this may not be needed.