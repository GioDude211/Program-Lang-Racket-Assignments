#lang racket/gui

(require racket/draw)
(require colors)

(define imageWidth 2048)
(define imageHeight 1152)
(define numPoly 0)        ; variable to keep track of the polygons drawn

(define radius (/ (min imageWidth imageHeight) 2))

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

;Up until this point code is good ----------^
; Do-loop in racket works like a for-loop,  (do ([i 0 (+ i 1)])

; Start position and parameters for the fractal
; Adjust the initial X and Y translation amounts for the starting branch
(define rotateAmount (- (/ pi 2)))
(define depth 16) ; depth of recursion
(define initial-baseX (/ imageWidth 2))
(define initial-baseY (/ imageHeight 2)) ; Start from the bottom of the screen


;Function creates the image (MAIN)
(define (create-fractal-image depth rotateAmount inputPolygon x1 y1)

  (when (> depth 0) ;; loop on polygons. move it a little, then draw it.

    ;Transform current polygon
    (send inputPolygon scale 0.9 0.9)           ; polygon scale
    (send inputPolygon rotate rotateAmount)     ; polygon rotate in radians
    

    ; draw polygon
    (drawToScreen inputPolygon x1 y1)                 
    (set! numPoly (+ 1 numPoly))

    (define x2 (+ x1 (* .2 (cos rotateAmount))))
    (define y2 (+ y1 (* .2 (sin rotateAmount))))
    
    ; Recursive call for "left" branch
    (create-fractal-image (- depth 1) (- rotateAmount) inputPolygon x2 y2)
    ; Recursive call for "right" branch
    (create-fractal-image (- depth 1) rotateAmount inputPolygon x2 y2)))


;drawToScreen Function
(define (drawToScreen polygon x1 y1)
  ; Set the brush and pen for the drawing context
  (send my-dc set-pen "white" 2 'solid)
  (send my-dc set-brush "purple" 'solid)
  (send myPolygon translate x1 y1) ;  translate
  ; Test screen-to-world conversion
  (send my-dc draw-path polygon)
)


(create-fractal-image  depth rotateAmount myPolygon initial-baseX initial-baseY) ; CALL CREATE_FRACTAL_IMAGE FUNCTION

(display "Number of polygons drawn: ")
(display numPoly)
(newline)

my-bitmap