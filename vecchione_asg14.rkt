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
(define my-pen (make-pen #:color "white" #:width 1))
(define my-brush (make-brush #:color "purple"))

(send my-dc set-pen my-pen)
(send my-dc set-brush my-brush)

;background color
(send my-dc draw-rectangle 0 0 imageWidth imageHeight)

(define myPolygon (new dc-path%)) ; create polygon
(send myPolygon move-to 0 0) ; input points (works like x-axis and y-axis)
(send myPolygon line-to 100 0)
(send myPolygon line-to 100 50)
(send myPolygon line-to 0 50)
(send myPolygon close)

;Up until this point code is good ----------^

;drawToScreen Function
(define (drawToScreen polygon)
  ; Draw the translated polygon on the screen
  (send my-dc draw-path polygon)
)

; DUPLICATE POLYGON FUNCTION
(define (duplicatePolygon inputPolygon)
  (let ((newPolygon (new dc-path%))) ; create a new polygon
    ; Add the same points to the new polygon
    (send newPolygon move-to 0 0) ; input points (works like x-axis and y-axis)
    (send newPolygon line-to 100 0)
    (send newPolygon line-to 100 50)
    (send newPolygon line-to 0 50)
    (send newPolygon close)
    newPolygon)) ; return the new polygon



; Initialize a variable to keep track of the number of polygons drawn
; Do-loop in racket works like a for-loop,  (do ([i 0 (+ i 1)])

(define (create-fractal-image depth rotateAmount inputPolygon baseX baseY)

  (when (> depth 0) ;; loop on polygons. move it a little, then draw it.

    ;Transform current polygon
    (send inputPolygon scale 0.7 0.7)           ; polygon scale
    (send inputPolygon rotate rotateAmount)     ; polygon rotate in radians
    (send inputPolygon translate baseX baseY)   ; Translate the polygon upwards, and slightly to the side for branches

    ; draw polygon
    (drawToScreen inputPolygon)                 
    (set! numPoly (+ 1 numPoly))

    ; Calculate new positions for left and right branches
    (define newBaseX (+ baseX 100)) ; Adjust as needed for the branch separation
    (define newBaseY (+ baseY 50)) ; Adjust as needed for the branch height
    
    ; Recursive call for "left" branch
    (create-fractal-image (- depth 1) (- rotateAmount) (duplicatePolygon inputPolygon) newBaseX newBaseY)
    ; Recursive call for "right" branch
    (create-fractal-image (- depth 1) rotateAmount (duplicatePolygon inputPolygon) newBaseX newBaseY)))


; Start position and parameters for the fractal
(define rotateAmount (- (/ pi 6)))
(define depth 16) ; depth of recursion
; Adjust the initial X and Y translation amounts for the starting branch
(define initial-baseX (/ imageWidth 2))
(define initial-baseY imageHeight) ; Start from the bottom of the screen


(create-fractal-image  depth rotateAmount myPolygon initial-baseX initial-baseY) ; CALL CREATE_FRACTAL_IMAGE FUNCTION

; Create a frame (window)
(define frame (new frame% [label "Fractal Drawing"]
                          [width imageWidth]
                          [height imageHeight]))

; Create a canvas that we will draw on, which is inside the frame
(define canvas (new canvas% [parent frame]
                           [paint-callback
                            (lambda (canvas my-dc)
                              (send my-dc draw-bitmap my-bitmap 0 0))]))

; Show the frame (this actually displays the window)
(send frame show #t)

(display "Number of polygons drawn: ")
(display numPoly)
(newline)