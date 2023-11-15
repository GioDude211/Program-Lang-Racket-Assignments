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



(define (create-fractal-image depth rotateAmount inputPolygon baseX baseY scaleFactor)
  (when (> depth 0)
    (define new-scale-factor (* scaleFactor 0.75)) ; Scale down for the next level of branches
    (define branch-angle (/ pi 6)) ; Adjust this angle for the divergence of branches

    ; Calculate new position for the end of the current branch
    (define end-x (+ baseX (* scaleFactor 100))) ; Assuming 100 is the length of the branch
    (define end-y baseY)

    ; Draw the current branch
    (send inputPolygon translate baseX baseY)
    (send inputPolygon rotate rotateAmount)
    (send inputPolygon scale scaleFactor scaleFactor)
    (drawToScreen inputPolygon)
    (set! numPoly (+ 1 numPoly))

    ; Recursive call for the left branch
    (create-fractal-image (- depth 1) (- rotateAmount branch-angle) (duplicatePolygon inputPolygon) end-x end-y new-scale-factor)

    ; Reset transformations for the next branch
    (send inputPolygon scale (/ 1 scaleFactor) (/ 1 scaleFactor))
    (send inputPolygon rotate (- rotateAmount))
    (send inputPolygon translate (- baseX) (- baseY))

    ; Recursive call for the right branch
    (create-fractal-image (- depth 1) (+ rotateAmount branch-angle) (duplicatePolygon inputPolygon) end-x end-y new-scale-factor)
  )
)

(define (calculate-end-point x y scale-factor angle)
  ; Here you will calculate the new end points after scaling and rotation
  ; You need to adjust this logic to match your specific branch end point calculation
  (values (+ x (* scale-factor (cos angle))) (+ y (* scale-factor (sin angle)))))

; Adjust initial starting position for the trunk
; Start position and parameters for the fractal
(define rotateAmount (- (/ pi 6)))
(define depth 16) ; depth of recursion
(define initial-end-x (/ imageWidth 2)) ; Center of the width
(define initial-end-y imageHeight) ; Bottom of the canvas
(define initial-scale-factor 1) ; Starting scale factor

; Start drawing the fractal tree
(create-fractal-image depth rotateAmount myPolygon initial-end-x initial-end-y initial-scale-factor)


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