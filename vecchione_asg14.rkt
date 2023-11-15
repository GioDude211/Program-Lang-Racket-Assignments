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
(send myPolygon line-to 100 0)
(send myPolygon line-to 100 50)
(send myPolygon line-to 0 50)
(send myPolygon close)

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


;Up until this point code is good ----------^
; Do-loop in racket works like a for-loop,  (do ([i 0 (+ i 1)])

; Start position and parameters for the fractal
; Adjust the initial X and Y translation amounts for the starting branch
(define rotateAmount (- (/ pi 2)))
(define depth 16) ; depth of recursion
(define initial-baseX (/ imageWidth 2))
(define initial-baseY imageHeight) ; Start from the bottom of the screen

;Function creates the image (MAIN)
(define (create-fractal-image depth rotateAmount inputPolygon baseX baseY)

  (when (> depth 0) ;; loop on polygons. move it a little, then draw it.

    ;Transform current polygon
    (send inputPolygon scale 0.9 0.9)           ; polygon scale
    (send inputPolygon rotate rotateAmount)     ; polygon rotate in radians
    (define x2 (+ baseX (* 100 (cos rotateAmount))))
    (define y2 (+ baseY (* 100 (sin rotateAmount))))
    (send inputPolygon translate x2 y2)

    ; draw polygon
    (drawToScreen inputPolygon)                 
    (set! numPoly (+ 1 numPoly))

    

    

    ; Recursive call for "left" branch
    (create-fractal-image (- depth 1) (- rotateAmount) (duplicatePolygon inputPolygon) x2 y2)
    ; Recursive call for "right" branch
    (create-fractal-image (- depth 1) rotateAmount inputPolygon x2 y2)))

;drawToScreen Function

(define (drawToScreen polygon)
;  Define the screen center coordinates
  (define screenCenterX (/ imageWidth 2))
  (define screenCenterY (/ imageHeight 2))
  
  ; Get the bounding box of the polygon and unpack the values
  (define-values (left top right bottom) (send polygon get-bounding-box))
  
  ; Calculate the center of the polygon in world coordinates
  (define polygonCenterX (/ (+ left right) 1.2))
  (define polygonCenterY (/ (+ top bottom) 1.2))
  
  ; Calculate the translation needed to center the polygon on the screen
  (define translateX (- screenCenterX polygonCenterX))
  (define translateY (- screenCenterY polygonCenterY))
  
  ; Translate the polygon to screen coordinates
  (send polygon translate translateX translateY)
  ; Draw the translated polygon on the screen
  (send my-dc draw-path polygon)
)


(create-fractal-image  depth rotateAmount myPolygon initial-baseX initial-baseY) ; CALL CREATE_FRACTAL_IMAGE FUNCTION

; Create a frame (window)
(define frame (new frame% [label "Fractal Drawing"]
                          [width imageWidth]
                          [height imageHeight]
                          [alignment '(center center)]))

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