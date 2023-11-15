#lang racket/gui

(require racket/draw)
(require colors)

(define imageWidth 2048)
(define imageHeight 1152)

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

(send my-dc draw-rectangle 0 0 imageHeight imageWidth)
;Up until this point code is good ----------^

; variable to keep track of the polygons drawn
(define numPoly 0)

; Initialize a variable to keep track of the number of polygons drawn
; Do-loop in racket works like a for-loop,  (do ([i 0 (+ i 1)])

(define (draw-fractal-polygon x1 y1 length angle depth)
  (when (> depth 0)
    (let* ((trunk-end-x x1)                         ; Trunk goes straight up, so x doesn't change
           (trunk-end-y (- y1 length))              ; Subtract from y to go upwards
           (branch-length (* length 0.75))          ; Shrink size for each recursive call
           (right-x (+ trunk-end-x branch-length))  ; Right branch end x coordinate
           (left-x (- trunk-end-x branch-length))   ; Left branch end x coordinate
           (branch-end-y (- trunk-end-y branch-length)) ; Both branches go up
           )
      (send my-dc set-pen my-pen)
      ; Draw Trunk
      (send my-dc draw-line x1 y1 trunk-end-x trunk-end-y)
      ; Draw right branch
      (send my-dc draw-line trunk-end-x trunk-end-y right-x branch-end-y)
      ; Draw left branch
      (send my-dc draw-line trunk-end-x trunk-end-y left-x branch-end-y)
      
      ; Recursive call for right and left branches
      (draw-fractal-polygon right-x branch-end-y branch-length angle (- depth 1))
      (draw-fractal-polygon left-x branch-end-y branch-length angle (- depth 1))
      )))


; Start position and parameters for the fractal
(define start-x (/ imageWidth 2)) ; start from center
(define start-y (/ imageHeight 2)) ; start from bottom
(define initial-size 100) ; length of trunk
(define initial-angle 45) ; start angle pointing up
(define depth 10) ; depth of recursion

(draw-fractal-polygon start-x start-y initial-size initial-angle depth)

; Create a frame (window)
(define frame (new frame% [label "Fractal Drawing"]
                          [width imageWidth]
                          [height imageHeight]
                          [alignment '(center center)]))

; Create a canvas that we will draw on, which is inside the frame
(define canvas (new canvas% [parent frame]
                           [paint-callback
                            (lambda (canvas dc)
                              (send dc draw-bitmap my-bitmap 0 0))]))

; Show the frame (this actually displays the window)
(send frame show #t)
