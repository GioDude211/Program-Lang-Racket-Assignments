#lang racket/gui

(require racket/draw)
(require colors)

(define imageWidth 2048)
(define imageHeight 1152)

; Create a new bitmap of size 2048 x 1152
(define my-bitmap (make-bitmap imageWidth imageHeight))
; Create a drawing context for the bitmap
(define my-dc (new bitmap-dc% [bitmap my-bitmap]))

; Set the brush and pen for the drawing context
(define my-pen (make-pen #:color "white" #:width 1))
(define my-brush (make-brush #:color "purple"))

(send my-dc set-pen my-pen)
(send my-dc set-brush my-brush)

; Draw a background rectangle
(send my-dc draw-rectangle 0 0 imageWidth imageHeight)

; Function to draw the fractal
(define (draw-fractal-polygon x1 y1 length angle depth)
  (when (> depth 0)
    (let* ((new-length (* length 0.9))  ; Shrink size
           (new-angle (+ angle 15))     ; Rotate
           (x2 (+ x1 (* new-length (cos angle)))) ; Calculate new x
           (y2 (- y1 (* new-length (sin angle))))) ; Calculate new y
      (send my-dc set-pen my-pen)
      (send my-dc draw-line x1 y1 x2 y2) ; Draw current line
      ; Recursive call
      (draw-fractal-polygon x2 y2 new-length new-angle (- depth 1)))))

; Start position and parameters for the fractal
(define start-x (/ imageWidth 4))
(define start-y (/ imageHeight 2))
(define initial-size 100)  ; Adjust the size as needed
(define initial-angle 0)
(define depth 10)  ; Adjust the depth as needed

; Call the function to draw the fractal
(draw-fractal-polygon start-x start-y initial-size initial-angle depth)

; Create a frame (window) to display the fractal
(define frame (new frame% [label "Fractal Drawing"]
                          [width imageWidth]
                          [height imageHeight]))

; Create a canvas inside the frame
(define canvas (new canvas% [parent frame]
                           [paint-callback
                            (lambda (canvas dc)
                              (send dc draw-bitmap my-bitmap 0 0))]))

; Show the frame
(send frame show #t)