#lang racket/gui

(require racket/draw)
(require colors)


(define imageWidth 2048)
(define imageHeight 1152)
; variable to keep track of the polygons drawn
(define numPoly 0)

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

(define (draw-branch my-dc x y length angle depth)
  (when (> depth 0)
    (define x2 (+ x (* length (cos angle))))
    (define y2 (+ y (* length (sin angle))))
    (send my-dc draw-line x y x2 y2)
    (draw-branch my-dc x2 y2 (* length 0.7) (+ angle (/ pi 4)) (- depth 1))
    (draw-branch my-dc x2 y2 (* length 0.7) (- angle (/ pi 4)) (- depth 1))))

(define (draw-tree)
  (define frame  (new frame% [label "Fractal Drawing"]
                          [width imageWidth]
                          [height imageHeight]))
                          
  (define canvas (new canvas% [parent frame]
                     [paint-callback
                      (λ (canvas my-dc)
                        (draw-branch my-dc 200 400 100 (- (/ pi 2)) 10))]))
  (send frame show #t))

(draw-tree)

(display "Number of polygons drawn: ")
(display numPoly)
(newline)