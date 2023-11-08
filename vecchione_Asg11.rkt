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
(define my-pen (make-pen #:color "blue" #:width 1))
(define my-brush (make-brush #:color "yellow"))

(send my-dc set-pen my-pen)
(send my-dc set-brush my-brush)

;Up until this point code is good ----------^

; Draw a filled polygon on my-dc
(define points (list (cons 100 100)  ; Point 1 (x1, y1)
                     (cons 300 100)  ; Point 2 (x2, y2)
                     (cons 200 300)  ; Point 3 (x3, y3)
                     (cons 100 100))) ; Closing the polygon back to the first point
(send my-dc draw-polygon points)

;Draw a line (rectangular polygon or something)
;count every iteration
;scale it, rotate it, translate it
;loop it back


; Create a frame (window)
;Correction - Use Draw to Screen instead makes it easier when shrinking image etc.
;

(define frame (new frame% [label "Polygon Drawing"]
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


my-bitmap