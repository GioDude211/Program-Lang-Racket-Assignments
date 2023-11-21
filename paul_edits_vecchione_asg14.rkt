#lang racket

(require racket/draw)

(define imageWidth 800)
(define imageHeight 800)

(define myTarget (make-bitmap imageWidth imageHeight))
(define dc (new bitmap-dc% [bitmap myTarget]))

(define (makeTree depth current-rotate current-x-end current-y-end current-scale)
  (when (> depth 0)
    ; Duplicate original branch (line)
    (define duplicate-x-end (* current-x-end current-scale))
    (define duplicate-y-end (* current-y-end current-scale))
    (define rotated-dx (* (- duplicate-x-end 0) (cos current-rotate)))
    (define rotated-dy (* (- duplicate-y-end 0) (sin current-rotate)))
    (define translated-dx (+ rotated-dx current-x-end))
    (define translated-dy (+ rotated-dy current-y-end))
    
    ; Draw the duplicate branch
    (send dc draw-line current-x-end current-y-end translated-dx translated-dy)
    
    ; Calculate new x-end, y-end for the next iteration
    (define new-x-end (* duplicate-x-end (cos current-rotate)))
    (define new-y-end (* duplicate-y-end (sin current-rotate)))
    (define rotated-new-x-end (+ new-x-end current-x-end))
    (define rotated-new-y-end (+ new-y-end current-y-end))
    
    ; Calculate new scale and rotation angles
    (define new-scale (* current-scale 0.8))
    (define new-rotate1 (+ current-rotate (/ pi 6)))
    (define new-rotate2 (- current-rotate (/ pi 6)))
    
    ; Recursive calls for the two branches
    (makeTree (- depth 1) new-rotate1 rotated-new-x-end rotated-new-y-end new-scale)
    (makeTree (- depth 1) new-rotate2 rotated-new-x-end rotated-new-y-end new-scale)))

; Initialize the drawing with initial parameters
(makeTree 12 200 (/ imageWidth 2) imageHeight 0.2)

; Save the image
(send myTarget save-file "FractalTree.png" 'png)

; Display the image
(send dc get-bitmap)


