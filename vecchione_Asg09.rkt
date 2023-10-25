;Programmer: Giovanni Vecchione
;Date: 10/22/23
;Subject; Asg 09
;Write a function that translates, scales, rotates, and draws polygons.
;See 'COSC 3301 Coding 9.pptx' for details.

#lang racket
(require racket/draw)
(require colors)

(define imageWidth 512)
(define imageHeight 288)
(define radius (/ (min imageWidth imageHeight) 2))


(define myTarget (make-bitmap imageWidth imageHeight))
(define dc (new bitmap-dc% [bitmap myTarget]))

;make background rectangle
(send dc set-pen "blue" 2 'solid)
(send dc set-brush "purple" 'solid)

(send dc draw-rectangle
      0 0
      512 288)

;make polygons
(send dc set-pen "red" 2 'solid)
(send dc set-brush "yellow" 'solid)

(define myPolygon (new dc-path%)) ; create polygon
(send myPolygon move-to 0 0) ; input points (works like x-axis and y-axis)
(send myPolygon line-to 20 0)
(send myPolygon line-to 15 20)
(send myPolygon line-to 5 20)
(send myPolygon close)

(send dc draw-path myPolygon) ; draw polygon
(send myPolygon translate 20 20) ;  translate
(send dc draw-path myPolygon) ; draw polygon
(send myPolygon scale 2 2) ;  scale
(send dc draw-path myPolygon) ; draw polygon
(send myPolygon translate 30 60) ;  translate


;colorwheel
(define (colorWheel shape)
  (let ([hue-increment (/ 1.0 90)]) ; Here, we'll divide 1 (360 degrees in the color wheel) by 90 steps
    (for ([i (in-range 90)])
      (define myHsv (hsv (* hue-increment i) 1.0 1.0)) ; hue, saturation, value
      (send dc set-pen (hsv->color myHsv) 2 'solid)
      (send dc set-brush (hsv->color myHsv) 'solid)
      (send shape rotate 400) ; If we're doing 90 iterations, 360/90 = 4 degrees per iteration
      (send dc draw-path shape))))


(colorWheel myPolygon)

myTarget

