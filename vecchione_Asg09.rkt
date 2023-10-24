;Programmer: Giovanni Vecchione
;Date: 10/22/23
;Subject; Asg 09
;Write a function that translates, scales, rotates, and draws polygons.
;See 'COSC 3301 Coding 9.pptx' for details.


;ISSUES change the colors and rates, in this case it seems that the rate of change is to stark...maybe
;

#lang racket
(require racket/draw)
(require colors)

(define imageWidth 512)
(define imageHeight 288)

(define myTarget (make-bitmap imageWidth imageHeight))
(define dc (new bitmap-dc% [bitmap myTarget]))

;make background rectangle
(send dc set-pen "green" 2 'solid)
(send dc set-brush "purple" 'solid)

(send dc draw-rectangle
      0 0
      512 288)
;make polygons
(send dc set-pen "red" 2 'solid)
(send dc set-brush "yellow" 'solid)
(define myPolygon (new dc-path%)) ; create polygon
(send myPolygon move-to 0 0) ; input points
(send myPolygon line-to 7 13)
(send myPolygon line-to 9 13)
(send myPolygon line-to 13 3)
(send myPolygon close)
(send dc draw-path myPolygon) ; draw polygon
(send myPolygon translate 20 60) ; polygon translate
(send dc draw-path myPolygon) ; draw polygon
(send myPolygon scale 3 2) ; polygon scale
(send dc draw-path myPolygon) ; draw polygon
(send myPolygon translate 20 40) ; polygon translate  
;color wheel function, takes shape as parameter and rotates it and changes its color on the color wheel
(define (colorWheel shape)
  (for ([i 90])
    (define myHsv (hsv (+ 0.0 (* i 0.001)) 1.0 1.0)); hue, saturation, value
    (send dc set-pen (hsv->color myHsv) 2 'solid)
    (send dc set-brush (hsv->color myHsv) 'solid)
    (send shape rotate 400)
    (send dc draw-path shape)))

(colorWheel myPolygon)
myTarget

;USE DrRacket for this, image pulls up better. In this case the 