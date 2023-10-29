;Programmer: Giovanni Vecchione
;Date: 10/29/23
;Subject; Asg 10
;Write a function that transforms polygons from world space to screen space (and back).
;See 'COSC 3301 Coding 10.pptx' for details.

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

;drawToScreen

(define (drawToScreen polygon)
  ; Define the screen center coordinates
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
  (send dc draw-path polygon)
)


(drawToScreen myPolygon) ; draw polygon
(send myPolygon translate 20 20) ;  translate
(drawToScreen myPolygon) ; draw polygon
(send myPolygon scale 2 2) ;  scale
(drawToScreen myPolygon) ; draw polygon
(send myPolygon translate 30 60) ;  translate


;colorwheel
(define x-offset 0) ; Adjust as needed
(define y-offset -10)
(define distance-offset 20)

(define (drawColorWheel dc polygon)
  (for ([degree (in-range 0 360 4)]) ; Increment by 4 for 90 positions around the circle
    ; Convert the degree to a hue value between 0 and 1
    (define hue (/ degree 360.0))
    (define color (hsv->color (hsv hue 1.0 1.0)))
    
    ; Calculate position for the polygon
    (define x (+ (+ (* (cos (degrees->radians degree)) (- radius distance-offset)) (/ imageWidth 2)) x-offset))
    (define y (+ (+ (* (sin (degrees->radians degree)) (- radius distance-offset)) (/ imageHeight 2)) y-offset))
    
    ; Set color for the polygon
    (send dc set-pen color 2 'solid)
    (send dc set-brush color 'solid)
    
    ; Reset transformations
    (send polygon reset)
    
    ; Scale the polygon (.5 makes it smaller)
    (send polygon scale 2 2) ; Adjust the scale factors as needed
    
    ; Move polygon to coordinates
    (send polygon move-to x y)
    
    ; Create the polygon's shape
    (send polygon line-to (+ x 20) y)
    (send polygon line-to (+ x 15) (+ y 20))
    (send polygon line-to (+ x 5) (+ y 20))
    (send polygon close)
    
    ; Draw the polygon
    (send dc draw-path polygon)
    )
)

(drawColorWheel dc myPolygon)



myTarget

