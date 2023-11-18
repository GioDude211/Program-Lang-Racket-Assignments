#lang racket/gui

(require racket/draw)
(require colors)

;Set up dimensions for image
(define imageWidth 2048)
(define imageHeight 1152)

(define numPoly 0)                ; variable to keep track of the polygons drawn
(define startX1 (/ imageWidth 2)) ;Start point for X
(define startY1 imageHeight);Start point for Y


; set threshold for smallest polygon
(define polyThreshold 1.0e-12)

;establish the dimensions of the polygons
(define baseLengthX 1152)
(define baseLengthY 2048)

; Create a new bitmap of size 2048 x 1152
(define my-bitmap (make-bitmap imageWidth imageHeight))
; Create a drawing context for the bitmap
(define my-dc (new bitmap-dc% [bitmap my-bitmap]))

; Set the brush and pen for the drawing context
(send my-dc set-pen "white" 2 'solid)
(send my-dc set-brush "gray" 'solid)

;background color
(send my-dc draw-rectangle 0 0 imageWidth imageHeight)

;Up until this point code is good ----------^
; Do-loop in racket works like a for-loop,  (do ([i 0 (+ i 1)])

; Start position and parameters for the fractal
; Adjust the initial X2 and Y2 translation amounts for the starting branch
(define rotateAmount (/ pi 2))
(define depth 7) ; depth of recursion
(define scaleFactorX 1)
(define scaleFactorY 1)

;drawToScreen FUNCTION -NOTHING WRONG HERE
(define (drawToScreen dc myPolygon)
  (let ([xTrans 800]
        [yTrans 1100]
        [xScale .2]
        [yScale .2])
    ; Convert the polygon to screen coordinates
    (send myPolygon scale xScale yScale)
    (send myPolygon translate xTrans yTrans)
    
    ; Draw the polygon in screen coordinates
    (send dc set-pen "black" 2 'solid) ; set the line color 
    (send dc set-brush (make-color 25 22 16) 'solid) ; set color for filling
    (send dc draw-path myPolygon)
    
    ; Convert the polygon back to world coordinates
    (send myPolygon translate (- xTrans) (- yTrans))
    (send myPolygon scale (/ 1.0 xScale) (/ 1.0 yScale))))

;Function creates the image (MAIN)
(define (create-fractal-image depth rotateAmount x1 y1 scaleFactorX scaleFactorY LengthX LengthY)

  (when (> depth 0) ;; loop on polygons. move it a little, then draw it.
    
    ;Calculate the new points for the branches
    ; Scale the translation distances by the current scaleFactor
    (define scaledDistanceX (* LengthX scaleFactorX))
    (define scaledDistanceY (* LengthY scaleFactorY))
    
    ;Take into account the rotation and translation
    (define x2 (- x1 (* scaledDistanceX (cos rotateAmount))))
    (define y2 (- y1 (* scaledDistanceY (sin rotateAmount))))


     ; ; create a new line for the poly
    (define poly (new dc-path%))
    (send poly move-to x1 y1)
    (send poly line-to x2 y2)
    
    ; draw polygon
    (drawToScreen my-dc poly)                 
    (set! numPoly (+ 1 numPoly))
   
    
    ;Do-Loop determines how many new branches
    (do ([i 0 (+ i 1)])
        ((= i 2))
      
      ;adjust factors for length
      (let* ([scaleFactorY (cond [(= i 0) .5]    
                                  [(= i 1) .7]   
                                  [(= i 2) 0.7])]
             
             [scaleFactorX (cond [(= i 0) 1]    
                                  [(= i 1) 0.8]   
                                  [(= i 2) .4])]  
             ;rotational change (angle)
              [rotateFactor (cond [(= i 0) -1.3]     
                                  [(= i 1) .5]     
                                  [(= i 2) 0])])
        
    ; Recursive call for "left" branch"
    (create-fractal-image (- depth 1) (- rotateAmount(* rotateFactor (/ pi 6)))  x2 y2 scaleFactorX scaleFactorY scaledDistanceX scaledDistanceY)
    ; Recursive call for "right" branch"
    (create-fractal-image (- depth 1) (+ rotateAmount (* rotateFactor (/ pi 6)))  x2 y2 scaleFactorX scaleFactorY scaledDistanceX scaledDistanceY)))))




(create-fractal-image depth rotateAmount  startX1 startY1 scaleFactorX scaleFactorY baseLengthX baseLengthY) ; CALL CREATE_FRACTAL_IMAGE FUNCTION

(display "Number of polygons drawn: ")
(display numPoly)
(newline)

my-bitmap

;NOTES:The image output is spiral in nature for some reason. The drawToScreen function appears to be not affecting this, furthermore I added a way to correctly
;track the transformations to keep track of the new x and y coordinates. For some reason I believe this is where the issue resides. I don't think its because of
;the duplicate poly function

;ISSUE 2: Theres a problem with the rotation of the branches

;IT IS CLOSE BUT IT IS NOT TRANSLATING TO THE CORRECT LOCATIONS