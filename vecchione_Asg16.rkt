;Programmer: Giovanni Vecchione
;Date: 11/21/23
;Subject: Asg_14

#lang racket/gui

(require racket/draw)
(require colors)

;NOTE:
;Was able to build the shape of the tree using rectangular polygons which ended up requiring more book keeping then i thought.
;Rotate function did not work as it kept causing a spiral image!

;ISSUE PRESENT: Was unable to scale down to the required polygon size however it is achievable with this program
;by setting the values of the scaleFactorX and scaleFactorY to two decimal spots to the right it reached the goal.
; However it did not retian its shape (did not have time to correct it, noticed it too late)
;;determined this through iterating through the scaledDistanceX which would be the unit size of the polygon
;(display "Scaled Distnace: 5.551376302080004e-13") 

;A cool thing tho is that I was able to color code the fractal branching at each iteration.
 

;Set up dimensions for image
(define imageWidth 2048)
(define imageHeight 1152)
(define numPoly 0)                ; variable to keep track of the polygons drawn
(define startX1 (/ imageWidth 2)) ;Start point for X
(define startY1 imageHeight)      ;Start point for Y
(define polyThreshold 1.0e-12)    ; set threshold for smallest polygon
(define baseLengthX 1152)         ;establish the dimensions of the polygons
(define baseLengthY 2048)
(define wWidth (* 2 imageWidth))  ;World Width
(define wHeight (* 2 imageHeight));World Height

; Create a new bitmap of size 2048 x 1152
(define my-bitmap (make-bitmap imageWidth imageHeight))
; Create a drawing context for the bitmap
(define my-dc (new bitmap-dc% [bitmap my-bitmap]))

; Set the brush and pen for the drawing context
(send my-dc set-pen "white" 2 'solid)
(send my-dc set-brush "gray" 'solid)
(send my-dc draw-rectangle 0 0 imageWidth imageHeight) ;background color

; Start position and parameters for the fractal
; Adjust the initial X2 and Y2 translation amounts for the starting branch
(define rotateAmount (/ pi 2))
(define depth 7) ; depth of recursion
(define scaleFactorX 1)
(define scaleFactorY 1)

;drawToScreen FUNCTION
(define xTrans 800)
(define yTrans 1380)
(define xScale .25)
(define yScale .25)

(define (drawToScreen dc myPolygon)
    ; Convert the polygon to screen coordinates
    (send myPolygon scale xScale yScale)
    (send myPolygon translate xTrans yTrans)

 ;changes color of polygons per 10k polygons stops at 30k
    (cond
  [(< numPoly 10000)
   ; Set colors for the first 10,000 polygons
   (send dc set-pen "black" 2 'solid)
   (send dc set-brush (make-color 25 22 16) 'solid)]

  [(< numPoly 20000)
   ; Set colors for the next 10,000 polygons
   (send dc set-pen "blue" 2 'solid)
   (send dc set-brush (make-color 100 100 250) 'solid)]

  [(< numPoly 30000)
   ; Set colors for the next 10,000 polygons
   (send dc set-pen "green" 2 'solid)
   (send dc set-brush (make-color 80 250 80) 'solid)]

  ; ... Add more conditions as needed ...

  [else
   ; Default colors for numPoly >= 30000 or the highest specified range
   (send dc set-pen "red" 2 'solid)
   (send dc set-brush (make-color 250 100 100) 'solid)])

    (send dc draw-path myPolygon)
    
    ; Convert the polygon back to world coordinates
    (send myPolygon translate (- xTrans) (- yTrans))
    (send myPolygon scale (/ 1.0 xScale) (/ 1.0 yScale)))


(define myPolygon (new dc-path%)) ;Define polygon

;FUNCTION CREATES THE POLYGON AND CALLS drawToScreen
(define (createPolygon x1 y1 x2 y2)
   ; create polygon - FIX THIS
    (send myPolygon move-to x1 y1) ; input points (works like x-axis and y-axis)
    (send myPolygon line-to x2 y1)
    (send myPolygon line-to x2 y2)
    (send myPolygon line-to x1 y2)
    (send myPolygon close)
  ; draw polygon
    (drawToScreen my-dc myPolygon)                 
    (set! numPoly (+ 1 numPoly))
  )

;Function creates the branching of tree (MAIN)
(define (create-fractal-image depth rotateAmount x1 y1 scaleFactorX scaleFactorY LengthX LengthY)

  (when (> depth 0) ;; loop on polygons. move it a little, then draw it.
    
    ;Calculate the new points for the branches
    ; Scale the translation distances by the current scaleFactor
    (define scaledDistanceX (* LengthX scaleFactorX))
    (define scaledDistanceY (* LengthY scaleFactorY))
    
    ;Take into account the rotation and translation
    (define x2 (- x1 (* scaledDistanceX (cos rotateAmount))))
    (define y2 (- y1 (* scaledDistanceY (sin rotateAmount))))

    (createPolygon x1 y1 x2 y2) ; create and draw the polygon

    ;Do-Loop determines how many new branches
    (do ([i 0 (+ i 1)])
        ((= i 3))
      
      ;adjust factors for length
      (let* ([scaleFactorY (cond [(= i 0) .5]    
                                  [(= i 1) .7]   
                                  [(= i 2) .7])]
             
             [scaleFactorX (cond [(= i 0) 1]    
                                  [(= i 1) 0.8]   
                                  [(= i 2) .4])]  
             ;rotational change (angle)
              [rotateFactor (cond [(= i 0) -1.3]     
                                  [(= i 1) .5]     
                                  [(= i 2) 0])])
        
    ; Recursive call for "left" branch"
    (create-fractal-image (- depth 1) (- rotateAmount(* rotateFactor (/ pi 6)))  x2 y2 (* scaleFactorX  1) (* scaleFactorY 1) scaledDistanceX scaledDistanceY)
    ; Recursive call for "right" branch"
    (create-fractal-image (- depth 1) (+ rotateAmount (* rotateFactor (/ pi 6)))  x2 y2 (* scaleFactorX 1) (* scaleFactorY 1) scaledDistanceX scaledDistanceY)))))


;reset all the polygons
(define(resetPolygon)
  (send myPolygon reset)
  (set! numPoly 0)
)


(define zoomScale .9) ;sets zoom scale

;zoomIn Function
(define (zoomIn iterations)
  (for ([i iterations])
    (printf "Creating image ~a\n" i)

    (createPolygon) ;ISSUE HERE needs to pass coordinates of the polygon, this is the issue due to branching
                    ;Possible solution is to have the x1,y1,x2,y2 coordinates work as a global variable
    (send my-dc save-file (format "myPic~a.png" i) 'png)
    (if (< i (- iterations 1))
        (resetPolygon)
        #f)
  
    (set! wWidth (* wWidth zoomScale))
    (set! wHeight (* wHeight zoomScale))
    (printf "World window Size: ~a x ~a \n" wWidth wHeight)
    (set! xScale (/ imageWidth wWidth))
    (set! yScale (/ imageHeight wHeight))
    (set! xTrans (* (/ wWidth 2) xScale))
    (set! yTrans (* (/ wHeight 2) yScale))
    )
  )

(zoomIn 3) ;the number dicatates how many images to create

(create-fractal-image depth rotateAmount startX1 startY1 scaleFactorX scaleFactorY baseLengthX baseLengthY) ; CALL CREATE_FRACTAL_IMAGE FUNCTION

(display "Number of polygons drawn: ")
(display numPoly)
(newline)


my-bitmap