;Programmer: Giovanni Vecchione
;Date: 11/21/23
;Subject: Asg_16 (based off asg14)

#lang racket/gui

(require racket/draw)
(require colors)

;NOTE:
;Was able to build the shape of the tree using rectangular polygons which ended up requiring more book keeping then i thought.
;Rotate function did not work as it kept causing a spiral image!

;ISSUE PRESENT FROM ASG14: Was unable to scale down to the required polygon size however it is achievable with this program
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
(define depth 3) ; depth of recursion (was at 7
(define scaleFactorX 1)
(define scaleFactorY 1)

;***************************drawToScreen FUNCTION********************
;Still working on the scale math to follow the image correctly
(define (drawToScreen dc myPolygon zoomX zoomY)

  (define xTrans 800)
  (define yTrans 1380)
  (define xScale .25)
  (define yScale .25)

  ;apply zoom to polygon
  (send myPolygon scale zoomX zoomY)
  
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

;*******************FUNCTION CREATES THE POLYGON AND CALLS drawToScreen*********************
;CHANGE: this was originally in the main function but to make debugging easier I seperated it.
(define (createPolygon x1 y1 x2 y2 zoomX zoomY)
   ; create polygon - FIX THIS
    (send myPolygon move-to x1 y1) ; input points (works like x-axis and y-axis)
    (send myPolygon line-to x2 y1)
    (send myPolygon line-to x2 y2)
    (send myPolygon line-to x1 y2)
    (send myPolygon close)
  ; draw polygon
    (drawToScreen my-dc myPolygon zoomX zoomY)                 
    (set! numPoly (+ 1 numPoly))
  )

;****************Function creates the branching of tree (MAIN)********************
(define (create-fractal-image depth rotateAmount x1 y1 scaleFactorX scaleFactorY LengthX LengthY zoomX zoomY)

  (when (> depth 0) ;; loop on polygons. move it a little, then draw it.
    
    ;Calculate the new points for the branches
    ; Scale the translation distances by the current scaleFactor
    (define scaledDistanceX (* LengthX scaleFactorX))
    (define scaledDistanceY (* LengthY scaleFactorY))
    
    ;Take into account the rotation and translation
    (define x2 (- x1 (* scaledDistanceX (cos rotateAmount))))
    (define y2 (- y1 (* scaledDistanceY (sin rotateAmount))))

    (createPolygon x1 y1 x2 y2 zoomX zoomY) ; create and draw the polygon
    

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
    (create-fractal-image (- depth 1) (- rotateAmount(* rotateFactor (/ pi 6)))  x2 y2 (* scaleFactorX  1) (* scaleFactorY 1) scaledDistanceX scaledDistanceY zoomX zoomY)
    ; Recursive call for "right" branch"
    (create-fractal-image (- depth 1) (+ rotateAmount (* rotateFactor (/ pi 6)))  x2 y2 (* scaleFactorX 1) (* scaleFactorY 1) scaledDistanceX scaledDistanceY zoomX zoomY)))))


;*******************reset all the polygons (use if needed)***************************
(define(resetPolygon)
  (send myPolygon reset)
  (set! numPoly 0)
)

; *************** Calls Main function and Applies zoom in ***************

(define (createImage zoomX zoomY)

  ; call main function
  (create-fractal-image depth rotateAmount startX1 startY1 scaleFactorX scaleFactorY baseLengthX baseLengthY zoomX zoomY) ;Calls the main function with the necessary zoom values for asg16

  ; calculate new window size
  (define scaledWidth  (/ imageWidth zoomX))
  (define scaledHeight (/ imageHeight zoomY))

  ;print world window size
    (printf "World Window Size: ~a" scaledHeight)
    (printf " x ~a\n" scaledWidth))


; ***************File output (image saved) ***************
;ISSUE: There is a limit to this method, 999 otherwise it errors if it goes above.
(define (file_output test_count prefix)
  (let ((suffix 
  (cond
    [(< test_count 10) (format "00~v.png" test_count)]
    [(< test_count 100) (format "0~v.png" test_count)]
    [ (format "~v.png" test_count)])))
    (string-append prefix suffix)))


; ***************Iteration and new zoom amount ***************
; iterate over different zoom levels, the number next to in-range dictates how many frames to save
(for ((i (in-range 600))
      (zoom_X (in-range 1 301 0.03))              ;zoom_X is the value dictating the x-axis
      (zoom_Y (in-range 1 301 0.03)))             ;zoom_Y is the value dictating the y-axis
  (let* ((file_name (file_output i "TEST-IMAGE"))
         ;Adjust zoom
         (newZoom_X (* zoom_X (+ 1 (* 0.001 i)))) ;Adjust the scale of the zoom
         (newZoom_Y (* zoom_Y (+ 1 (* 0.001 i)))));for both x and y
    (createImage newZoom_X newZoom_Y)
    ; get the final bitmap image
    (send my-dc get-bitmap)
    (send my-bitmap save-file file_name 'png))) ; This saves the image


(display "Number of polygons drawn: ")
(display numPoly)
(newline)

;Good News: the image is saving at each frame correctly
;Bad News: its not zooming in as it does so

my-bitmap
