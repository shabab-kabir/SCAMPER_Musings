(import image)

; freestyle.scm
;
; An amazing image of abstract art I've created.
;
; CSC 151 (22fa)
; Mini Project 1, Part 2
; Author: Shabab Kabir
; Date: 2023-01-30
; Acknowledgements: Abstract Art in General

; Following is simply for generatoring various random values
; this was made prior to the lesson on "Lambda"

(define 1n255 ; 1st random number generator to 255
    (random 255)
)

(define 2n255 ; 2nd random number generator to 255
    (random 255)
)

(define 3n255 ; 3rd random number generator to 255
    (random 255)
)

(define 4n255 ; 4th random number generator to 255
    (random 255)
)

(define 1n1 ; 1st random float generator
    (/ 1 (+ 1 (random 10)))
)

(define 2n1 ; 2nd random float generator
    (/ 1 (+ 1 (random 10)))
)

(define 3n1 ; 3rd random float generator
    (/ 1 (+ 1 (random 10)))
)

(define 4n1 ; 4th random float generator
    (/ 1 (+ 1(random 10)))
)

; Following are the subimages

(define circles-right-top
    (overlay/align "right" "top"
        (circle 1n255 "solid" (color 2n255 3n255 4n255 1n1))
        (circle 2n255 "solid" (color 3n255 4n255 1n255 2n1))
        (circle 3n255 "solid" (color 4n255 1n255 2n255 3n1))
        (circle 4n255 "solid" (color 1n255 2n255 3n255 4n1))
    ))

(define circles-middle-center
    (overlay/align "middle" "center"
        (circle 1n255 "solid" (color 2n255 3n255 4n255 1n1))
        (circle 3n255 "solid" (color 4n255 1n255 2n255 3n1))
        (circle 2n255 "solid" (color 3n255 4n255 1n255 2n1))
        (circle 4n255 "solid" (color 1n255 2n255 3n255 4n1))
    ))

(define triangles-left-bottom
    (overlay/align "left" "bottom"
        (triangle 1n255 "solid" (color 2n255 3n255 4n255 1n1))
        (triangle 2n255 "solid" (color 3n255 4n255 1n255 2n1))
        (triangle 3n255 "solid" (color 4n255 1n255 2n255 3n1))
        (triangle 4n255 "solid" (color 1n255 2n255 3n255 4n1))
    ))

(define rectangles-right-bottom
    (overlay/align "right" "bottom"
        (rectangle 4n255 1n255 "outline" (color 1n255 2n255 3n255 4n1))
        (rectangle 2n255 3n255 "outline" (color 3n255 4n255 1n255 2n1))
        (rectangle 1n255 4n255 "outline" (color 2n255 3n255 4n255 1n1))
        (rectangle 3n255 2n255 "outline" (color 4n255 1n255 2n255 3n1))
    ))

(define ellipse-top-bottom
    (overlay/align "left" "top"
        (ellipse 4n255 1n255 "outline" (color 1n255 2n255 3n255 4n1))
        (ellipse 2n255 3n255 "outline" (color 3n255 4n255 1n255 2n1))
        (ellipse 1n255 4n255 "outline" (color 2n255 3n255 4n255 1n1))
        (ellipse 3n255 2n255 "outline" (color 4n255 1n255 2n255 3n1))
    ))

; Following are the image sections

(define image-left-top
    (overlay/align "left" "top" 
        triangles-left-bottom
        rectangles-right-bottom
        ellipse-top-bottom
        circles-right-top
        circles-middle-center
))

(define image-right-bottom
    (overlay/align "right" "bottom" 
        triangles-left-bottom
        rectangles-right-bottom
        ellipse-top-bottom
        circles-right-top
        circles-middle-center
))

(define image-middle-bottom
    (overlay/align "middle" "bottom" 
        triangles-left-bottom
        rectangles-right-bottom
        ellipse-top-bottom
        circles-right-top
        circles-middle-center
))

(define image-rotate-overlay
    (rotate 2n255 (overlay
        triangles-left-bottom
        rectangles-right-bottom
        ellipse-top-bottom
        circles-right-top
        circles-middle-center
)))

; Following is the final image

(define my-image
    (beside
        (above
            image-left-top
            image-right-bottom)
        (above
            image-middle-bottom
            image-rotate-overlay)
))

my-image