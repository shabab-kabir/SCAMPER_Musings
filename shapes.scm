; shapes.scm
; A file from the first lab for CSC-151-01 Spring 2023
; Authors: 
;   Shabab Kabir
;   Sophie Lawrence
; Acknowledgements:
;   N/A

(sqrt 144)
(+ 3 4 )
(+ 3 (* 4 5))
(* (+ 3 4) 5)
(string-append "Hello" " " "World!")
(string-split "Twas brillig and the slithy tove" " " )
(length (string-split "Twas brillig and the slithy toves" " "))

(import image)

(*(+ 4 2) 2)
(- 1 (/ 1 2))
(string-length "Snicker snack")
(string-split "Snicker snack" "ck")
(circle 100 "solid" "pink")

(above (circle 40 "outline" "blue")
         (circle 60 "outline" "red"))

(beside (circle 40 "solid" "blue")
          (circle 40 "outline" "blue"))

(above (rectangle 60 40 "solid" "red")
         (beside (rectangle 60 40 "solid" "blue")
                 (rectangle 60 40 "solid" "black")))


(sqrt 137641)

(define trial01 11.2)
(define trial02 12.5)
(define trial03 8.5)
(define trial04 10.6)

; orginally an error would occur here do to the 1/4 and misnaming of trial03, now fixed for output:
(* (/ 1 4) (+ trial01 trial02 trial03 trial04))

; From the CSC 151 course materials:
;   https://csc151.cs.grinnell.edu/labs/scamper-intro-mathlan.html


(define name "Sophie")

(string-append "Hello " name)
(string-length name)

(import image)



(define blue-circle
  (circle 12 "outline" "blue"))

(define red-square
  (square 12 "outline" "red"))

(define black-rectangle
  (rectangle 12 24 "outline" "black"))

 (beside blue-circle red-square)

 (beside red-square blue-circle)

 (beside blue-circle blue-circle)

 (beside blue-circle red-square blue-circle red-square blue-circle) 

 (beside red-square black-rectangle)

 (above blue-circle black-rectangle)

 (beside red-square (above black-rectangle black-rectangle) red-square)

 (above black-rectangle 
        (beside 
              red-square 
              blue-circle))

(define simpleimage
  ((above 
      (beside
        (blue-circle)
        (red-square))
      (black-rectangle))))
    
simpleimage

(above black-rectangle (beside red-square blue-circle))

; (2 + 3)
; 7 * 9
; sqrt(49)
; (+ (87) (23))