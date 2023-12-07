(import image)

; spaceship.scm
;
; An amazing image of a rainbow spaceship I've created.
;
; CSC 151 (22fa)
; Mini Project 1, Part 1
; Author: Shabab Kabir
; Date: 2023-09-08
; Acknowledgements: Nyan Cat

(define column1
  (rectangle 100 25 "solid" "red"))

(define column2
  (above 
    column1
    (rectangle 100 25 "solid" "orange")))

(define column3
  (above 
    column2
    (rectangle 100 25 "solid" "yellow")))


(define column4
  (above 
    column3
    (rectangle 100 25 "solid" (color 0 255 0 1))))

(define column5
  (above 
    column4
    (rectangle 100 25 "solid" "blue")))

(define column6
  (above 
    column5
    (rectangle 100 25 "solid" (color 255 0 255 1))))

(define rainbow-spaceship
  (beside
    column1
    column2
    column3
    column4
    column5
    column6 ;notice vertical symmetry
    column5
    column4
    column3
    column2
    column1))

rainbow-spaceship
