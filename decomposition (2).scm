; CSC 151-NN (Spring 2023)
; Lab: Decomposition 
; Authors: Shabab, Shibam
; Date: 01/30/2023
; Acknowledgements: 
;   ACKNOWLEDGEMENTS HERE

; +-----------+------------------------------------------------------
; | Libraries |
; +-----------+

(import image)


; +---------------------+--------------------------------------------
; | Exercise 1: A party |
; +---------------------+
(define little-green-triangle
    (triangle 10 "solid" "green"))

(define circle-face
     (circle 20 "outline" "black"))

(define rectangle-body
    (rectangle 5 20 "solid" "black"))

(define party-person
   (above little-green-triangle
          circle-face
          rectangle-body))



(define party-people
  (beside party-person
          party-person
          party-person
          party-person
          party-person))

party-people

; +-------------------------+----------------------------------------
; | Exercise 2: A landscape |
; +-------------------------+


(define tree
  (above (triangle 25 "solid" (color 0 255 0 1))
        (rectangle 5 25 "solid" "brown"))
)

(define house
  (above (triangle 40 "solid" "blue")
          (rectangle 40 40 "solid" "red")
  )
)

(define landscape
  (beside
        tree
        tree
        house
        tree
        tree))
landscape

; --- ...now partner A drives! ---

; +------------------------------+-----------------------------------
; | Exercise 3: Falling dominoes |
; +------------------------------+

(define sun
    (overlay/align "left" "top" 
              (circle 120 "solid" "orange")
              (circle 122 "solid" "black")))

(define mercury
    (overlay/align "left" "top"
              (circle 10 "solid" "grey")
              (circle 11 "solid" "black"))
)

(define venus
    (overlay/align "left" "top"
              (circle 30 "solid" "yellow")
              (circle 32 "solid" "black"))
)

(define earth
    (overlay/align "left" "top"
              (circle 50 "solid" "blue")
              (circle 52 "solid" "black"))
)

(define mars
    (overlay/align "left" "top"
              (circle 40 "solid" "red")
              (circle 42 "solid" "black"))
)

(define jupiter
    (overlay/align "left" "top"
              (circle 80 "solid" (color 128 128 50 1))
              (circle 82 "solid" "black"))
)

(define saturn
    (overlay/align "left" "top"
              (circle 70 "solid" (color 254 251 180 1))
              (circle 72 "solid" "black"))
)

(define uranus
    (overlay/align "left" "top"
              (circle 50 "solid" (color 109 250 251 1))
              (circle 52 "solid" "black"))
)

(define neptune
    (overlay/align "left" "top"
              (circle 40 "solid" (color 12 39 151 1))
              (circle 42 "solid" "black"))
)

(define pluto
    (overlay/align "left" "top"
              (circle 8 "solid" (color 235 235 235 1))
              (circle 9 "solid" "black"))
)

(define white-rectangle
    (rectangle 10 10 "solid" "white")
)

(define the-planets
    (beside sun
            white-rectangle
            mercury
            white-rectangle
            venus
            white-rectangle
            earth
            white-rectangle
            mars
            white-rectangle
            jupiter
            white-rectangle
            saturn
            white-rectangle
            uranus
            white-rectangle
            neptune
            white-rectangle
            pluto)
)
the-planets


; --- ...now partner B drives! ---

; +---------------------------+-----------------------------------------
; | Exercise 4: Sun triangles |
; +---------------------------+

; Switch roles one last time!  This is perhaps the longest of the
; exercises. Even if you don't complete this exercise, you should read the
; material below, put in your best effort, and capture your work below.

; You will find the standard `overlay` function useful for this exercise:

; `(overlay img1 img2 ...)` creates a new image that appears as if
; the images were overlaid on top of each other (not above, but
; "nearer" in the third dimension. This requires that we change the
; *opacity* of the shape's color.

; In addition, when we specify the color of an image with the
; `(color r g b a)` function, we can also fill of an image, *e.g.*, using
; `"solid"` as in `(square 50 "solid" "red")`, we can also specify a
; number, *e.g.*, `(square 50  "red")`.  The number, which must
; range from 0–255 is interpreted as the degree of *opacity* of
; the shape's color.  0 is interpreted as fully transparent whereas
; 255 is equivalent to `"solid"`.

; Use these functions to define an image, `sun-triangles`, that looks
; like the one in the lab handout.

; The image is a collection of transparent yellow triangles with black
; outlines on top of an orange circle.

; This one is a bit trickier than the previous ones, and there is a
; few ways you might approach it.  Not all approaches will result in
; the exact same image, so we aren't looking for your result to be
; identical, but it is possible, and we encourage you to think carefully
; about your decomposition to get that result.

; At the very least, you will need to draw yellow triangles with
; outlines.  To achieve this effect, you should use `overlay` and two
; triangles with a combination of `"solid"` and `"outline"` fills.

(define sun2
    (circle 134 "solid" "orange")
)

(define top-triangle
    (overlay
            (triangle 270 "solid" (color 255 255 0 0.25))
            (triangle 273 "outline" "black"))
)

(define bot-triangle
    (rotate 180 (overlay
            (triangle 270 "solid" (color 255 255 0 0.25))
            (triangle 273 "outline" "black"))
))


 (define sun-triangles
   (overlay
        top-triangle
        bot-triangle
        sun2))

sun-triangles

; +---------------------+--------------------------------------------
; | Submitting your lab |
; +---------------------+

; Yay!  You're done!  Well, you are almost done.  You still need
; to submit the lab and make sure it passes the autograder.

; Once you have all the exercises in your file, `decomposition.scm`,
; it is now complete!  Finally, **_one_ member of your group** can
; upload the completed `decomposition.scm` file to Gradescope for
; this lab.  Make sure that you upload you work as a group assignment
; and that you include your partner in the submission!  

; Don't forget to check the autograder results.  Note that the
; autograder gives 0.999 rather than 1 if you get everything 
; correct.

; If you get errors from the autograder, you should discuss them with
; one of the class staff (or you can try to resolve them yourself).

; You should also check to make sure that the file looks readable.
; (It usually does, but there are times that Scheme does strange
; things to your file.  In such cases, ask the class staff for help.)

; Finally, we would also recommend that you use Teams or email to
; exchange files or portions thereof.

; +---------------------------+--------------------------------------
; | For those with extra time |
; +---------------------------+

; If you find that you have extra time, you should attempt the 
; following exercises.  You need not turn them in.

; --- ...either partner can drive for this one! ---

; +------------------------------------+-----------------------------
; | Extra: Expanding prior exercises   |
; +------------------------------------+

; Expand one of the ideas from a prior exercise.  Perhaps you'll add
; a door to the house, or some fruit to the trees, or bowties to the
; party people. You can even try to use transparency and overlay/offset
; to add texture to the planets. Have fun!
