(import image)

;; CSC 151-NN (TERM)
;; Lab: Reading and writing procedures
;; Authors: Shabab Kabir, Shibam Mukhopadhyay
;; Date: 1 February 2023
;; Acknowledgements:
;;   N/A

; +------------------+-----------------------------------------------
; | Helper functions |
; +------------------+

;;; (color-square side-length color) -> image?
;;;   side-length : non-negative-integer?
;;;   color : color?
;;; Create a solid square in the given color.
(define color-square
  (lambda (side-length color)
    (rectangle side-length side-length "solid" color)))

;;; red-square : image?
;;; A small red square, useful for exercises in this lab.
(define red-square (color-square 20 "red"))

;;; black-square : image?
;;; A small black square, useful for exercises in this lab.
(define black-square (color-square 20 "black"))

; +-------------+----------------------------------------------------
; | Preparation |
; +-------------+


; Before beginning this lab, please make sure to update Scamper!
;
; a. In the left-hand sidebar, click the extensions icon to open the extensions
;    marketplace.
;
; b. Under your installed extensions, click the `scamper-vscode` extension.
;
; c. Verify that the version listed on the extension's page matches the
;    the most recent version. Your instructor will tell you what that is!
;
; d. If you need to update, do so if there a button prompting you to update
;    or reload VSCode now.
;
; e. To force an upgrade, click the down arrow (⌄) next to the Uninstall
;    button, "Install another version...", and choose the most recent
;    version to install.


; +---------------------------------------------+--------------------
; | Exercise 1: Interpreting function execution |
; +---------------------------------------------+

; Approximate time: 10 minutes
; For this problem, B side drives and A side navigates.

; In this exercise, we will use our substitutive model of Racket program
; execution to predict the behavior of some tricky scenarios involving
; functions.  We will then generalize what we see to arrive at some conclusions
; regarding the behavior of functions.

; Consider the following function definitions:

(define my-func-1
  (lambda (x y)
    (+ x (* y y) x)))

(define my-func-2
  (lambda (n)
    (* n 2)))

(define my-func-3
  (lambda (n)
    (+ (my-func-2 n) (my-func-2 8) n)))

; Give the execution trace (i.e., step-by-step evaluation) of each of
; the following expressions in the space above them.  Note that when you
; evaluate an arithmetic operator with more than 2 arguments, you can
; evaluate the addition in one step, e.g., (+ 1 1 1) --> 3. Make sure
; to write your trace as a collection of line comments so that your
; file still runs successfully. (Similarly to how this prose is just
; one big comment block!)

; Use the explorations pane to check that your trace produces the correct value.
; We recommend checking each intermediate expression as you go.

; a.
;      (my-func-1 (+ 1 1) 5)
;      (my-func-1 2 5)
;      (+ 2 (* 5 5) 2)
;      (+ 2 25 2)
;  --> 29

(my-func-1 (+ 1 1) 5)

; b.
;     (my-func-2 (my-func-2 (my-func-2 3)))
;     (my-func-2 (my-func-2 (* 3 2)))
;     (my-func-2 (my-func-2 6))
;     (my-func-2 (* 6 2))
;     (my-func-2 12)
;     (* 12 2)
; --> 24

(my-func-2 (my-func-2 (my-func-2 3)))

; +----------------------+-------------------------------------------
; | Exercise 2: What if? |
; +----------------------+

; Driver: A
; Approximate time: Five to Ten minutes

; The previous exercises might suggest an important maxim regarding functions
; and parameter names:

; > The names of the parameters of a function don't matter.

; To delve into what this means, consider the following alternative
; definition of my-func-2:

(define my-func-2-alt
  (lambda (x)
    (* x 2)))

; With your partner, use your mental model of computation to compare the
; behavior of my-func-2 and my-func-2-alt and in a few sentences, explain what
; the maxim presented above means.  E.g., you might consider

(my-func-2-alt (my-func-2-alt (my-func-2-alt 3)))

; and compare the evaluation.

; Write your explanation in the space below:
;     (my-func-2-alt (my-func-2-alt (my-func-2-alt 3)))
;     (my-func-2-alt (my-func-2-alt (* 3 2)))
;     (my-func-2-alt (my-func-2-alt 6))
;     (my-func-2-alt (* 6 2))
;     (my-func-2-alt+ 12)
;     (* 12 2)
; --> 24

; +--------------------+---------------------------------------------
; | Exercise 3: Houses |
; +--------------------+
;
; Driver: A
; Our reading on procedures contained a procedure to make a simple house,
; reproduced below for your use.

; Copied from the CSC-151 reading "Writing your own procedures"
(define simple-house
  (lambda (size)
    (above (triangle size "solid" "red")
           (rectangle (* 0.80 size) size "solid" "black"))))

; a.  Use the Explorations pane to verify that you can create houses of
; different sizes.  You may choose to do the same examples as in the reading or
; you may choose to do a few of your own.
;
; You need not submit anything for this part of the exercise.

; b.  The original simple-house procedure does not color the house.  Write a new
; procedure, (painted-house size color), in the space below.  The procedure
; should take takes both the size and color as parameters and makes a house of
; that specified size whose main body is in the specified color.  (The roof
; should remain red.)
;
; Make sure to add a comment to painted-house that indicates you adapted this
; code from the reading.  We always credit our sources!

(define painted-house ; adapted from reading
    (lambda (size color)
        (above 
                (triangle size "solid" "red")
                (rectangle (* 0.80 size) size "solid" color))))

(painted-house 100 "blue")

; c.  In addition to this simple-house procedure, the reading also included a
; definition for a more complex house with a door and doorknob.  Use this as the
; basis of a new function, fancy-house, that makes a house of a given size and
; color, including a brown door and yellow doorknob.  Make sure that the size of
; the door scales with the size of the house.

(define fancy-house
  (lambda (size color)
          (overlay/align "middle" "bottom"
                 (overlay/align "left" "center"
                              (circle (* 0.03 size) "solid" "yellow")
                              (rectangle (* 0.2 size) (* 0.4 size) "solid" "brown"))
          (above 
                (triangle size "solid" "red")
                (rectangle (* 0.80 size) size "solid" color)))))
(fancy-house 250 "green")
; +------------------------+-----------------------------------------
; | Exercise 4: Snowpeople |
; +------------------------+

; Driver: B
; The reading on images contained an image that looked a bit like a snowperson.

; a. Complete the definition of the procedure, (snowperson size), that creates a
; simple snowperson with three white circles with black outlines, where the size
; is used for the size of the largest part of the snowperson, the base.

(define snowperson
  (lambda (size)
    (above
      (overlay
        (circle 50 "solid" "white")
        (circle 50 "outline" "black"))
      (overlay
        (circle 75 "solid" "white")
        (circle 75 "outline" "black"))
      (overlay
        (circle size "solid" "white")
        (circle size "outline" "black")))))

(snowperson 10)

; b. Complete the definition of (snowperson-revisited height) that creates a
; simple snowperson with three white circles with black outlines.  Unlike
; snowperson, this function's parameter controls the *total height* of the
; snowperson rather than just the base.  (It should be as close to the height
; parameter as you can get it.)

(define snowperson-revisited
  (lambda (height)
    (above
      (overlay
        (circle (* 0.5 height) "solid" "white")
        (circle (* 0.5 height) "outline" "black"))
      (overlay
        (circle (* 0.75 height) "solid" "white")
        (circle (* 0.75 height) "outline" "black"))
      (overlay
        (circle height "solid" "white")
        (circle height "outline" "black")))))

(snowperson-revisited 100)

; c.  Complete the definition of (snowperson-with-hat height) that behaves like
; snowperson-revisited but adds a black top hat to the top of the snowperson.
; Note that the total height should include the top hat.

(define snowperson-with-hat
  (lambda (height)
    (above
      (overlay/align "middle" "bottom"
        (rectangle (* 0.95 height) (* 0.25 height) "solid" "black")
        (square (* 0.65 height) "solid" "brown"))
      (overlay
        (circle (* 0.5 height) "solid" "white")
        (circle (* 0.5 height) "outline" "black"))
      (overlay
        (circle (* 0.75 height) "solid" "white")
        (circle (* 0.75 height) "outline" "black"))
      (overlay
        (circle height "solid" "white")
        (circle height "outline" "black")))))

(snowperson-with-hat 100)

; d.  From the prior exercise, you might have realized that top hats are
; universal and should be applicable to any image you can think of.  Define a
; function (add-top-hat width height image) that places a top hat on the given
; size above the provided image.  

; For example, `(add-top-hat 20 10 (circle 20 "solid" "blue"))` will
; add a top hat that is twenty units wide and ten units high to a
; circle of radius 10 and `(addtop-hat 20 30 (snowperson-revisited 50))`
; will had a top had that is 20 units wide and 30 units high to the
; top of a 50-unit-high snowperson.

(define add-top-hat
  (lambda (width height image)
    (above 
      (overlay/align "middle" "bottom"
          (rectangle width (* 0.45 height) "solid" "black")
          (square height "solid" "brown"))
      (overlay
        image))))

(add-top-hat 20 10 (circle 20 "solid" "blue"))
(add-top-hat 50 30 (snowperson-revisited 50))

; +----------------+-------------------------------------------------
; | Submit the lab |
; +----------------+

; You're done.  Congratulations!  It's time to submit your lab.  After
; doing so, you should review the extra problems below and, as
; appropriate, pick one or more to attempt.  We'd suggest that you
; start with the first extra problem.

; To turn in your work:

; a. If you were working with separate parts, combine the two parts of 
;    the assignment into one file.
; b. Ensure that your file runs properly.  (E.g., when we click "Run", it should
;    work not produce errors.)
; c. Make sure that this file is named `procedures.scm`.
; d. Submit this final file to Gradescope.  Make sure to submit your work 
;    as a group submission and include your partner in the submission.
; e. Wait for the autograder results to make sure that you met the
;    basic goals.

