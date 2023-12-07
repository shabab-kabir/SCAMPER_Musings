;; CSC 151 (Spring '23)
;; Lab: Numeric Recursion (numeric-recursion.scm)
;; Authors: Shabab Kabir, Marina Ananias, and Fatin Wahid
;; Date: 06 March 2023
;; Acknowledgements:
;;   Just Ben, but also the funny fog on my way here.

(import image)

;; -------------------
"Problem 1: Replicate"
;; -------------------

;; (Partner A drives!)

;; Implement a recursive function (replicate v n) that takes a value v and
;; natural number n as input and returns a list that contains n copies of v.
;;
;; (replicate "q" 5)
;; > (list "q" "q" "q" "q" "q")
;; (replicate "hello" 0)
;; > (list)
;;
;; Make sure to give a recursive decomposition, docstring, and test suite
;; for the function.
;;
;; (Note that replicate is really another name for make-list, so you can
;; use make-list to write easily write test cases for your function).
;;
;; To replicate a value v n times:
;; + If n is zero... <TODO: fill me in!>
;; + If n is non-zero... <TODO: fill me in!>

(define replicate
  (lambda (v n)
          (match n
                 [0 null]
                 [_ (cons v (replicate v (- n 1)))])))
(replicate "q" 5)

;; -----------------
"Problem 2: Harmony"
;; -----------------

;; (Partner B drives!)

;; Implement a recursive function (harmonic-sequence-sum n) that takes a
;; natural number n as input and returns the sum of the first n terms of the
;; harmonic sequence. The harmonic sequence is defined as follows:
;;
;; 0 + 1/1 + 1/2 + 1/3 + 1/4 + 1/5 + ...
;;
;; (harmonic-sequence-sum 5)
;; > 2.283333333333333
;; (harmonic-sequence-sum 100)
;; > 5.187377517639621
;; (harmonic-sequence-sum 0) 
;; > 0
;;
;; Make sure to give a recursive decomposition, docstring, and test suite
;; for the function.
;;
;; The sum of the first n terms of the harmonic-sequence is:
;; + If n is zero... <TODO: fill me in!>
;; + If n is non-zero... <TODO: fill me in!>

(define harmonic-sequence-sum
  (lambda (n)
          (match n
                [0 0]
                [_ (+ (/ 1 n) (harmonic-sequence-sum (- n 1)))])))
(harmonic-sequence-sum 0)
(harmonic-sequence-sum 5)
(harmonic-sequence-sum 100)

;; --------------
"Problem 3: Drop"
;; --------------

;; (Partner A drives!)

;; Implement a recursive function (my-drop n l) that takes a list l and natural
;; number n and returns l, but with the first n elements of l removed. If
;; n is greater than the length of l, then null is returned.
;;
;; (my-drop 3 (range 10))
;; > (list 3 4 5 6 7 8 9)
;; (my-drop 0 (range 10))
;; > (list 0 1 2 3 4 5 6 7 8 9)
;; (my-drop 5 null)
;; > null
;;
;; For my-drop, you will need to decompose both n and l! Consequently, write
;; your recursive decomposition in terms of the 4 cases we have based on the
;; recursive definitions for natural numbers and lists. To pattern match on
;; both n and l at the same time, you can use pair them up, e.g., (pair n l)
;; creates a pair of n and l that you can pattern match on with
;; cons as the pattern. It turns out that pair is just an alias for cons!
;;
;; (Note that my-drop is really the drop function provided in the standard
;; library. Feel free to use drop in your test cases!)
;;
;; To drop n elements from l:
;; <TODO: fill in your recursive decomposition here>

(define my-drop
  (lambda (n l)
          (cond
                [(= n 0) l]
                [(>= n (length l)) null]
                [else (cons n (my-drop (+ n 1) l))])))
(my-drop 3 (range 10))

;; --------------
"Problem 4: Take"
;; --------------

;; (Partner A drives!)

;; Implement a recursive function (my-take n l) that takes a list l and natural
;; number n and returns the first n elements of l as a list. If n is greater
;; than l, then l is returned.
;;
;; (my-take 3 (range 10))
;; > (list 0 1 2)
;; (my-take 0 (range 10))
;; > (list)
;; (my-take 5 null)
;; > null
;;
;; Like my-drop, my-take will need to decompose both n and l. Your recursive
;; decomposition should have 4 cases based on the recursive definitions for
;; natural numbers nad lists.
;;
;; (Note that my-take is really the take function provided in the standard
;; library. Feel free to use take in your test cases!)
;;
;; To take n elements from l:
;; <TODO: fill in your recursive decomposition here>

(define my-take
  (lambda (n l)
          (cond
                [(= n 0) null]
                [(> n (length l)) "error, n surpassed max length of l"]
                [else (cons (my-take (- n 1) l) (list-ref l (- n 1)) )])))
(my-take 3 (range 10))

;; -------------------
"Problem 6: Triangles"
;; -------------------

;; Sierpinski triangles are a fractal drawing composed of a collection of
;; triangles nested inside of each other according to the following rules
;; (taken from: https://en.wikipedia.org/wiki/Sierpi%C5%84ski_triangle):
;; 
;; 1. Start with an equilateral triangle.
;; 2. Subdivide it into four smaller congruent equilateral triangles and
;;    remove the central triangle.
;; 3. Repeat step 2 with each of the remaining smaller triangles infinitely.
;;
;; Call each layer of Sierpinski triangles a level. At level 0, we draw no
;; triangles.
;;
;; Examples of Sierpinski Triangles for nesting level n = 1, n = 2, and n = 3
;; can be found on the webpage for this lab.
;;
;; Write a recursive function (sierpinski size color n) that draws n levels
;; of sierpinski triangles in a size × size drawing. Again, proceed by
;; numeric recursion, give a recursive decomposition and appropriate docstring.
;;
;; (Partner A, drive for the recursive decomposition of the function!)
;;
;; To draw n levels of Sierpinski Triangles:
;; + When n = 0: do nothing
;; + When n > 0: draw triangles
;;
;; (Partner B, drive for the implementation of the function!)

(define sierpinski 
(lambda (size color n)
  (if (zero? n)
      (triangle size "solid" color)
      (let ([smt (sierpinski (/ size 2) color (- n 1))])
           (above smt
               (beside smt smt))))))
(sierpinski 600 "blue" 13)