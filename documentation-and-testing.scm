;; CSC 151 (Spring '23)
;; Lab: Documentation and Testing (documentation-and-testing.scm)
;; Authors: Shabab Kabir, Anthony Almanza, Shibam Mukhopadhyay
;; Date: 24 February 2023
;; Acknowledgements:
;;   Just Supreme Overlord, all-mighty Ben.

(import image)
(import music)

"Problem 1: Roundtable"
"====================="

; (A side drives)

; Define a series of tests using test-case for this procedure:

;;; (range1 n) -> listof integer?
;;;   n : integer?
;;; Returns the list of numbers from 1 to n, inclusive.  If
;;; n is non-positive, then returns the empty list.
(define range1
  (lambda (n)
    (map (lambda (n) (+ n 1)) (range n))))

; We have given you one test case to get started.


(test-case "A very small range" equal? (range1 1) (list 1))
(test-case "A null range" equal? (range1) "error")
(test-case "A very big range" equal? (range1 5) (list 1 2 3 4 5))

; This is a good practice as you are developing your program so you can quickly
; know if your code meets the current set of tests.
;
; To develop the tests, you should alternate volunteering test cases which the
; driver then adds to the file. Continue identifying test cases until your
; group is satisfied with the set of tests. You should agree on when you feel
; that you have reasonably validated the function’s behavior. Make sure that
; the procedure passes all the tests that you write!

"Problem 2: Positive and Negative Cases"
"======================================"

; (B side drives)

; One way to organize our tests is by exploring positive and negative test 
; cases. A positive test case is an example that exercises when the function
; reports “yes”—e.g., returns true, computes a result—when the inputs are 
; “good”. A negative test case is an example that exercises when the function 
; reports “no”—e.g., returns false, returns an error value, does not modify the 
; input—when the inputs are “bad”. (For the time being, you can’t test for 
; error values, so stick to the other kinds of negative tests.)
;
; Follow the same process as in the prior exercise for this function:

;;; (palindrome? str) -> boolean?
;;;   str : string?
;;; Returns true iff the string s is a palindrome, i.e., str is
;;; equal to its reversal.
(define palindrome?
  (lambda (str)
    (and (string? str)
         (string=? str (list->string (reverse (string->list str)))))))

; As in the previous exercise, collaboratively develop a set of tests for this 
; procedure. For this exercise, make sure to keep in mind the idea of positive 
; and negative test cases.
;
; When you are done, the driver should make sure that the completed function
; and its test suite are in the file and then comment out the code.

(test-case "given simple palindrome" equal? (palindrome? "ana") #t)
(test-case "given numeral palindrome" equal? (palindrome? "12321") #t)
(test-case "given not string palindrome" equal? (palindrome? 123) #f)



"Problem 3: Types and Corners"
"============================"

; Another way to organize our tests is by exploring the range of possible 
; inputs. If the type of the input admits a finite set of values, we ought to 
; test all those values directly. However, if an infinite set of values is 
; possible, we need to be more judicious in what values we examine.
;
; One way to do this is to identify corner and non-corner case values. Think of 
; a corner case as an example input that exercises the “boundaries” of how the
; function ought to work. For example, if you are operating over a certain range
; of numbers, a corner case might be an input at the lower or upper end of that
; range. In contrast, the values in the middle of the range are non-corner case
; values. We expect that the function will likely operate in the same way over
; these non-corner values, so we would then surmise that we don’t have to test 
; all of these non-corner values; a few of them will suffice!
;
; Note: dedup-adjacent, below, relies on aspects of Scheme you do not yet know.
; That’s okay. You should focus on the documentation docs and the testing that
; might be appropriate given that documentation.

;;; (dedup-adjacent l) -> listof any?
;;;   l : listof any?
;;; Returns the original list l but with all duplicates found
;;; adjacent to each other removed from the list.  For example:
;;;   > (dedup-adjacent (list 3 4 7 8 1 1 0 9 9 9 6 5 5 2 4))
;;;   (list 3 4 7 8 1 0 9 6 5 2 4)
(define dedup-adjacent
  (lambda (l)
    (cond 
      [(null? l)
       null]
      [(null? (cdr l)) 
       l]
      [else
       (let ([c1 (car l)]
             [c2 (car (cdr l))]
             [rest (cdr (cdr l))])
         (if (equal? c1 c2)
             (dedup-adjacent (cons c2 rest))
             (cons c1 (dedup-adjacent (cons c2 rest)))))])))

; (A side drives)
;
; a. As in the previous exercises, collaboratively develop a set of tests for 
;    this function. For this exercise, keep in mind the idea of types and 
;    corner cases/edge cases.

(test-case "given simple list" equal? (dedup-adjacent (list 1 1 2 3)) (list 1 2 3))
(test-case "given letter list" equal? (dedup-adjacent (list "a" "a" "b" "c" "d" "d")) (list "a" "b" "c" "d"))
(test-case "given one letter list" equal? (dedup-adjacent (list "aabbccdd")) (list "aabbccdd"))

; (B side drives)
;
; b. Below is a not-quite-correct version of dedup-adjacent called 
;    bad-dedup-adjacent. Replicate your tests for dedup-adjacent for this
;    version of the function. Do your tests identify the error? If not, add more
;    tests until you find the error!

(define bad-dedup-adjacent
  (lambda (l)
    (cond
      [(null? l)
       null]
      [(null? (cdr l))
       l]
      [else
       (let ([c1 (car l)]
             [c2 (car (cdr l))]
             [rest (cdr (cdr l))])
         (if (equal? c1 c2)
             (bad-dedup-adjacent (cons c2 rest))
             (cons c1 (cons c2 (bad-dedup-adjacent rest)))))]))) 

(test-case "given simple list" equal? (bad-dedup-adjacent (list 1 1 2 3)) (list 1 2 3))
(test-case "given letter list" equal? (bad-dedup-adjacent (list "a" "a" "b" "c" "d" "d")) (list "a" "b" "c" "d"))
(test-case "given one letter list" equal? (bad-dedup-adjacent (list "aabbccdd")) (list "aabbccdd"))

"Problem 4: What's up Doc?"
"========================="

; (A and B sides switch driver-navigator roles for each function.)

; Consider each of the following function definitions that are undocumented
; and poorly named. Note that these functions use techniques and libraries that
; we may not have introduced or used yet; that is fine! The purpose of this
; exercise is to get us to think about the _contract_ of these functions rather
; than how they work precisely.

;;; (func-1 n r e g b) -> image?
;;;   n : integer?
;;;   r : integer? 
;;;   e : integer?
;;;   g : integer?
;;;   b : integer?
;;; Returns an overlay of triangles based on n and the alpha/color-dependency is given by e r g b.

(define func-1
  (lambda (n r e g b)
    (if (zero? n)
        (triangle 0 "outline" "black")
        (overlay (triangle (* n r) "outline" (color e g b 1))
                 (func-1 (- n 1) r e g b);The function will repeat until n reaches 0. (Recursion)
                 ))))

"----- func-1 tests -----"
(test-case "given n=1" equal? (func-1 1 2 3 4 5) 
                              (overlay (triangle (* 1 2) "outline" (color 3 4 5 1))
                                       (triangle 0 "outline" "black")))
                                       
;;; (func-2 n s) -> string?
;;;   n : string?
;;;   s : integer? 
;;; Returns a new string with the first n characters of s put at the end of s
(define func-2
  (lambda (s n)
    (string-append (substring s n (string-length s))
                   (substring s 0 n))))

(test-case "given simple string" equal? (func-2 "pineapple" 3) "eapplepin")
(test-case "given null string" equal? (func-2 "" 39) "")
(test-case "given big integer" equal? (func-2 "pineappleapplepen" 399) "pineappleapplepen")

"----- func-2 tests -----"

;;; (func-3 s k) -> string?
;;;   s : string?
;;;   k : char?
;;; Returns a new string with all numeric characters of s changed to k.
(define func-3
  (lambda (s k)
    (string-map (lambda (c)
                  (if (char-numeric? c) k c))
                s)))

; TODO: add func-3 test cases here:
"----- func-3 tests -----"
(test-case "given simple string" equal? (func-3 "p2ine" #\s) "psine")
(test-case "given only numeric" equal? (func-3 "234" #\s) "sss")
(test-case "given no numeric" equal? (func-3 "abc" #\s) "abc")

;;; (func-4 l) -> list?
;;;   l : list?
;;; Returns a new list where each pair in the original list is swapped in terms of position recursivlly
(define func-4
  (lambda (l)
    (if (< (length l) 2)
        l
        (let ([h1 (car l)]
              [h2 (car (cdr l))]
              [t (cdr (cdr l))])
          (cons h2 (cons h1 (func-4 t)))))))

; TODO: add func-4 test cases here:
"----- func-4 tests -----"
(test-case "given simple list" equal? (func-4 (list "potato" "juul" "s" "ss" "apple" "bob")) (list "juul" "potato" "ss" "s" "bob" "apple"))
(test-case "given repetitive list" equal? (func-4 (list "s" "ss" "s" "ss" "s" "ss")) (list "ss" "s" "ss" "s" "ss" "s"))
(test-case "given null list" equal? (func-4 (list )) (list ))

; For each function:
;
; (a) Explore how the function works by writing down 3--5 test cases
;     illustrating its behavior on a variety of inputs. You can determine
;     what types of inputs the program expects by inspecting the code or
;     by trying out different values to see if they work.
; (b) Based on your results, write down a doc comment for each function
;     as described in the reading. Make sure to include a function
;     signature describing any preconditions on the function's parameters
;     (e.g., their types) as well as postconditions on the output of
;     the function.
