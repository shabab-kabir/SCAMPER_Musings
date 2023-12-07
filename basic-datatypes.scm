;; CSC-151-NN (Spring 23)
;; Mini-Project 2: Working with the basic datatypes
;; Shabab Kabir
;; 2023-02-09
;; ACKNOWLEDGEMENTS:
;;   The pinecone that hit my window as I was writing this.

(import image)


;;; PART 1: String Utilities


;;; 1.)

;Expected Tests
; (slight-trim "pokemon go "); "pokemon go"
; (slight-trim " pokemon go"); "pokemon go"
; (slight-trim " pokemon go "); "pokemon go"
; (slight-trim " why hello there good sir "); "why hello there good sir"
; (slight-trim " why hello there good sir"); "why hello there good sir"
; (slight-trim " "); " "


;;; (slight-trim str) -> string?
;;;    str : string?
;;;    Returns str and removes a single leading space and a 
;;;    single trailing space on the ends of str, if they exist.

(define slight-trim
    (lambda (str)
            (cond 
                [(and (equal? (string-ref str 0)  #\space) (equal? (string-ref str (- (string-length str) 1)) #\space))
                 (substring str 1 (- (string-length str) 1))]
                [(equal? (string-ref str 0)  #\space)
                 (substring str 1 (string-length str))]
                [(equal? (string-ref str (- (string-length str) 1)) #\space)
                 (substring str 0 (- (string-length str) 1))])))

;Tests for slight-trim
(slight-trim "pokemon go "); "pokemon go"
(slight-trim " pokemon go"); "pokemon go"
(slight-trim " pokemon go "); "pokemon go"
(slight-trim " why hello there good sir "); "why hello there good sir"
(slight-trim " why hello there good sir"); "why hello there good sir"
(slight-trim " "); " "


;;; 2.)
;;; (starts-with? s1 s2) -> boolean?
;;;     s1 : string?
;;;     s2 : string?
;;;     Returns #t or #f depending if s1 starts with s2.

; Made a version that does not use Substring!
; (define starts-with?
;     (lambda (s1 s2)
;         (if (>= (string-length s1) (string-length s2))
;             (reduce (lambda (x y) (and x y)) 
;                 (list-take 
;                     (map equal? (string->list s1) 
;                                 (append  
;                                     (string->list s2)
;                                     (make-list (- (string-length s1) (string-length s2)) "a"))) 
;                 (string-length s2)))
;             #f)))

(define starts-with?
    (lambda (s1 s2) 
        (equal? 
            (substring s1 0 (string-length s2))     
            (substring s2 0 (string-length s2)))))

(starts-with? "pokemon" "poke"); #t
(starts-with? "apple" "ap"); #t
(starts-with? "ap" "app"); #f

;;; 3.)

;Expected Tests
; (ends-with? "pokemon" "mon"); #t
; (ends-with? "apple" "le"); #t
; (ends-with? "ap" "ba"); #f
; (ends-with? "" ""); #t


;;; (ends-with? s1 s2) -> boolean?
;;;     s1 : string?
;;;     s2 : string?
;;;     Returns #t or #f depending if s1 ends with s2.

(define ends-with?
    (lambda (s1 s2) 
        (equal? 
            (substring s1 (- (string-length s1) (string-length s2)) (string-length s1))     
            (substring s2 0 (string-length s2)))))

;Tests for ends-with?
(ends-with? "pokemon" "mon"); #t
(ends-with? "apple" "le"); #t
(ends-with? "ap" "ba"); #f
(ends-with? "" ""); #t


;;; 4.)

;Expected Tests
; (all-digits? "301489"); #t
; (all-digits? "3ABSss9"); #f
; (all-digits? ""); #f


;;; (all-digits? str) -> boolean?
;;;     str : string? 
;;;     Returns #t or #f depending if str contains only digits.

(define all-digits? 
    (lambda (str) 
        (if (> (string-length str) 0)
            (reduce (lambda (x y) (and x y)) 
                    (map char-numeric? (string->list str)))
            #f)))

(all-digits? "301489"); #t
(all-digits? "3ABSss9"); #f
(all-digits? ""); #f


;;; PART 2: Ehrenstein Illusions

;;; (ehrenstein length n box-color circ-color outline-color):

;; creates an image that contains a single Ehrenstein 
;; illusion with side length length, n circles, 
;; with the given box-color and circ-color for
;; the box color and circle color, respectively. 
;; outline-color determines the color of the outline 
;; of the circles and the diamond.

;;; (create-background length box-color circ-color outline-color) -> image?
;;;     length : integer?
;;;     n : integer?
;;;     box-color : string?
;;;     circ-color : string?
;;;     outline-color : string?
;;;     Returns a circle of specified length and color enclosed by a square of specified length and color.

(define create-background
    (lambda (length n box-color circ-color outline-color) 
        (overlay 
            (circle (* 0.5 length) "solid" circ-color)
            (if (= 0 n)
                (circle 0 "outline" "white")
                (circle (+ (* 0.5 length) 1) "outline" outline-color))
            (square length "solid" box-color))))

;;; (transparent-circles length outline-color) -> image?
;;;     length : integer?
;;;     outline-color : string?
;;;     Returns the outline of a circle with specified length and color.

(define transparent-circles
    (lambda (length outline-color) (circle (/ length 2) "outline" outline-color)))

;;; (concentric-circles length n outline-color) -> image?
;;;     length : integer?
;;;     n : integer?
;;;     outline-color : string?
;;;     Returns an image that contains concentric circles, equidistant to each other, with each subseuqnet circle smaller then the previous.
;;;     The number of circles contained is specefied by n, and each circle is based on the intial circle's length and color.

(define concentric-circles 
    (lambda (length n outline-color)
            (if (= 0 n) 
                (circle 0 "outline" "white")
                (apply overlay
                    (map 
                     (lambda (k) (transparent-circles (* (/ length n) k) outline-color)) 
                     (range (+ 1 n)))))))

;;; (diamond length outline-color) -> image?
;;;     length : integer?
;;;     outline-color : string?
;;;     Returns a diamond shape of specifed length and color.

(define diamond ;Rotate -> Square was not used due to bug found with overlay + rotate misalligning 
    (lambda (length outline-color) 
            (beside
                (above 
                    (path length length
                        (list (pair (/ length 2) 0) 
                              (pair length (/ length 2))
                              (pair (/ length 2) length)
                              (pair 0 (/ length 2))
                              (pair (/ length 2) 0)
                        ) "outline" outline-color))))) 

;;; (ehrenstein length n box-color circ-color outline-color) -> image?
;;;     length : integer?
;;;     n : integer?
;;;     box-color : string?
;;;     circ-color : string?
;;;     outline-color : string?
;;;     Returns ehrenstein illustion with specified size, number of circles contained, color of circle, and outline color.

(define ehrenstein 
    (lambda (length n box-color circ-color outline-color)
                (overlay
                    (diamond length outline-color)
                    (concentric-circles length n outline-color)
                    (create-background length n box-color circ-color outline-color))))

;;; (grid m n img)
;;;     m : integer?
;;;     n : integer?
;;;     img : image?
;;;     Returns an image that is a grid of m rows and n columns of the provided image img.

(define grid 
    (lambda (m n img)
            (apply beside 
                (map (lambda (y) 
                             (apply above (map (lambda (x) img) (range m)))) 
            (range n)))))


; ehrenstein-1: a single Ehrenstein illusion of length 200, 5 circles, a "red" box, "yellow" circles, and "black" outline.
(define ehrenstein-1 (ehrenstein 200 5 "red" "yellow" "black"))
ehrenstein-1

;ehrenstein-2: a single Ehrenstein illusion of length 100, 10 circles, an "aqua" box, "orange" circles, and "black" outline.
(define ehrenstein-2 (ehrenstein 100 10 "aqua" "orange" "black"))
ehrenstein-2

;ehrenstien-3: a single Ehrenstein illusion of length 50, 1 circle, a "white" box and circle, and "green" outline.
(define ehrenstein-3 (ehrenstein 50 0 "white" "white" "green"))
ehrenstein-3

;ehrenstein-4: a 3 × 3 grid of Ehrenstein illusions of length 100, 10 circles each, and a "red" box, "yellow" circle, and "orange" outline.
(define ehrenstein-4 (grid 3 3 (ehrenstein 100 10 "red" "yellow" "orange")))
ehrenstein-4

;ehrenstein-5: a 3 × 2 grid of Ehrenstein illusions of length 50, 5 circles each, and a "blue" box, "green" circles, and "white" outline.
(define ehrenstein-5 (grid 3 2 (ehrenstein 50 5 "blue" "green" "white")))
ehrenstein-5