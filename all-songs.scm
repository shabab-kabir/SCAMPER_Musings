;; CSC-151-01 (Spring '23)
;; Mini-Project 5: All Songs
;; Shabab Kabir
;; Date: 08 March 2023
;; ACKNOWLEDGEMENTS:
;;   The lightbulb that slapped my face the other day.

(import music)
;;; (all-list2 v l) -> list?
;;; v : any?
;;; l : list?
;;; Returns a list of lists where v is appened onto each element of l individually as a new list
(define all-list2 
    (lambda (v l)
            (match l
                    [null null]
                    [(cons head tail)
                     (cons (list v head)
                           (all-list2 v tail))])))
(test-case "string all-list2 test" equal? (list (list "a" 0) (list "a" 1) (list "a" 2) (list "a" 3) (list "a" 4)) (all-list2 "a" (range 5)))
(test-case "char all-list2 test" equal? (list (list #\a 0) (list #\a 1) (list #\a 2) (list #\a 3) (list #\a 4)) (all-list2 #\a (range 5)))
(test-case "null all-list2 test" equal? null (all-list2 3 null))

;;; (cartesian-product l1 l2) -> list?
;;; l1 : list?
;;; l2 : list?
;;; Returns a new list where each element of the new list is a list of two elements each containing an element from l1 and l2 respectivally
(define cartesian-product 
    (lambda (l1 l2)
        (cond ((or (null? l1) (null? l2)) null)
              (else (append (all-list2 (car l1) l2)
                            (cartesian-product (cdr l1) l2))))))
(test-case "simple cartesian-product test" equal? (list (list 0 0) (list 0 1) (list 0 2) (list 0 3) (list 0 4) (list 1 0) (list 1 1) (list 1 2) (list 1 3) (list 1 4) (list 2 0) (list 2 1) (list 2 2) (list 2 3) (list 2 4)) (cartesian-product (range 3) (range 5)))
(test-case "non-numerical cartesian-product test" equal? (list (list "a" "d") (list "a" "e") (list "a" "f") (list "b" "d") (list "b" "e") (list "b" "f") (list "c" "d") (list "c" "e") (list "c" "f")) (cartesian-product (list "a" "b" "c") (list "d" "e" "f")))
(test-case "null cartesian-product test" equal? null (cartesian-product null null))

;;; (all-two-note-songs notes) -> list? (of compositions)
;;; notes : integer? (must be a midi value)
;;; Returns a list of all possible two note combinations of notes
(define all-two-note-songs 
    (lambda (notes)
            (map (lambda (pair) (seq (note (list-ref pair 0) qn) 
                                     (note (list-ref pair 1) qn)))
                 (cartesian-product notes notes))))

;;; two-note-example -> composition?
;;; Returns all two note compositions that are possible to made up from the midi values of 60, 69, and 64
(define two-note-example 
    (all-two-note-songs (list 60 69 64)))
two-note-example

;;; (cons-all x l) -> list?
;;; x : any?
;;; l : list? (list of lists)
;;; Returns a new list with each element-list now containing x as its own first internal element
(define cons-all 
    (lambda (x l)
        (cond ((null? l) null)
              ((pair? (car l)) (cons (cons x (car l)) (cons-all x (cdr l))))
              (else (cons (cons x (list (car l))) (cons-all x (cdr l)))))))

(test-case "simple cons-all test" equal? (list (list 0 1 2) (list 0 3 4 5) (list 0 6 7)) (cons-all 0 (list (list 1 2) (list 3 4 5) (list 6 7))))
(test-case "mixed value cons-all test" equal? (list (list 9 "a" "b" "c") (list 9 2 8 9)) (cons-all 9 (list (list "a" "b" "c") (list 2 8 9))))
(test-case "null cons-all test" equal? null (cons-all 0 null))

;;; (delete-1st-kind x l) -> list?
;;; x : any?
;;; l : list?
;;; Returns a new list without x
(define delete-1st-kind
  (lambda (x lst)
    (match lst
      [null null]
      [(cons head tail)
       (if (equal? head x)
           tail
           (cons head (delete-1st-kind x tail)))])))

;;; (combinations l) -> list?
;;; l : list? (list of lists)
;;; Returns a list of lists of all possible combinations of each element of l
(define combinations
  (lambda (l)
    (if (null? l)
        (list null)
        (let ((first-list (car l))
              (rest (combinations (cdr l))))
          (map (lambda (x) (delete-1st-kind null x))
               (apply append (map (lambda (x) (cons-all x rest)) first-list)))))))

(test-case "simple combinations test" equal? (list (list 1 3 6) (list 1 3 7) (list 1 4 6) (list 1 4 7) (list 1 5 6) (list 1 5 7) (list 2 3 6) (list 2 3 7) (list 2 4 6) (list 2 4 7) (list 2 5 6) (list 2 5 7)) (combinations (list (list 1 2) (list 3 4 5) (list 6 7))))
(test-case "mixed values combinations test" equal? (list (list 5 "a") (list 5 "b") (list 5 9) (list #\a "a") (list #\a "b") (list #\a 9) (list "9" "a") (list "9" "b") (list "9" 9)) (combinations (list (list 5 #\a "9") (list "a" "b" 9))))
(test-case "null combinations test" equal? (list null) (combinations null))

;;; (all-songs notes n) -> list? (list of compositions)
;;; notes : list? (must be midi values)
;;; n : integer?
;;; Returns a n-note long compositions that are possible to made out of the notes from l
(define all-songs 
    (lambda (n notes)
            (let* ([total (map (lambda (x) (map (lambda (y) (note y qn)) x)) 
                         (combinations (make-list notes n)))])
                 (map (lambda (x) (apply seq x)) total))))

;;; five-note-example -> list? (list of compositions)
;;; Returns all posible five note compositions that are qn lengthed by the following midi values: 60, 58, and 65
(define five-note-example
    (all-songs (list 60 58 65) 5))
five-note-example

;;; Reflection:
;;; Do you think music should still be valued in light of modern-day computation’s ability to “do it all?” 
;;; If so, what do you personally value about music in spite of this assignment? If not, why do you feel that music has lost its value?

 
; While modern-day computation has undoubtedly revolutionized the way music is created and consumed, I personally believe that music still holds immense value in our lives. 
; Advancements in technology have certainly expanded the possibilities for a variety of musical creations, but they have not diminished the importance of the human touch 
; and emotional connection that is inherent within music. The evolution of music through technology demonstrates our never-ending quest for expression and creativity.

; Music has been an integral part of human culture and society for thousands of years. Its ability to transcend 
; language barriers, evoke emotions, and foster connections between various people is unparalleled. Despite technological advancements, the emotional and cultural 
; significance of music remains. The human experience is intricately woven into the fabric of music, and no computational algorithm can fully replicate 
; the depth and nuance of the human expression (espically since CHAT-GPT makes pretty shitty music).

; Moreover, technology has simply democratized music creation, allowing for people from various backgrounds and skill levels to create and share their art. 
; This has resulted in a diverse array of musical styles and genres, espically in the electronic world, that enrich our lives and expand our understanding of what music can be. 
; Computation has made it easier for artists to experiment with new sounds and techniques, pushing the boundaries of traditional musical structures and fostering innovation.
; Nowadays, most coposers rely heavilly on notational softwares or digital audio work stations for their art.

; Additionally, the human element in music cannot be ignored. While computational algorithms can create impressive and complex compositions, 
; they often lack the emotional resonance that comes from a human composer or performer. An explicit example is human variability. Music is not merely an assembly of notes and rhythms, 
; but rather an expression of the human spirit. The stories, experiences, and emotions that musicians bring to their work contribute to the unique 
; and irreplaceable value of music.

; In conclusion, although modern-day computation has undoubtedly impacted the way we create and consume music, it has not diminished its value. 
; Instead, technology has expanded the possibilities for musical expression and allowed for a greater diversity of voices and styles. The emotional and cultural 
; significance of music remains undiminished, as it continues to provide a unique connection between people and an outlet for human creativity. 
; Music will always be valued, as long as it continues to resonate with the human experience and serve as a conduit for our shared emotions and expressions.