(define example-small-alphabet (string->list "abcde"))

(define process
  (lambda (char alphabet n vec i)
    (if (< i (length alphabet))
        (begin
          (vector-set! vec i (if (equal? char (list-ref alphabet i))
                                 (+ (vector-ref vec i) 1)
                                 (vector-ref vec i)))
          (process char alphabet n vec (+ i 1)))
        vec)))

(define create-inventory
  (lambda (str alphabet)
    (let ([vec (make-vector (length alphabet) 0)])
         (begin
          (for-range 0 (string-length str)
                      (lambda (i)
                              (process (string-ref str i) alphabet 0 vec 0)))
                              vec))))

(create-inventory "abcee" example-small-alphabet)
;;; (vector-max-helper vec max n) -> any
;;;   vec : vector?
;;;   max : any
;;;   n : integer?
;;; Recursively finds the maximum value of the vector.
;;; Credit to Original Assignment Instructions & Authors for Mini-Project 6 : Cracking a Cipher
(define vector-max-helper
  (lambda (vec max n)
    (if (< n 0)
      max
      (let ([current (vector-ref vec n)])
        (if (> current max)
          (vector-max-helper vec current (- n 1))
          (vector-max-helper vec max (- n 1)))))))

;;; (vector-max vec) -> any
;;;   vec : vector?
;;; Returns the maximum value in the vector.
;;; Credit to Original Assignment Instructions & Authors for Mini-Project 6 : Cracking a Cipher
(define vector-max
  (lambda (vec)
    (vector-max-helper vec (vector-ref vec (- (vector-length vec) 1)) (- (vector-length vec) 1))))

;;; (vector-index-of-helper vec val n) -> integer?
;;;   vec : vector?
;;;   val : any
;;;   n : integer?
;;; Recursively determines the first index of val in vec, or returns -1 if it is not present.
;;; Credit to Original Assignment Instructions & Authors for Mini-Project 6 : Cracking a Cipher
(define vector-index-of-helper
  (lambda (vec val n)
    (if (> n (- (vector-length vec) 1))
      -1
      (if (equal? (vector-ref vec n) val)
        n
        (vector-index-of-helper vec val (+ n 1))))))

;;; (vector-index-of vec val) -> integer?
;;;   vec : vector?
;;;   val : any
;;; Returns the index of the first occurrence of val in vec or -1 if val is not in vec.
;;; Credit to Original Assignment Instructions & Authors for Mini-Project 6 : Cracking a Cipher
(define vector-index-of
  (lambda (vec val)
    (vector-index-of-helper vec val 0)))

;;; (find-vector-max vec) -> integer?
;;;   vec: vector?
;;; Returns the index of the maximum value in the input vector.
(define find-vector-max 
  (lambda (vec)
          (vector-index-of vec (vector-max vec))))

(define example-alphabet 
    (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ "))

;;; (list-contains lst val) -> boolean?
;;;   lst : list?
;;;   val : any
;;; Returns #t if and only if the list contains the value.
;;; Credit to Original Assignment Instructions & Authors for Mini-Project 6 : Cracking a Cipher
(define list-contains?
  (lambda (lst val)
    (match lst
      [null #f]
      [(cons head tail) (or (equal? head val) (list-contains? tail val))])))

;;; (encipher-single-char ch cipher alphabet) -> char
;;;   ch : char?
;;;   cipher : vector?
;;;   alphabet : list?
;;; Returns the enciphered character for the input character ch.
(define encipher-single-char 
  (lambda (ch cipher alphabet)
    (if (list-contains? alphabet ch)
        (list-ref alphabet (vector-ref cipher (index-of alphabet ch)))
        ch)))

;;; (encipher str cipher alphabet) -> string?
;;;   str : string?
;;;   cipher : vector?
;;;   alphabet : list?
;;; Returns the enciphered string for the input string str.
(define encipher
  (lambda (str cipher alphabet)
    (list->string
      (map (lambda (ch) (encipher-single-char ch cipher alphabet))
           (string->list str)))))

;;; (example-alphabet) -> string?
;;; Returns a string of all lowercase and uppercase English letters, as well as a space character.
;;; Credit to Original Assignment Instructions & Authors for Mini-Project 6 : Cracking a Cipher
(define example-alphabet 
    (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ "))
example-alphabet 

;;; (swap! vec i j) -> void
;;;   vec : vector?
;;;   i : integer?
;;;   j : integer?
;;; Swaps the ith and jth entries in the provided vector vec.
;;; Credit to Original Assignment Instructions & Authors for Mini-Project 6 : Cracking a Cipher
(define swap!
  (lambda (vec i j)
    (let ([temp (vector-ref vec i)])
      (begin
        (vector-set! vec i (vector-ref vec j))
        (vector-set! vec j temp)))))


;;; (shuffle-helper! vec n) -> void
;;;   vec : vector?
;;;   n : integer?
;;; Recursively selects the next random element and swaps it into location n.
;;; Credit to Original Assignment Instructions & Authors for Mini-Project 6 : Cracking a Cipher
(define shuffle-helper!
  (lambda (vec n)
    (if (zero? n)
      void
      (begin
        (swap! vec (random (+ n 1)) n)
        (shuffle-helper! vec (- n 1))
        void))))

;;; (shuffle! vec) -> void
;;;   vec : vector?
;;; Randomly shuffles the elements of the input vector.
;;; Credit to Original Assignment Instructions & Authors for Mini-Project 6 : Cracking a Cipher
(define shuffle!
  (lambda (vec)
    (shuffle-helper! vec ( - (vector-length vec) 1))))

(define create-cipher
  (lambda (n)
    (let ([vec (make-vector n 0)])
         (begin    
                (vector-map! (lambda (k) (random (- n 1))) vec)
                (shuffle! vec)
                (shuffle! vec) ; double shuffling!
                vec))))

(define example-text
        "Lorem ipsum dolor sit amet")
example-text


(define test-cipher (create-cipher (length example-alphabet)))
test-cipher

(define ciphered-example-text (encipher example-text test-cipher example-alphabet))
ciphered-example-text          

(define reverse-cipher-helper 
  (lambda (encoded-list ref-list result)
    (cond
      [(null? encoded-list) (list->string result)]
      [else
        (let* ([max-encoded (find-vector-max (create-inventory (list->string encoded-list) (list->string example-alphabet)))]
               [max-ref (find-vector-max (create-inventory ref-list example-alphabet))]
               [j-encoded (vector-ref max-encoded 0)]
               [j-ref (vector-ref max-ref 0)])
          (reverse-cipher-helper (cdr encoded-list) (vector-set! ref-list j-ref -1) 
                                 (append result (list j-ref))))])))

(define reverse-cipher! 
  (lambda (encoded-inv ref-inv)
    (if (string? ref-inv)
        (let ([encoded-list (string->list encoded-inv)]
              [ref-list (make-vector (string-length ref-inv) 0)])
          (reverse-cipher-helper encoded-list ref-list (list)))
        (error "Invalid input: ref-inv must be a string"))))

(reverse-cipher! ciphered-example-text example-text)