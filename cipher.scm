;; CSC-151-01 (Spring '23)
;; Mini-Project 6: Cracking a Cipher
;; Shabab Kabir
;; 18 April 2023
;; ACKNOWLEDGEMENTS:
;;   That one tree that keeps up awake at night; also to the original authors of Mini-Project 6 : Cracking a Cipher.



;;; Part 1: Creating Substitution Ciphers



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

;;; (create-cipher n) -> vector?
;;;   n : integer?
;;; Returns a cipher of length n.
(define create-cipher
  (lambda (n)
    (let ([vec (vector-range 0 n)])
         (begin    
                (shuffle! vec)
                vec))))

;; Tests for create-cipher
(create-cipher 10)
(create-cipher 10)
(create-cipher 6)
(create-cipher 3)

;; Demo for create-cipher
(define test-cipher (create-cipher (length example-alphabet)))
test-cipher



;;; Part 2: Enciphering



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

;; Tests for encipher-single-char
(define example-small-alphabet (string->list "abcde"))
(define example-small-cipher (vector 4 3 1 4 0))

(test-case "Test #1 for encipher-single-char" equal? #\e (encipher-single-char #\a example-small-cipher example-small-alphabet))
(test-case "Test #2 for encipher-single-char" equal? #\b (encipher-single-char #\c example-small-cipher example-small-alphabet))
(test-case "Test #3 for encipher-single-char" equal? #\a (encipher-single-char #\e example-small-cipher example-small-alphabet))

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

;; Tests for encipher
(test-case "Simple Test #1 for encipher-single-char" equal? "bdebe" (encipher "cbaca" example-small-cipher example-small-alphabet))
(test-case "Simple Test #2 for encipher-single-char" equal? "dbaee" (encipher "bceda" example-small-cipher example-small-alphabet))
(test-case "Unknown Char Test for encipher-single-char" equal? "dush" (encipher "bush" example-small-cipher example-small-alphabet))

; Demo for encipher
(define example-text
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus sit amet congue nunc. Morbi lobortis nulla et erat suscipit dictum. Morbi vitae iaculis eros. Aliquam in maximus mi. In eu risus porttitor, imperdiet est a, suscipit massa. Cras vel dui sed nulla ultricies rutrum quis nec lacus. Mauris ullamcorper fermentum neque, quis rhoncus turpis varius et.
        Donec mollis sapien in lorem imperdiet pellentesque. Duis eu justo quis ipsum tincidunt pellentesque in vitae nibh. Sed lacus eros, facilisis a dignissim a, ornare nec odio. Interdum et malesuada fames ac ante ipsum primis in faucibus. Sed pellentesque, nisi et suscipit volutpat, odio risus aliquam purus, ac maximus lacus tortor quis ex. Sed diam nunc, fermentum ac placerat eget, sollicitudin eu purus. Interdum et malesuada fames ac ante ipsum primis in faucibus. Donec ac lacus nulla. Ut eu risus ut arcu maximus accumsan quis eu urna. Donec pulvinar semper enim eu maximus. Nullam ornare accumsan dictum. Aenean quis augue mauris. Donec dictum luctus dolor in pharetra.
        Nunc ornare est quis neque consectetur, nec aliquam erat varius. Ut in libero ornare, tempus odio eu, tempus nisl. Aenean lobortis dui dolor, nec malesuada turpis sollicitudin non. Etiam ut diam justo. Suspendisse ut ligula eget est maximus maximus. Nam finibus metus at risus luctus, a eleifend lorem iaculis. Aliquam congue eget augue non condimentum. Integer a interdum nunc, in lobortis tortor. Etiam urna felis, efficitur vel consectetur in, placerat nec nibh. Vivamus non faucibus neque, sed faucibus nunc. Vivamus at interdum mi. Pellentesque non facilisis tortor. Suspendisse convallis leo et elit suscipit, condimentum mattis felis molestie. Suspendisse ante arcu, viverra eu dignissim quis, rhoncus sit amet libero.
        Fusce ac congue nulla. Donec vulputate velit at sodales scelerisque. Integer dapibus et nunc et tempus. Nullam malesuada, dolor a hendrerit egestas, orci leo fringilla mauris, ut rhoncus ipsum massa in ante. Quisque scelerisque ultricies quam, eget iaculis eros iaculis non. Etiam ac euismod lacus, nec sodales purus. Nullam ipsum magna, lacinia non urna ac, condimentum efficitur sem. Fusce volutpat turpis sed ultrices aliquet. Vivamus pellentesque lectus ligula, eu vestibulum tortor egestas eu. Etiam nec porta odio. Cras rutrum nunc ultricies, posuere mi ut, vehicula metus. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; In hac habitasse platea dictumst. Cras facilisis nisi nibh, nec pharetra felis tempus a. Donec dictum nibh non metus auctor lobortis.
        Integer viverra nulla tellus, a viverra est luctus eu. Phasellus vel sodales velit. 
        Suspendisse viverra non diam a iaculis. Vivamus eu enim sit amet urna viverra gravida at nec dui. 
        Nam consectetur erat a tempor hendrerit. 
        Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. 
        Donec non risus urna. Ut ullamcorper at lorem sed tincidunt.")
example-text

(define ciphered-example-text (encipher example-text test-cipher example-alphabet))
ciphered-example-text



;;; Part 3: Letter Inventories and Ciphers



;;; (define process char alphabet n vec i) -> vector?
;;;   char: char?
;;;   alphabet: list?
;;;   n: integer?
;;;   vec: vector?
;;;   i: integer?
;;; Helper function that updates the inventory vector for a single character
;;; in the input string. Returns the updated inventory vector.
(define process
  (lambda (char alphabet n vec i)
    (if (< i (length alphabet))
        (begin
          (vector-set! vec i (if (equal? char (list-ref alphabet i))
                                 (+ (vector-ref vec i) 1)
                                 (vector-ref vec i)))
          (process char alphabet n vec (+ i 1)))
        vec)))

;;; (define update-inventory char alphabet n) -> vector?
;;;   char: char?
;;;   alphabet: list?
;;;   n: integer?
;;; Returns the inventory vector for a given character and alphabet. Creates a new
;;; vector and passes it along with the character, alphabet, and n to the process
;;; function, which updates the vector with the count of each letter in the input
;;; string. Returns the updated vector.
(define update-inventory
  (lambda (char alphabet n)
    (let ([vec (make-vector (length alphabet) 0)])
      (process char alphabet n vec n))))

;; Test for update-inventory
(test-case "Test for update-inventory" equal? (vector 0 0 1 0 0) (update-inventory #\c example-small-alphabet 0))

;;; (define create-inventory str alphabet) -> vector?
;;;   str: string?
;;;   alphabet: list?
;;; Returns the inventory vector for a given input string and alphabet. Creates a
;;; new vector and iterates through each character in the string, passing it along
;;; with the alphabet, 0, and the vector to the process function, which updates the
;;; vector with the count of each letter in the input string. Returns the updated
;;; vector.
(define create-inventory
  (lambda (str alphabet)
    (let ([vec (make-vector (length alphabet) 0)])
         (begin
          (for-range 0 (string-length str)
                      (lambda (i)
                              (process (string-ref str i) alphabet 0 vec 0)))
                              vec))))

;; Tests for create-inventory
(test-case "Test #1 for create-inventory" equal? (vector 1 1 1 0 2) (create-inventory "abcee" example-small-alphabet))
(test-case "Test #2 for create-inventory" equal? (vector 1 1 0 0 3) (create-inventory "eeeab" example-small-alphabet))
(test-case "Test #3 for create-inventory" equal? (vector 0 1 1 2 1) (create-inventory "bddec" example-small-alphabet))

;; Demo for create-inventory
(define example-text-inv (create-inventory example-text example-alphabet))
example-text-inv



;;; Part 4: Deciphering



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

;; Tests for find-vector-max
(test-case "Test #1 for find-vector-max" equal? 3 (find-vector-max (vector 3 0 1 9)))
(test-case "Test #2 for find-vector-max" equal? 0 (find-vector-max (vector 9)))
(test-case "Test #3 for find-vector-max" equal? 0 (find-vector-max (vector 9 0 9 0 9 0 9 0 9 0)))

;;; (reverse-cipher!-helper encoded-inv ref-inv new-cipher) -> string?
;;;   encoded-inv : vector?
;;;   ref-inv : vector?
;;;   new-cipher : vector?
;;; To create the reverse cipher for the input encoded string based on the reference string. Helper function for reverse-cipher!.
(define reverse-cipher!-helper
  (lambda (encoded-inv ref-inv new-cipher)
          (let ([max-enc (find-vector-max encoded-inv)]
                [max-ref (find-vector-max ref-inv)])
               (match (vector-ref ref-inv max-ref)
                      (-1 new-cipher)
                      (_  (begin (vector-set! new-cipher max-ref max-enc)
                                 (vector-set! encoded-inv max-enc -1)
                                 (vector-set! ref-inv max-ref -1)
                                 (reverse-cipher!-helper encoded-inv ref-inv new-cipher)))))))

;;; (reverse-cipher! encoded-inv ref-inv) -> string?
;;;   encoded-inv : vector?
;;;   ref-inv : vector?
;;; Returns the reverse cipher for the input encoded string, based on the reference string.
(define reverse-cipher!
  (lambda (encoded-inv ref-inv)
          (let ([new-cipher (make-vector (length example-alphabet) 0)])
               (reverse-cipher!-helper encoded-inv ref-inv new-cipher))))

;; Demo for reverse-cipher!
(define example-small-text "quick brown fox has something")
(define example-ref-inv (create-inventory example-small-text example-alphabet))
(define example-encoded-inv (create-inventory (encipher example-small-text test-cipher example-alphabet) example-alphabet))
example-small-text
example-ref-inv
example-encoded-inv
(reverse-cipher! example-encoded-inv example-ref-inv)

(reverse-cipher! (create-inventory ciphered-example-text example-alphabet) (create-inventory example-text example-alphabet))



;;; Part 5: Does it work?



;;; (decipher scrambled alphabet ref-inv) -> string?
;;;   scrambled : string?
;;;   alphabet : list?
;;;   ref-inv : vector?
;;; Returns the reverse cipher for the input encoded string, based on the reference string.
(define decipher 
  (lambda (scrambled alphabet ref-inv)
          (encipher scrambled (reverse-cipher! (create-inventory scrambled alphabet) ref-inv) alphabet)))

;; Demo for decipher
(test-case "Big Text Test for decipher" equal? example-text (decipher (encipher example-text test-cipher example-alphabet) example-alphabet example-text-inv))

(define three-letter-alphabet (string->list "abc"))
(define small-letter-str "aabbcaa")
(define small-cipher (create-cipher (length three-letter-alphabet)))

(test-case "Small Text Test for decipher" equal? small-letter-str (decipher (encipher small-letter-str small-cipher three-letter-alphabet) three-letter-alphabet (create-inventory small-letter-str three-letter-alphabet)))