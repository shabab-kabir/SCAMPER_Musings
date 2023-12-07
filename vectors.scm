;; CSC 151 (SEMESTER)
;; Lab: Vectors (vectors.scm)
;; Authors: Shabab Kabir, Brayden Basinger
;; Date: 4 April 2023
;; Acknowledgements:
;;   Just Ben, and that one really quirky leaf that slapped my face in the morning on my way here.

;; In today's lab, we'll explore how we can create and manipulate vectors in
;; Scheme. For each problem, alternative driver/navigator with your partner.

;; ------------------------------
"Problem: Binding and Sequencing"
;; ------------------------------

;; Whenever possible, we will want to avoid using top-level definitions in
;; favor of local bindings and parameter passing. However, it is relatively
;; easy syntactically to define a top-level vector and mutate it at the top
;; level. But doing so locally requires a particular combination of binding
;; and sequencing with the begin expression that we should get used to.

;; First, let's review how the begin expression works. Consider the following
;; code snippet that attempts to mutate the first, third, and fifth slots of
;; the vector using function chaining:

; (define example-vector (vector 0 1 2 3 4))

; (vector-set!
;   (vector-set!
;     (vector-set! example-vector 0 "zero")
;     2 "two")
;   4 "four")

;; Uncomment the code (highlight the block, then press ctrl-/) and run it.
;; You should receive an error! In a sentence or two, describe the error and
;; why it occurs:
;;
;; TODO: explain the error here: 
;; It is an error because void is returned in 
;; (vector-set! example-vector 0 "zero") and you need a vector there for the second vector-set!

;; (begin ...) allows us to execute a sequence of side effects. Now, let's
;; combine this concept with a local binding to:
;;
;; (a) Create a new vector (as a let-binding)
;; (b) Mutate the elements of the vector (using vector-set!)
;;
;; Complete the definition of the function below that creates a new vector
;; of five elements, initially the values 0 through 4. The function then
;; mutates the 1st, 3rd, and 5th elements to be "zero", "two", and "four."
;; Finally, the function returns that vector as output. (Note that the
;; value of the final expression is produced by a begin expression!)

(define make-and-mutate-vector
  (lambda (v1)
    (let*
    (
      [v1 (vector 0 1 2 3 4)]
    )
    (begin
      (vector-set! v1 0 "zero")
      (vector-set! v1 3 "two")
      (vector-set! v1 4 "four")
      v1
    ))))

(make-and-mutate-vector (vector 0 0 0 0))


;; ----------------
"Problem: Swapping"
;; ----------------

;; Complete the following function definition using vector-set! that
;; swaps the elements found at two locations in a vector. For this function,
;; make sure to check that i and j are valid indices into the vector. If
;; not, your function should raise an error with (error ...).

;; (swap! v i j) -> void?
;;   v: vector?
;;   i: integer?, a valid index into v
;;   j: integer?, a valid index into v
;; Swaps the elements at indices i and j of v.
(define swap!
  (lambda (vec i j)
          (if (and (< i (vector-length vec)) (< j (vector-length vec)))
              (let* ([ith-element (vector-ref vec i)]
                     [jth-element (vector-ref vec j)])
                    (begin
                        (vector-set! vec i jth-element)
                        (vector-set! vec j ith-element)
                    vec))
              "index error")))

(swap! (vector 1 3) 0 1)

;; --------------------------------
"Problem: Incrementing in two ways"
;; --------------------------------

;; A common "stateful" operation we might consider is incrementing the
;; value at a particular location in a vector. We may also want the value at
;; at that location either _before_ the increment occurs or _after_ the
;; increment occurs.
;;
;; Implement the following pair of functions:
;;
;; + (pre-inc vec i) takes a vector vec of numbers and a valid index i into
;;   vec as input. The function increments the ith element of vec as a side-
;;   effect and returns the value of the ith element _before_ the increment.
;; + (post-inc vec i) takes a vector vec of numbers and a valid index i into
;;   vec as input. The function increments the ith element of vec as a side-
;;   effect and returns the value of the ith element _after_ the increment.

(define dummy-vec (vector 0 1 2 3 4))
dummy-vec

(define pre-inc
  (lambda (vec i)
    (let ([x (vector-ref vec i)])
         (begin (vector-set! vec i (+ 1 (vector-ref vec i)))
                 x))))


(define post-inc
  (lambda (vec i)
    (begin (vector-set! vec i (+ 1 (vector-ref vec i)))
    (vector-ref vec i))))

;; Write test cases for your functions below. Keep in mind that pre-inc and
;; and post-inc mutate their input vectors, so the effects of previous
;; changes to a test vector will be reflected in your tests if you aren't
;; careful! We recommend using the let/begin pattern from the first problem
;; to write test-cases that are self-contained. Because test-case is a
;; statement, you will need to provide the let/begin expression as the
;; "expected" expression of the test-case.

;; TODO: write tests here!

(test-case "simple pre-inc case" equal? 2 (pre-inc dummy-vec 2))
(test-case "simple post-inc case" equal? 4 (post-inc dummy-vec 2))

;; ------------------------------
"Problem: Recursion with Vectors"
;; ------------------------------

;; A notable difference between lists and vectors is that vectors cannot be
;; easily broken up into a head and tail. Our only recourse for creating the
;; tail of a vector is to create a new vector without the head! If the
;; vector is large, then creating these vectors can become very expensive.
;; In contrast, lists are implemented behind the scenes so that we do not
;; need to create a copy of the list to access the tail---it is just available
;; to us.
;;
;; Nevertheless, we can still use recursion to traverse vectors. However,
;; instead of using structural recursion on the vector (because it is not
;; recursively defined), we'll instead use numeric recursion on the indices
;; of the vector. These indices range from 0 to (- 1 (vector-length vec)) for
;; vec.
;;
;; This "current index" will then become a parameter to our function in
;; question. However, we frequently will want to always start this index at
;; 0 or (- 1 (vector-length vec)) to scan the entire vector. Thus, our
;; recursive functions over vectors will be broken up into two functions:
;;
;; - A "helper" function that takes the current index and vector as input and
;;   actually does the recursion.
;; - A "top-level" function that takes just the vector as input and simply
;;   calls the helper with an appropriate initial index.
;;
;; Let's apply this concept to write a recursive function
;; (vector-contains vec v) that returns #t if and only if v is contained
;; somewhere inside of vec. First, implement the helper function which
;; performs numeric recursion on the index of the vector:

(define vector-contains-helper
  (lambda (vec v i)
         (cond  [(>= i (vector-length vec)) #f]
                [(equal? (vector-ref vec i) v) #t]
                [else (vector-contains-helper vec v (+ i 1))])))

;; Now, implement the top-level function that calls the helper you wrote
;; with an appropriate initial value for the index:

(define vector-contains
  (lambda (vec v)
     (vector-contains-helper vec v 0)))

;; Finally, write test cases for vector-contains below, using the local
;; vector test-case pattern with let/begin described previously.

;; TODO: write your test cases here
(test-case "simple true case for vector-contains" equal? #t (vector-contains (vector 1 2 3) 2))
(test-case "simple false case for vector-contains" equal? #f (vector-contains (vector 1 2 3) 4))
(test-case "non-numeric vector element case for vector-contains" equal? #t (vector-contains (vector "hello" 1 #f (list "a")) (list "a")))

;; -----------------------------
"Problem: More Vector Recursion"
;; -----------------------------

;; Use numeric recursion over vectors to implement the following functions.
;; Make sure to write helper functions that perform the recursion; the
;; signatures of the top-level functions are provided below. Make sure to
;; write test cases to check your work below each implementation.

; (vector-all pred vec) -> bool?
;   pred: a predicate (one-argument function) over the elements of vec
;   vec: vector?
; Returns #t if every element of vec satisifies pred.
(define vector-all-helper
  (lambda (pred vec i)
    (if (>= i (vector-length vec))
        #t
        (and (pred (vector-ref vec i))
             (vector-all-helper pred vec (+ i 1))))))

(define vector-all
  (lambda (pred vec)
    (vector-all-helper pred vec 0)))

(test-case "simple true vector-all test" equal? #t (vector-all number? (vector 1 2 3)))
(test-case "simple false vector-all test" equal? #f (vector-all string? (vector 1 2 3)))

; (vector-sum-every-n vec n) -> number?
;   vec: vector? of numbers
;   n: integer?, non-negative
; Returns the sum of every nth element of vec, starting with the first.
; For example, if n = 3, then we sum up the elements at index 0, 3, 6, 9, etc.
(define vector-sum-every-n-helper
  (lambda (vec n i)
    (if (>= i (vector-length vec))
        0
        (+ (vector-ref vec i)
           (vector-sum-every-n-helper vec n (+ i n))))))

(define vector-sum-every-n
  (lambda (vec n)
    (vector-sum-every-n-helper vec n 0)))

(test-case "simple true vector-sum-every-n test" equal? 36 (vector-sum-every-n (vector 1 2 3 4 5 6 7 8) 1))
(test-case "null vector-sum-every-n test" equal? 0 (vector-sum-every-n (vector) 1))

;; -------------------
"Problem: Mutable Map"
;; -------------------

;; (vector-map f vec) works identically to map except over vectors. It creates
;; a new vector that is the result of applying f to each element of vec. This is
;; more efficient than using a list in terms of memory usage, but still runs
;; into the issue that we are creating a new vector of the same length as the
;; old one. If we don't need the original vector, then we can save space by
;; mutating the original vector instead!
;;
;; Use this idea to implement a function (vector-map! f vec) with numeric
;; recursion that performs a mapping operation over vec with f but instead of
;; creating a new vector, it instead mutates each element of vec to be the
;; result of applying f to that element. The function does not return anything
;; as a result!

(define vector-map!-helper
  (lambda (f vec i)
    (if (< i (vector-length vec))
        (begin
          (vector-set! vec i (f (vector-ref vec i)))
          (vector-map!-helper f vec (+ i 1)))
        void)))

(define vector-map!
  (lambda (f vec)
    (vector-map!-helper f vec 0)))

(define vec (vector 1 2 3))
(vector-map! (lambda (x) (+ 1 x)) vec)
vec

(define vec (vector 1 2 3))
(vector-map! (lambda (x) (- x 4)) vec)
vec
