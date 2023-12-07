; (define stuff (list (pair "peas" 7) (pair "peanuts" 8) (pair "highlighters" 10)))

; (define material-greater-7?
;     (lambda (material l)
;             (if (assoc-key? material l)
;                 (> (assoc-ref material l) 7)
;                 "Material Not in List")))

; (material-greater-7? "peas" stuff)
; (material-greater-7? "peanuts" stuff)
; (material-greater-7? "highlighters" stuff)

; stuff
; (define increment-material
;     (lambda (material l value)
;             (if (assoc-key? material l)
;                 (assoc-set material (+ (assoc-ref material l) value) l)
;                 "Material Not in List")))

; (increment-material "peanuts" stuff 20)

; (struct leaf ())

; (struct node (value left right))

; (define singleton
;   (lambda (value)
;     (node value (leaf) (leaf))))

; (define example-bst
;   (node 4
;     (node 2
;       (singleton 1)
;       (singleton 3))
;     (node 6
;       (singleton 5)
;       (singleton 7))))

; (define example-tree
;   (node 5
;         (node 3
;               (node 1 (leaf) (leaf))
;               (node 4 (leaf) (leaf)))
;         (node 7
;               (leaf)
;               (node 9
;                     (node 8 (leaf) (leaf))
;                     (leaf)))))

; (define bst-helper
;     (lambda (t min max)
;             (match t
;                 [(leaf) #t]
;                 [(node value left right) (and (<= value max) 
;                                               (>= value min) 
;                                               (bst-helper left min (- value 1)) 
;                                               (bst-helper right (+ value 1) max))])))

; (define bst?
;     (lambda (t)
;             (bst-helper t -100 100)))

; (bst? example-bst)

; (define tree-size
;     (lambda (t)
;             (match t
;                   [(leaf) 0]
;                   [(node value left right) (+ 1 (tree-size left) (tree-size right))])))

; (tree-size example-bst)
; (tree-size example-tree)

; (define tree-depth
;     (lambda (t)
;             (match t
;                   [(leaf) 0]
;                   [(node value left right) (+ 1 (max (tree-depth left) (tree-depth right)))])))

; (tree-depth example-bst)
; (tree-depth example-tree)

; (define example-tree2
;   (node 5
;         (node 3
;               (node 1 (leaf) (leaf))
;               (node 4 (leaf) (leaf)))
;         (node 7
;               (leaf)
;               (node 9
;                     (node 8 (leaf) (leaf))
;                     (leaf)))))

; (define tree-sum
;     (lambda (t)
;             (match t
;                   [(leaf) 0]
;                   [(node value left right) (+ value (tree-sum left) (tree-sum right))])))

; (tree-sum example-bst)
; (tree-sum example-tree2)

; (define tree-largest
;     (lambda (t)
;             (match t
;                   [(node value (leaf) (leaf)) value]
;                   [(node value left right) (max value (tree-largest left) (tree-largest right))])))

; (define tree-largest
;   (lambda (t)
;     (match t
;       [(leaf) 0]
;       [(node v (leaf) (leaf)) v]
;       [(node v l r) (max v (tree-largest l)
;                            (tree-largest r))])))

; (tree-largest example-bst)
; (tree-largest example-tree2)

; (define vec-copy
;     (lambda (vec)
;             (let ([copy (make-vector (vector-length vec) 0)])
;                  (begin
;                        (for-range 0 (vector-length vec)
;                                     (lambda (i)
;                                             (vector-set! copy i (vector-ref vec i))))
;                        copy))))

; (define dummy-vec (vector 1 2 3))
; (vector-length dummy-vec)
; dummy-vec
; (vec-copy dummy-vec)
; (begin (vector-fill! dummy-vec 1) (vector-map! (lambda (k) (+ k 1)) dummy-vec) dummy-vec)

(define flights
    (list (pair (pair "American" 340) 261)
          (pair (pair "Delta" 5370) 340)
          (pair (pair "Delta" 8992) 200)
          (pair (pair "United" 2408) 100)
          (pair (pair "Delta" 8902) 987)
          (pair (pair "United" 4676) 286)))

(define min-flight-Delta
    (lambda (flights)
            (reduce (lambda (x y) (min (if (pair? x) (cdr x) x) 
                                       (if (pair? y) (cdr y) y))) 
                    (filter (lambda (x) (equal? "Delta" (car (car x)))) flights))))

(min-flight-Delta flights)