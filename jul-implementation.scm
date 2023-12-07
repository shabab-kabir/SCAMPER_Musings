(import music)

(define harry-potter (file->string "harry-potter.txt"))

(define string-split-vector
  (lambda (str)
    (|> str
      (lambda (str)
        (string-map (lambda (ch) (if (or (char-alphabetic? ch) (char-ci=? #\' ch)) ch #\space)) str))
      (lambda (str)
        (string-split str " "))
      (lambda (strlist)
        (filter (lambda (str) (> (string-length str) 0)) strlist)))))

(define words-to-length
  (lambda (vec)
    (map
      string-length
      vec)))


(define word-lengths (words-to-length (string-split-vector harry-potter)))

(define occurences (make-vector (reduce max word-lengths) 0))

(define count-repetitions
  (begin
    (map
      (lambda (w)
        (vector-set! occurences (- w 1) (+ 1 (vector-ref occurences (- w 1)))))
      word-lengths)
    (map
      (lambda (r) (cons (+ r 1) (vector-ref occurences r)))
      (range (vector-length occurences)))))


; (define length-occurences
  ; (lambda (vec)
    ; ())
  ; count-repetitions
  
  (define list-split
    (lambda (l n)
      (if (zero? n)
        (pair null l)
        (match l
          [null (error "list-split: index out of bound")]
          [(cons head tail)
            (let ([result (list-split tail (- n 1))])
              (pair (cons head (car result)) (cdr result)))]))))
  
  (define list-split-half
    (lambda (l)
      (list-split l (floor (/ (length l) 2)))))
  
  (define merge-helper
    (lambda (l1 l2 reverse-sorted)
      (match (pair l1 l2)
        [(pair null _) (append (reverse reverse-sorted) l2)]
        [(pair _ null) (append (reverse reverse-sorted) l1)]
        [(pair (cons head1 tail1)
            (cons head2 tail2))
          (if (>= (cdr head1) (cdr head2))
            (merge-helper tail1 l2 (cons head1 reverse-sorted))
            (merge-helper l1 tail2 (cons head2 reverse-sorted)))])))
  
  (define merge
    (lambda (l1 l2)
      (merge-helper l1 l2 null)))
  
  
  (define merge-sort
    (lambda (l)
      (match l
        [null l] ; the empty list case
        [(cons _ null) l] ; the one-element list case
        [_ (let* ([halves (list-split-half l)]
              [l1 (merge-sort (car halves))]
              [l2 (merge-sort (cdr halves))])
            (merge l1 l2))])))
  
  (define sorted-repetitions (merge-sort count-repetitions))
  
  sorted-repetitions
  
  (define base-helper
    (lambda (base10 n)
      (match base10
        [0 (list)]
        [_
          (cons
            (modulo base10 n)
            (base-helper (quotient base10 n) n))])))
  
  (define base
    (lambda (base10 n)
      (match (reverse (base-helper base10 n))
        [null (list 0)]
        [lst lst])))
  
  (define remove-last
    (lambda (lst)
      (match lst
        [null null]
        [(cons last null) null]
        [(cons head tail) (cons head (remove-last tail))])))
  
  (define get-last
    (lambda (lst)
      (match lst
        [null null]
        [(cons last null) last]
        [(cons _ tail) (get-last tail)])))
  
  (define first-middle-last
    (lambda (lst)
      (match lst
        [(cons head tail) (list head (remove-last tail) (get-last tail))])))
  
  (define degree->offset
    (lambda (scale degree)
      (list-ref scale (- degree 1))))
  
  (define offset-root
    (lambda (root scale offset)
      (+ root (degree->offset scale (+ offset 1)))))
  
  (define offset-pitch
    (lambda (root scale v)
      (if (list? v)
        (map (lambda (u) (offset-pitch root scale u)) v)
        (offset-root root scale v))))
  
  ; TO-DO: make duration modulo editable
  (define chord
    (lambda (kv-pair root scale old-dur)
      (let* ([new-dur (dur (* (+ (remainder (- (car kv-pair) 1) 8) 1) (numerator old-dur)) (denominator old-dur))]
          [s-at-b (map (lambda (v) (offset-pitch root scale v)) (first-middle-last (base (cdr kv-pair) (length scale))))]
          [soprano (+ (list-ref s-at-b 0) 12)]
          [middle (list-ref s-at-b 1)]
          [bass-nullable (list-ref s-at-b 2)]
          [bass (if (null? bass-nullable) null (- bass-nullable 12))]
          [safe-note (lambda (midi dur) (if (null? midi) empty (note midi dur)))])
        
        (par (note soprano new-dur)
          (apply par (map (lambda (midi) (safe-note midi new-dur)) middle))
          (safe-note bass new-dur)))))
  
  (define create-chords-helper
    (lambda (assoc-lst n root scale old-dur)
      (if (<= n 0)
        null
        (cons
          (chord (list-ref assoc-lst (- n 1)) root scale old-dur)
          (create-chords-helper assoc-lst (- n 1) root scale old-dur)))))
  
  (define create-chords
    (lambda (assoc-lst n root scale old-dur)
      (reverse (create-chords-helper assoc-lst n root scale old-dur))))
  
  (define chromatic (range 12))
  (define major-diatonic (list 0 2 4 5 7 9 11))
  (define minor-diatonic (list 0 2 3 5 7 8 10))
  (define major-pentatonic (list 0 2 4 7 9))
  
  (define implementation-helper
    (lambda (assoc-lst amt-chords root scale base-dur)
      (apply seq (create-chords assoc-lst amt-chords root scale base-dur))))
  
  (define implementation
    (lambda (assoc-lst)
      (implementation-helper assoc-lst (length assoc-lst) 60 major-diatonic en)))
  
  
  ; harry potter - chamber of secrets
  (repeat 1 (implementation sorted-repetitions))