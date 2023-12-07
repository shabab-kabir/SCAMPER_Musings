(import music)

(note 78 qn)

(define div
    (lambda (d n)
            (dur (numerator d) (* (denominator d) n))))

(note 78 (div hn 2))

(define dominoes
    (lambda (freq d n)
            (if (= n 0)
                empty
                (seq (note-freq freq d)
                     (dominoes (/ freq 2) (div d 2) (- n 1))))))
(dominoes 740 qn 5)

(define raindrops 
    (lambda (lo hi d n)
            (if (= n 0)
                (seq (note-freq lo (div d 3))
                     (note-freq hi (div d 3))
                     (note-freq (/ (+ hi lo) 2) (div d 3)))
                (let* ([sum (+ hi lo)]
                       [lqrt (/ sum 4)]
                       [mid  (/ sum 2)]
                       [3qrt (+ (/ sum 3) mid)])
                       (seq (raindrops lo lqrt (div d 3) (- n 1))
                            (raindrops 3qrt hi (div d 3) (- n 1))
                            (raindrops lqrt 3qrt (div d 3) (- n 1)))))))
(raindrops 240 870 wn 4)

(define my-musical-fractal 
    (lambda (lo hi d n k)
            (let*  ([sum (+ lo hi)]
                    [avg (/ sum 2)]
                    [lqrt (/ sum 4)]
                    [mid  (/ sum 2)]
                    [3qrt (+ (/ sum 3) mid)])
                   (if (= n 0)
                       (par (note-freq avg d)
                            (note-freq lqrt (div d k))
                            (note-freq 3qrt (div d k))
                            (note-freq 3qrt (div d k)))
                       (seq (my-musical-fractal lo hi (div d 3) (- n 1) k)
                            (my-musical-fractal (/ lo 2) (/ hi 2) (div d 3) (- n 1) k))))))
(my-musical-fractal 440 770 wn 3 1)