;; CSC-151-01 (Spring '23)
;; Mini-Project 5: Visual and Musical Fractals
;; Shabab Kabir
;; 2023-03-15
;; ACKNOWLEDGEMENTS:
;;   The super random snow I see here every so often that stares into my soul.

;;; Part 1: Image Fractals

(import image)

;;; (cantor-set width height fill color n) -> image?
;;; width : number?
;;; height : number? 
;;; fill : string? [either "solid" or "outline"]
;;; color : string? [either a color name or the form "rgba(r, g, b, a)"]
;;; n : integer?
;;; Returns an image of a cantor set with the aforementioned parameters.

(define cantor-set
    (lambda (width height fill color n)
        (match n
            [0 "Not sure what you are expecting here, but alright."]
            [1 (rectangle width height fill color)]
            [_ (let ([t (cantor-set (/ width 3) height fill color (- n 1))])
                        (above (cantor-set width height fill color 1)
                               (rectangle width height fill "white")
                        (beside t (rectangle (/ width 3) height fill "white") t)))])))

(cantor-set 100 10 "solid" "green" 1)
(cantor-set 100 10 "outline" "blue" 2)
(cantor-set 100 10 "outline" "purple" 3)
(cantor-set 100 10 "solid" "black" 10)

;;; (serpinski-carpet size fill color n) -> image?
;;; size : number?
;;; fill : string? [either "solid" or "outline"]
;;; color : string? [either a color name or the form "rgba(r, g, b, a)"]
;;; n : integer?
;;; Returns an image of a serpinski carpet with the aforementioned parameters.

(define serpinski-carpet
  (lambda (size fill color n)
    (if (= n 0)
        (rectangle size size fill "white")
        (let* ([subimage   (serpinski-carpet (/ size 3) fill color (- n 1))]
               [center     (rectangle (/ size 3) (/ size 3) fill color)]
               [top-row    (beside subimage subimage subimage)]
               [middle-row (beside subimage center subimage)])
              (above top-row middle-row top-row)))))

(serpinski-carpet 100 "solid" "blue" 1)
(serpinski-carpet 100 "outline" "green" 2)
(serpinski-carpet 100 "solid" "black" 3)
(serpinski-carpet 300 "solid" "purple" 5)

(color 255 255 255 1)

;;; (random-color n) -> string?
;;; n : number?
;;; Returns a random color. Note that the n does not affect the final result.
;;; However it is needed to ensure that each recursive call uses a different random color.

(define random-color
    (lambda (n)
    (color (random 255) (random 255) (random 255) 1)))

;;; (my-fractal size fill color n) -> image?
;;; width : number?
;;; height : number? 
;;; fill : string? [either "solid" or "outline"]
;;; color : string? [either a color name or the form "rgba(r, g, b, a)"]
;;; n : integer?
;;; Returns an image of a unique and complicated fractal of my own creation.

(define my-fractal
    (lambda (width height fill color n)
            (let* ([color2 (random-color (- n 1))])
            (if (= n 0)
                (rectangle width height fill color)
                (above (beside (rotate 180 (above
                                            (triangle (/ width 2) fill color2)
                                            (serpinski-carpet (/ width 3) fill color2 (- n 1))
                                            (my-fractal (/ width 2) height fill color2 (- n 1))))
                        (rotate 90 (above
                                            (triangle (/ width 2) fill color2)
                                            (serpinski-carpet (/ width 3) fill color2 (- n 1))
                                            (my-fractal (/ width 2) height fill color2 (- n 1))
                       (beside (rotate 180 (above
                                            (triangle (/ width 2) fill color2)
                                            (serpinski-carpet (/ width 3) fill color2 (- n 1))
                                            (my-fractal (/ width 2) height fill color2 (- n 1))))
                        (rotate 180 (above
                                            (triangle (/ width 2) fill color2)
                                            (serpinski-carpet (/ width 3) fill color2 (- n 1))
                                            (my-fractal (/ width 2) height fill color2 (- n 1)))))))))))))

(my-fractal 100 10 "solid" "blue" 6)

;;; Part 2: Music Fractals

(import music)

;;; (div d n) -> dur?
;;; d : dur?
;;; n : integer?
;;; Returns a new dur that is 1/n * old dur.

(define div
    (lambda (d n)
            (dur (numerator d) (* (denominator d) n))))

;;; (dominoes freq d n) -> composition?
;;; freq : integer? [0 <= frequency <= 4000]
;;; d : dur?
;;; n : integer? [Used for recursive call]
;;; Returns a musical dominoe fractal using given parameters.

(define dominoes
    (lambda (freq d n)
            (if (= n 0)
                empty
                (seq (note-freq freq d)
                     (dominoes (/ freq 2) (div d 2) (- n 1))))))
(dominoes 740 qn 5)

;;; (raindrops lo hi d n) -> composition?
;;; lo : integer? [0 <= frequency <= 4000]
;;; hi : integer? [0 <= frequency <= 4000]
;;; d : dur?
;;; n : integer? [Used for recursive call]
;;; Returns a musical raindrop fractal using given parameters.

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
(raindrops 240 870 wn 9)

;;; (my-musical-fractal d n k bool) -> composition?
;;; lo : integer? [0 <= frequency <= 4000]
;;; hi : integer? [0 <= frequency <= 4000]
;;; d : dur?
;;; n : integer? [Used for recursive call]
;;; k : integer? [Used for dividing d into small dur]
;;; bool : boolean? [Used to include the effects of Dominoes]
;;; Returns a unique musical fractal using given parameters of my own creation.

(define my-musical-fractal 
    (lambda (lo hi d n k bool)
            (let*  ([sum (+ lo hi)]
                    [avg (/ sum 2)]
                    [lqrt (/ sum 4)]
                    [mid  (/ sum 2)]
                    [3qrt (+ (/ sum 3) mid)])
                   (if (= n 0)
                       (par (note-freq avg d)
                            (note-freq lqrt (div d k))
                            (seq (note-freq 3qrt (div d k))
                                 (if bool
                                     (dominoes (- mid 1) (div d k) 4)
                                     empty))
                            (note-freq 3qrt (div d k)))
                       (seq (my-musical-fractal lo hi (div d 3) (- n 1) k bool)
                            (raindrops 3qrt hi (div d 3) (- n 1))
                            (my-musical-fractal (/ lo 2) (/ hi 2) (div d 3) (- n 1) k bool)
                            (raindrops lo lqrt (div d 3) (- n 1)))))))

(my-musical-fractal 440 770 wn 3 1 #f)
(my-musical-fractal 440 770 wn 3 1 #t)