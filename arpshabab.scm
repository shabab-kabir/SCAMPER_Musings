
;Initialization
(import music)
(import image)
(import canvas)
(import html)

;setup variables
(define Color (vector "black" "black" "black" "black" "black" "black"))
(define pos-vector (vector-range 6))
(define size 1000)
(define canv (make-canvas size size))

(define midi-to-color 
  (lambda (n)
  (match n
    [0 "black"]
    [1 "brown"]
    [2 (color 115 243 64 1)]
    [3 "red"]
    [4 "yellow"]
    [5 (color 0 255 255 1)]
    [6 (color 238 130 238 1)]
    [7 "orange"]
    [8 (color 255 0 255 1)]
    [9 (color 0 105 148 1)]
    [10 "blue"]
    [11 "green"]
    [12 "purple"])))

(define Chord-Bank
    (lambda (type)
            (match type
                ["major" (list 0 4 7 12)]
                ["minor" (list 0 3 7 12)]
                ["major7" (list 0 4 7 11 12)]
                ["minor7" (list 0 3 7 10 12)]
                ["minor7b5" (list 0 3 7 10 12)]
                ["aug" (list 0 3 7 12)]
                ["dim" (list 0 3 6 9 12)]
                ["dom7" (list 0 4 7 10 12)]
                ["major9" (list 0 4 7 11 12 14)]
                ["minor9" (list 0 3 7 10 12 14)]
                ["major11" (list 0 4 7 11 12 14 17)]
                ["minor11" (list 0 3 7 10 12 14 17)]
                ["major13" (list 0 4 7 11 12 14 17 21)]
                ["minor13" (list 0 3 7 10 12 14 17 21)])))


(define swap-color
  (lambda () 
        (begin (vector-set! Color 0 (midi-to-color (random 13)))
               (vector-set! Color 1 (midi-to-color (random 13)))
               (vector-set! Color 2 (midi-to-color (random 13)))
               (vector-set! Color 3 (midi-to-color (random 13)))
               (vector-set! Color 4 (midi-to-color (random 13)))
               (vector-set! Color 4 (midi-to-color (random 13))))))

(define change-pos
  (lambda ()
    (map (lambda (n) (vector-set! pos-vector n (random 1600))) (range 12))))

(define arp-helper
    (lambda (type) (list-ref type (random (length type)))))

(define arp
    (lambda (root dur type inst)
                (mod (instrument inst) (seq (par (note (+ root (arp-helper type)) dur) (trigger swap-color) (trigger change-pos))
                                            (par (note (+ root (arp-helper type)) dur) (trigger swap-color) (trigger change-pos))
                                            (par (note (+ root (arp-helper type)) dur) (trigger swap-color) (trigger change-pos))
                                            (par (note (+ root (arp-helper type)) dur) (trigger swap-color) (trigger change-pos))))))

(define arp-looper
    (lambda (root dur num_loop type inst)
            (if (= num_loop 0)
                (mod (instrument inst) (seq (par (note root dur)(trigger swap-color) (trigger change-pos))))
                (seq (arp root dur type inst)
                     (arp-looper root dur (- num_loop 1) type inst)))))

(define arp-looper-multi
    (lambda (note num_loop type inst)
            (par (arp-looper note en num_loop type inst)
                 (arp-looper note qn num_loop type inst)
                 (arp-looper note sn num_loop type inst)
                 (arp-looper (+ note 12) sn num_loop type inst)
                 (arp-looper note en num_loop type inst))))

(animate-with
  (lambda (time)
    (begin
      (draw-rectangle canv 0 0 size size "solid" "black")
           (begin (draw-ellipse canv (/ size 6)         (vector-ref pos-vector 0) (random 45) (random 50) (random 45) 0 360 "solid" (vector-ref Color 0))
                  (draw-ellipse canv (* 1.8 (/ size 6)) (vector-ref pos-vector 1) (random 45) (random 55) (random 45) 0 360 "solid" (vector-ref Color 1))
                  (draw-ellipse canv (* 2.8 (/ size 6)) (vector-ref pos-vector 2) (random 45) (random 65) (random 45) 0 360 "solid" (vector-ref Color 2))
                  (draw-ellipse canv (* 3.8 (/ size 6)) (vector-ref pos-vector 3) (random 45) (random 65) (random 45) 0 360 "solid" (vector-ref Color 3))
                  (draw-ellipse canv (* 4.8 (/ size 6)) (vector-ref pos-vector 4) (random 45) (random 55) (random 45) 0 360 "solid" (vector-ref Color 4))
                  (draw-ellipse canv (* 5.8 (/ size 6)) (vector-ref pos-vector 5) (random 45) (random 50) (random 45) 0 360 "solid" (vector-ref Color 5))
                  (draw-circle canv (random size) (random size) 5 "solid" "white")
                  (draw-circle canv (random size) (random size) 5 "solid" "white")))))

(tag "h1" "\n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n \n")
(tag "h1" "Space Jam (SpamX)")
(tag "h2" "By: Ashmit Bindal, Fatin Wahid, Shabab Kabir, Shibam Mukhopadhyay")
(tag "h2" "Acknowledgements: Prof. Osera")
(tag "h2" "                  Ben, the one we love, pray, and worship every moment of our miserable lives")
(tag "h2" "                  X Æ A-12")
(tag "h3" "                       Also that one random pinecone that fell on top of a child who was kicking a rock and eating salt.")
(tag "h1" "\n \n \n \n \n \n \n \n \n \n \n")
(tag "h2" "¡¡¡¡¡¡¡¡¡¡Epilepsy Warning!!!!!!!!" )
(tag "h1" "\n \n \n \n \n \n \n \n \n \n \n")

; The canvas
canv

(tag "h2" "\n")

"Arp Looper in G Minor"
(arp-looper (- 67 12) sn 400 (Chord-Bank "minor") 91)

"Arp Looper Multi in G Minor"
(arp-looper-multi (- 67 12) 8 (Chord-Bank "minor") 91)

"Arp Looper in Cmin7"
(arp-looper 60 sn 400 (Chord-Bank "minor7") 91)

"Arp Looper Multi in Cmin7"
(arp-looper-multi 60 8 (Chord-Bank "minor7") 91)

"Arp Looper in Cmin13"
(arp-looper 60 sn 400 (Chord-Bank "minor13") 91)

"Arp Looper Multi in Cmin13"
(arp-looper-multi 60 8 (Chord-Bank "minor13") 91)
