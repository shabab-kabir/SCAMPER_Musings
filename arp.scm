(import music)

(define Chord-Bank
    (lambda (type)
            (match type
                ["major" (list 0 4 7 12)]
                ["minor" (list 0 3 7 12)]
                ["major7" (list 0 4 7 11 12)]
                ["minor7" (list 0 3 7 10 12)]
                ["minor7b5" (list 0 3 7 10 12)]
                ["dim" (list 0 3 6 9 12)]
                ["dom7" (list 0 4 7 10 12)]
                ["major9" (list 0 4 7 11 12 14)]
                ["minor9" (list 0 3 7 10 12 14)]
                ["major11" (list 0 4 7 11 12 14 17)]
                ["minor11" (list 0 3 7 10 12 14 17)]
                ["major13" (list 0 4 7 11 12 14 17 21)]
                ["minor13" (list 0 3 7 10 12 14 17 21)])))

(define arp-helper
    (lambda (type) (list-ref type (random (length type)))))

(define arp
    (lambda (root dur type inst)
                (mod (instrument inst) (seq (note (+ root (arp-helper type)) dur)
                                            (note (+ root (arp-helper type)) dur)
                                            (note (+ root (arp-helper type)) dur)
                                            (note (+ root (arp-helper type)) dur)))))

(define arp-looper
    (lambda (root dur num_loop type inst)
            (if (= num_loop 0)
                (mod (instrument inst) (seq (note root dur)))
                (seq (arp root dur type inst)
                     (arp-looper root dur (- num_loop 1) type inst)))))

(define arp-looper-multi
    (lambda (note num_loop type inst)
            (par (arp-looper note en num_loop type inst)
                 (arp-looper note qn num_loop type inst)
                 (arp-looper note sn num_loop type inst)
                 (arp-looper (+ note 12) sn num_loop type inst)
                 (arp-looper note en num_loop type inst))))

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
