;; CSC-151-NN (Spring '23)
;; Mini-Project 3: Beat Machine
;; Shabab Kabir
;; 2023-02-22
;; ACKNOWLEDGEMENTS:
;;   The ice that keeps making me trip on my way to the Bucksbaum.

(import music)

;;; PART 1:

;;; (pnote midi dur) -> composition?
;;; midi : integer?
;;; dur : dur?
;;; Returns the midi percussion note with given dur
(define pnote
    (lambda (midi dur)
            (mod percussion (note midi dur))))

;;; (accent midi dur) -> composition?
;;; midi : integer?
;;; dur : dur?
;;; Returns the midi percussion note with given dur, except accented
(define accent
    (lambda (midi dur)
            (mod percussion
            (mod (dynamics 125) (note midi dur)))))

(pnote 38 qn)
(accent 38 qn)

;;; (ghost midi dur) -> composition?
;;; midi : integer?
;;; dur : dur?
;;; Returns the midi percussion ghost note with given dur
(define ghost
    (lambda (midi dur)
            (mod percussion 
                (mod (dynamics 20) (note midi dur)))))

(pnote 38 qn)
(ghost 38 qn)

;;; strokes -> composition?
;;; Returns a basic stroke rythm
(define strokes
    (seq (accent 38 qn)
         (ghost 38 qn)
         (ghost 38 qn)
         (pnote 38 qn)
         (accent 38 qn)
         (ghost 38 qn)
         (ghost 38 qn)
         (pnote 38 qn)))

strokes

;;; (tremelo slashes midi dur2) -> composition?
;;; slahes : integer?
;;; midi : integer?
;;; dur2 : dur?
;;; Returns the midi percussion note with given dur, 
;;;         except played as a tremelo of given slash amount
(define tremolo
    (lambda (slashes midi dur2)
            (repeat (expt 2 slashes) 
                (mod percussion 
                    (note midi 
                        (dur (numerator dur2) 
                             (* (denominator dur2) 
                                (expt 2 slashes))))))))

(tremolo 2 38 qn)

;;; (roll midi dur) -> composition?
;;; midi : integer?
;;; dur : dur?
;;; Returns a roll of a midi percussion note with given dur
(define roll
    (lambda (midi dur)
            (tremolo (denominator dur) midi dur)))

(roll 38 hn)

;;; (flam midi dur2) -> composition?
;;; midi : integer?
;;; dur2 : dur?
;;; Returns the flam of a midi percussion note with given dur
(define flam 
    (lambda (midi dur2)
            (pickup (pnote midi (dur 1 (* 2 (denominator dur2))))
                    (accent midi dur2))))
(flam 38 qn)

;;; (single-drag-tap midi) -> composition?
;;; midi : integer?
;;; Returns the midi percussion note, except as a single drag tap
(define single-drag-tap 
    (lambda (midi)
            (pickup (seq (pnote midi sn)
                         (pnote midi sn))
                    (seq (pnote midi en)
                         (accent midi en)))))
(single-drag-tap 38)

;;; PART 2:

;;; horizontal-simple-rock-beat -> composition?
;;; Returns a simple rock beat based on the horizontal method
(define horizontal-simple-rock-beat
    (par (repeat 4 (pnote 42 qn))
         (seq (pnote 35 qn)
              (pnote 38 qn)
              (pnote 35 qn)
              (pnote 38 qn))))

;;; vertical-simple-rock-beat -> composition?
;;; Returns a simple rock beat based on the vertical method
(define vertical-simple-rock-beat
    (repeat 2 
        (seq (par (pnote 42 qn)
                  (pnote 35 qn))
             (par (pnote 42 qn)
                  (pnote 38 qn)))))


horizontal-simple-rock-beat

vertical-simple-rock-beat

;;; (horizontal-beat-machine voices) -> composition?
;;; voices : list?
;;; Returns a beat given a list of compositions / voices
(define horizontal-beat-machine 
    (lambda (voices)
            (apply par voices)))

;;; (vertical-beat-machine pulses dur) -> composition?
;;; pulses : list?
;;; dur : dur?
;;; Returns a beat given a list of a list of midi values per pulse of a given dur
(define vertical-beat-machine
    (lambda (pulses dur)
            (reduce seq (map (lambda (x) (reduce par 
                             (map (lambda (n) (pnote n dur)) x))) pulses))))

;;; horizontal-simple-rock-beat-rewrite -> composition?
;;; Returns a simple rock beat based on the horizontal beat machine
(define horizontal-simple-rock-beat-rewrite
    (horizontal-beat-machine 
        (list (repeat 4 (pnote 42 qn))
              (seq (pnote 35 qn)
                   (pnote 38 qn)
                   (pnote 35 qn)
                   (pnote 38 qn)))))

;;; vertical-simple-rock-beat-rewrite -> composition?
;;; Returns a simple rock beat based on the vertical beat machine
(define vertical-simple-rock-beat-rewrite
    (vertical-beat-machine 
        (list (list 42 35)
              (list 42 38)
              (list 42 35)
              (list 42 38)) qn))

horizontal-simple-rock-beat-rewrite

vertical-simple-rock-beat-rewrite

;;; horizontal-elaborate-rock-beat -> composition?
;;; Returns a elaborate rock beat based on the horizontal beat machine
(define horizontal-elaborate-rock-beat
    (horizontal-beat-machine (list (repeat 8 (pnote 42 en))
                                   (seq (ghost 38 en)
                                        (ghost 38 en)
                                        (accent 38 en)
                                        (ghost 38 en)
                                        (ghost 38 en)
                                        (accent 38 en)
                                        (ghost 38 en)
                                        (ghost 38 en))
                                   (seq (pnote 35 en)
                                        (pnote 35 en)
                                        (rest en)
                                        (pnote 35 en)
                                        (rest en)
                                        (rest en)
                                        (rest en)
                                        (pnote 35 en)))))

horizontal-elaborate-rock-beat

;;; vertical-elaborate-rock-beat -> composition?
;;; Returns a vertical rock beat based on the vertical beat machine
(define vertical-elaborate-rock-beat
    (vertical-beat-machine (list (list 42 38 35)
                                 (list 42 38 35)
                                 (list 42 38 0)
                                 (list 42 38 35)
                                 (list 42 38 0)
                                 (list 42 38 0)
                                 (list 42 38 0)
                                 (list 42 38 35)) en))
vertical-elaborate-rock-beat


;;; Part 4:

;;; horizontal-latin-beat -> composition?
;;; Returns a latin beat based on the horizontal beat machine
(define horizontal-latin-beat
    (horizontal-beat-machine (list (repeat 8 (pnote 42 en))
                                   (seq (rest en)
                                        (rest en)
                                        (pnote 38 en)
                                        (rest en)
                                        (rest en)
                                        (pnote 38 en)
                                        (rest en)
                                        (rest en))
                                   (seq (pnote 35 qn)
                                        (rest en)
                                        (pnote 35 en)
                                        (pnote 35 qn)
                                        (rest en)
                                        (pnote 35 en)))))
horizontal-latin-beat

;;; vertical-latin-beat -> composition?
;;; Returns a latin beat based on the vertical beat machine
(define vertical-latin-beat
    (vertical-beat-machine (list (list 42 35)
                                 (list 42)
                                 (list 42 38)
                                 (list 42 35)
                                 (list 42 35)
                                 (list 42 38)
                                 (list 42)
                                 (list 42 35)) en))
vertical-latin-beat

;;; horizontal-swing-beat -> composition?
;;; Returns a swing beat based on the horizontal beat machine
(define horizontal-swing-beat
    (horizontal-beat-machine (list (seq (pnote 42 qn)
                                        (pnote 42 (dur 1 12))
                                        (rest (dur 1 12))
                                        (pnote 42 (dur 1 12))
                                        (pnote 42 qn)
                                        (pnote 42 (dur 1 12))
                                        (rest (dur 1 12))
                                        (pnote 42 (dur 1 12)))
                                   (seq (rest qn)
                                        (pnote 38 (dur 1 12))
                                        (rest (dur 1 12))
                                        (rest (dur 1 12))
                                        (rest qn)
                                        (pnote 38 (dur 1 12))
                                        (rest (dur 1 12))
                                        (rest (dur 1 12)))
                                   (repeat 4 (pnote 35 qn)))))
horizontal-swing-beat

;;; vertical-swing-beat -> composition?
;;; Returns a swing beat based on the vertical beat machine
(define vertical-swing-beat
    (vertical-beat-machine (list (list 42 35)
                                 (list 0)
                                 (list 0)
                                 (list 42 38 35)
                                 (list 0)
                                 (list 42)
                                 (list 42 35)
                                 (list 0)
                                 (list 0)
                                 (list 42 38 35)
                                 (list 0)
                                 (list 42)) (dur 1 12)))
vertical-swing-beat

;;; horizontal-funk-beat -> composition?
;;; Returns a funk beat based on the horizontal beat machine
(define horizontal-funk-beat
    (horizontal-beat-machine (list (seq (pnote 42 en)
                                        (pnote 42 en)
                                        (pnote 42 en)
                                        (pnote 42 sn)
                                        (rest sn)
                                        (pnote 42 sn)
                                        (rest sn)
                                        (pnote 42 sn)
                                        (rest sn)
                                        (pnote 42 en)
                                        (pnote 42 sn)
                                        (rest sn))
                                   (seq (rest qn)
                                        (accent 38 en)
                                        (rest sn)
                                        (ghost 38 sn)
                                        (rest sn)
                                        (ghost 38 sn)
                                        (ghost 38 sn)
                                        (rest sn)
                                        (accent 38 en)
                                        (rest sn)
                                        (ghost 38 sn))
                                   (seq (pnote 35 en)
                                        (pnote 35 en)
                                        (rest qn)
                                        (rest en)
                                        (rest sn)
                                        (pnote 35 sn)
                                        (rest en)
                                        (pnote 35 en)))))
horizontal-funk-beat

;;; vertical-funk-beat -> composition?
;;; Returns a funk beat based on the vertical beat machine
(define vertical-funk-beat
    (vertical-beat-machine (list (list 42 35)
                                 (list 0)
                                 (list 42 35)
                                 (list 0)
                                 (list 42 38)
                                 (list 0)
                                 (list 42)
                                 (list 38)
                                 (list 42)
                                 (list 38)
                                 (list 42 38)
                                 (list 35)
                                 (list 42 38)
                                 (list 0)
                                 (list 42 35)
                                 (list 38)) sn))
vertical-funk-beat

;;; horizontal-garba-beat -> composition?
;;; Returns a garba beat based on the horizontal beat machine
(define horizontal-garba-beat
    (horizontal-beat-machine (list (seq (pnote 35 en)
                                        (pnote 38 en)
                                        (pnote 35 en)
                                        (pnote 38 en)
                                        (pnote 35 tn)
                                        (rest tn)
                                        (rest tn)
                                        (pnote 38 tn)
                                        (rest sn)
                                        (pnote 38 sn)
                                        (pnote 48 (dur 1 48))
                                        (pnote 48 (dur 1 48))
                                        (pnote 48 (dur 1 48))
                                        (pnote 38 en)))))
horizontal-garba-beat

;;; vertical-garba-beat -> composition?
;;; Returns a garba beat based on the vertical beat machine
(define vertical-garba-beat
    (vertical-beat-machine (list (list 35)
                                 (list 0)
                                 (list 0)
                                 (list 0)
                                 (list 0)
                                 (list 38)
                                 (list 0)
                                 (list 0)
                                 (list 0)
                                 (list 0)
                                 (list 35)
                                 (list 0)
                                 (list 0)
                                 (list 0)
                                 (list 0)
                                 (list 38)
                                 (list 0)
                                 (list 0)
                                 (list 0)
                                 (list 0)
                                 (list 35)
                                 (list 38)
                                 (list 38)
                                 (list 48)
                                 (list 48)
                                 (list 48)
                                 (list 38)) (dur 1 48)))
vertical-garba-beat

;;; my-own-beat -> composition?
;;; Returns my own beat based on the horizontal beat machine
(define my-own-beat
    (horizontal-beat-machine (list (seq horizontal-elaborate-rock-beat
                                        horizontal-funk-beat
                                        horizontal-swing-beat
                                        horizontal-latin-beat
                                        horizontal-garba-beat)
                                   (repeat 3 (par horizontal-elaborate-rock-beat
                                                  horizontal-funk-beat
                                                  horizontal-swing-beat
                                                  horizontal-latin-beat
                                                  horizontal-garba-beat)))))
my-own-beat

;;; Part 5:
; Personally, I found the horizontal beat machine far easier to implement and to use.
; I choose the horizontal beat machine for the final groove as the durations would have been impossible to implement without extreame redunacies on the vertical beat machine.
; They are sliced up into simple beats of 4 or 8 or even 16. This is due to the complexitiy associated with polyrythms and polymetric devices in music.
; To fix the vertical beat machine, we could add another input "a" or "g" for accent and ghost respectivally and have a seperate algorithm that checks to see if "a", "g", or "" is attached to each midi note.