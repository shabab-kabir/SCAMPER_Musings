;; CSC-151-01 (Spring '23)
;; Mini-Project 7: A Simple Synthesizer
;; Shabab Kabir
;; 26 April 2023
;; ACKNOWLEDGEMENTS:
;;   That one tree that keeps up awake at night; also to the original authors of Mini-Project 7 : A Simple Synthesizer and myself from Lab Lab (04/12).

"Part 1"

(import audio)

;;; medium-quality : integer [A predefined constant for medium quality sampling rate]
;;; Value: 16000 [The number of samples per second for medium quality]
(define medium-quality 16000)

;;; (square-sample-helper sample-quality duration) -> Vector? [of square-wave samples]
;;; sample-quality : integer? [The number of samples per second]
;;; duration : integer? [The duration of the waveform in seconds]
;;; Returns a vector of samples for a square waveform with the given parameters.
(define square-sample-helper
    (lambda (sample-quality duration)
            (let* ([dur-vector (vector-range duration)]
                   [num-samples (* sample-quality duration)])
                  (vector-map (lambda (v) (remainder (round (/ v 10)) 2)) dur-vector))))

;;; (square-sample sample-quality frequency duration) -> Sample node?
;;; sample-quality : integer? [The number of samples per second]
;;; frequency : integer? [The frequency of the square waveform in Hz]
;;; duration : integer? [The duration of the waveform in seconds]
;;; Returns a sample node of a square waveform with the given parameters.
(define square-sample
    (lambda (sample-quality frequency duration)
         (sample-node (|> (square-sample-helper sample-quality duration) 
                    (lambda (vec)
                        (vector-map (lambda (n) (- n 1)) vec))
                        (lambda (v) (vector-map (lambda (i) (if (equal? i 0)  
                                                                    1
                                                                    -1)) v))))))
"Square Sample"
(square-sample medium-quality 600 10000)

;;; (sin-sample-helper sample-quality duration frequency) -> Vector? [of sine samples]
;;; sample-quality : integer? [The number of samples per second]
;;; duration : integer? [The duration of the waveform in seconds]
;;; frequency : integer? [The frequency of the sinusoidal waveform in Hz]
;;; Returns a vector of samples for a sinusoidal waveform with the given parameters.
(define sin-sample-helper
    (lambda (sample-quality duration frequency)
        (let* ([dur-vector (vector-range duration)]
               [num-samples (* sample-quality duration)]
               [samples-per-cycle (/ sample-quality frequency)])
          (vector-map (lambda (v) (sin (* 2 pi (/ v samples-per-cycle)))) dur-vector))))

;;; (sin-sample sample-quality frequency duration) -> Sample node?
;;; sample-quality : integer? [The number of samples per second]
;;; frequency : integer? [The frequency of the sinusoidal waveform in Hz]
;;; duration : integer? [The duration of the waveform in seconds]
;;; Returns a sample node of a sinusoidal waveform with the given parameters.
(define sin-sample
    (lambda (sample-quality frequency duration)
        (sample-node (sin-sample-helper sample-quality duration frequency))))
"Sin Sample"
(sin-sample medium-quality 600 10000)

;;; (combine-samples-helper sine-wave square-wave combined-wave i num-samples) -> Vector? [of combined samples]
;;; sine-wave : Vector? [Vector of sine-wave samples]
;;; square-wave : Vector? [Vector of square-wave samples]
;;; combined-wave : Vector? [Vector to store combined samples]
;;; i : integer? [The current index for iteration]
;;; num-samples : integer? [The total number of samples to combine]
;;; Returns a vector of combined sine and square waveform samples with given parameters.
(define combine-samples-helper
  (lambda (sine-wave square-wave combined-wave i num-samples)
    (if (< i num-samples)
        (begin
          (let ([sine-sample (vector-ref sine-wave i)]
                [square-sample (vector-ref square-wave i)])
            (vector-set! combined-wave i (/ (+ sine-sample square-sample) 2)))
          (combine-samples-helper sine-wave square-wave combined-wave (+ i 1) num-samples))
        combined-wave)))

;;; (generate-note sample-quality frequency duration) -> Sample node?
;;; sample-quality : integer? [The number of samples per second]
;;; frequency : integer? [The frequency of the sinusoidal waveform in Hz]
;;; duration : integer? [The duration of the waveform in seconds]
;;; Returns a sample node of a combined sinusoidal and square waveform with the given parameters.
(define generate-note
  (lambda (sample-quality frequency duration)
    (let* ([sin-wave (sin-sample-helper sample-quality duration frequency)]
           [square-wave (square-sample-helper sample-quality duration)]
           [num-samples (vector-length sin-wave)]
           [combined-wave (make-vector num-samples 0)])
      (sample-node (combine-samples-helper sin-wave square-wave combined-wave 0 num-samples)))))

"generate-note Sample"
(generate-note medium-quality 600 10000)

"Part 2"

;;; (adsr-attack-helper envelope i attack-duration) -> Vector? [of envelope with attack phase]
;;; envelope : Vector? [Vector to store the envelope values]
;;; i : integer? [The current index for iteration]
;;; attack-duration : integer? [The attack duration in samples]
;;; Returns a vector of the envelope with the attack phase applied.
(define adsr-attack-helper
  (lambda (envelope i attack-duration)
    (if (< i attack-duration)
        (begin
          (vector-set! envelope i (/ i attack-duration))
          (adsr-attack-helper envelope (+ i 1) attack-duration))
        envelope)))

;;; (adsr-decay-helper envelope i attack-duration decay-duration) -> Vector? [of envelope with decay phase]
;;; envelope : Vector? [Vector to store the envelope values]
;;; i : integer? [The current index for iteration]
;;; attack-duration : integer? [The attack duration in samples]
;;; decay-duration : integer? [The decay duration in samples]
;;; Returns a vector of the envelope with the decay phase applied.
(define adsr-decay-helper
  (lambda (envelope i attack-duration decay-duration)
    (if (< i decay-duration)
        (begin
          (vector-set! envelope (+ attack-duration i) (- 1.0 (* i (/ 0.5 decay-duration))))
          (adsr-decay-helper envelope (+ i 1) attack-duration decay-duration))
        envelope)))

;;; (adsr-sustain-helper envelope i attack-decay-duration sustain-duration) -> Vector? [of envelope with sustain phase]
;;; envelope : Vector? [Vector to store the envelope values]
;;; i : integer? [The current index for iteration]
;;; attack-decay-duration : integer? [The combined duration of attack and decay phases in samples]
;;; sustain-duration : integer? [The sustain duration in samples]
;;; Returns a vector of the envelope with the sustain phase applied.
(define adsr-sustain-helper
  (lambda (envelope i attack-decay-duration sustain-duration)
    (if (< i sustain-duration)
        (begin
          (vector-set! envelope (+ attack-decay-duration i) 0.5)
          (adsr-sustain-helper envelope (+ i 1) attack-decay-duration sustain-duration))
        envelope)))

;;; (adsr-release-helper envelope i attack-decay-sustain-duration release-duration) -> Vector? [of envelope with release phase]
;;; envelope : Vector? [Vector to store the envelope values]
;;; i : integer? [The current index for iteration]
;;; attack-decay-sustain-duration : integer? [The combined duration of attack, decay, and sustain phases in samples]
;;; release-duration : integer? [The release duration in samples]
;;; Returns a vector of the envelope with the release phase applied.
(define adsr-release-helper
  (lambda (envelope i attack-decay-sustain-duration release-duration)
    (if (< i release-duration)
        (begin
          (vector-set! envelope (+ attack-decay-sustain-duration i) (* 0.5 (- 1.0 (/ i release-duration))))
          (adsr-release-helper envelope (+ i 1) attack-decay-sustain-duration release-duration))
        envelope)))

;;; (adsr-envelope relative-durations num-samples) -> Vector? [of envelope]
;;; relative-durations : List? [List of relative durations for the ADSR envelope phases]
;;; num-samples : integer? [The total number of samples in the envelope]
;;; Returns a vector of the complete envelope using the given relative durations.
(define adsr-envelope
  (lambda (relative-durations num-samples)
    (let* ([attack-duration (* (list-ref relative-durations 0) num-samples)]
           [decay-duration (* (list-ref relative-durations 1) num-samples)]
           [sustain-duration (* (list-ref relative-durations 2) num-samples)]
           [release-duration (- num-samples (+ attack-duration decay-duration sustain-duration))]
           [envelope (make-vector num-samples 0)])
      (begin
        (adsr-attack-helper envelope 0 attack-duration)
        (adsr-decay-helper envelope 0 attack-duration decay-duration)
        (adsr-sustain-helper envelope 0 (+ attack-duration decay-duration) sustain-duration)
        (adsr-release-helper envelope 0 (+ attack-duration decay-duration sustain-duration) release-duration)
        envelope))))

;;; (combine-samples-helper sine-wave square-wave combined-wave i num-samples amplitude-sine amplitude-square) -> Vector? [of combined samples]
;;; sine-wave : Vector? [Vector of sine-wave samples]
;;; square-wave : Vector? [Vector of square-wave samples]
;;; combined-wave : Vector? [Vector to store combined samples]
;;; i : integer? [The current index for iteration]
;;; num-samples : integer? [The total number of samples to combine]
;;; amplitude-sine : float? [The amplitude scaling factor for sine-wave samples]
;;; amplitude-square : float? [The amplitude scaling factor for square-wave samples]
;;; Returns a vector of combined sine and square waveform samples with the given amplitudes.
(define combine-samples-helper
  (lambda (sine-wave square-wave combined-wave i num-samples amplitude-sine amplitude-square)
    (if (< i num-samples)
        (begin
          (vector-set! combined-wave i (+ (* (vector-ref sine-wave i) amplitude-sine) (* (vector-ref square-wave i) amplitude-square)))
          (combine-samples-helper sine-wave square-wave combined-wave (+ i 1) num-samples amplitude-sine amplitude-square))
        combined-wave)))

;;; (apply-envelope-helper combined-wave envelope i num-samples) -> Vector? [of samples with applied envelope]
;;; combined-wave : Vector? [Vector of combined sine and square waveform samples]
;;; envelope : Vector? [Vector of the ADSR envelope]
;;; i : integer? [The current index for iteration]
;;; num-samples : integer? [The total number of samples]
;;; Returns a vector of combined waveform samples with the envelope applied.
(define apply-envelope-helper
  (lambda (combined-wave envelope i num-samples)
    (if (< i num-samples)
        (begin
          (vector-set! combined-wave i (* (vector-ref combined-wave i) (vector-ref envelope i)))
          (apply-envelope-helper combined-wave envelope (+ i 1) num-samples))
        combined-wave)))

;;; (generate-note sample-quality frequency duration amplitude-sine amplitude-square envelope-list) -> Sample node?
;;; sample-quality : integer? [The number of samples per second]
;;; frequency : integer? [The frequency of the sinusoidal waveform in Hz]
;;; duration : integer? [The duration of the waveform in seconds]
;;; amplitude-sine : float? [The amplitude scaling factor for sine-wave samples]
;;; amplitude-square : float? [The amplitude scaling factor for square-wave samples]
;;; envelope-list : List? [List of relative
(define generate-note
  (lambda (sample-quality frequency duration amplitude-sine amplitude-square envelope-list)
    (let* ([sin-wave (sin-sample-helper sample-quality duration frequency)]
           [square-wave (square-sample-helper sample-quality duration)]
           [num-samples (vector-length sin-wave)]
           [combined-wave (make-vector num-samples 0)]
           [envelope (adsr-envelope envelope-list num-samples)])
      (begin
        (combine-samples-helper sin-wave square-wave combined-wave 0 num-samples amplitude-sine amplitude-square)
        (apply-envelope-helper combined-wave envelope 0 num-samples)
        (sample-node combined-wave)))))

"generate-note with ASDR Sample"
(generate-note medium-quality 600 10000 0.5 0.5 (list 0.4 0.1 0.2))



