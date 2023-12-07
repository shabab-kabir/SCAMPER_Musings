;; CSC-151-01 (Spring '23)
;; Mini-Project 7: A Simple Synthesizer
;; Shabab Kabir
;; 26 April 2023
;; ACKNOWLEDGEMENTS:
;;   That one tree that keeps up awake at night; also to the original authors of Mini-Project 7 : A Simple Synthesizer and myself from Lab Lab (04/12).


(import audio)

(define medium-quality 16000)

(define square-sample-helper
    (lambda (sample-quality duration)
            (let* ([dur-vector (vector-range (+ duration 0))]
                   [num-samples (* sample-quality duration)])
                  (vector-map (lambda (v) (remainder (round (/ v 10)) 2)) dur-vector))))

(define square-sample
    (lambda (sample-quality frequency duration)
            (sample-node 
                (|> (square-sample-helper sample-quality duration) 
                    (lambda (vec)
                        (vector-map (lambda (n) (- n 1)) vec))))))

(square-sample medium-quality 600 10000)

(define sine-sample-helper
    (lambda (sample-quality duration frequency)
        (let* ([dur-vector (vector-range (+ duration 0))]
               [num-samples (* sample-quality duration)]
               [samples-per-cycle (/ sample-quality frequency)])
          (vector-map (lambda (v) (sin (* 2 pi (/ v samples-per-cycle)))) dur-vector))))

(define sine-sample
    (lambda (sample-quality frequency duration)
        (sample-node (sine-sample-helper sample-quality duration frequency))))

(sine-sample medium-quality 600 10000)

(define combine-samples-helper
  (lambda (sine-wave square-wave combined-wave i num-samples amplitude-sine amplitude-square)
    (if (< i num-samples)
        (begin
          (let ([sine-sample (vector-ref sine-wave i)]
                [square-sample (vector-ref square-wave i)])
            (vector-set! combined-wave i (/ (+ (* amplitude-sine sine-sample) (* amplitude-square square-sample)) 2)))
          (combine-samples-helper sine-wave square-wave combined-wave (+ i 1) num-samples amplitude-sine amplitude-square))
        combined-wave)))

(define generate-note
  (lambda (sample-quality frequency duration amplitude-sine amplitude-square)
    (let* ([sine-wave (sine-sample-helper sample-quality duration frequency)]
           [square-wave (square-sample-helper sample-quality duration)]
           [num-samples (vector-length sine-wave)]
           [combined-wave (make-vector num-samples 0)])
      (sample-node (combine-samples-helper sine-wave square-wave combined-wave 0 num-samples amplitude-sine amplitude-square)))))


(generate-note medium-quality 600 10000 0.5 0.5)

(define adsr-attack-helper
  (lambda (envelope i attack-duration)
    (if (< i attack-duration)
        (begin
          (vector-set! envelope i (/ i attack-duration))
          (adsr-attack-helper envelope (+ i 1) attack-duration))
        envelope)))

(define adsr-decay-helper
  (lambda (envelope i attack-duration decay-duration)
    (if (< i decay-duration)
        (begin
          (vector-set! envelope (+ attack-duration i) (- 1.0 (* i (/ 0.5 decay-duration))))
          (adsr-decay-helper envelope (+ i 1) attack-duration decay-duration))
        envelope)))

(define adsr-sustain-helper
  (lambda (envelope i attack-decay-duration sustain-duration)
    (if (< i sustain-duration)
        (begin
          (vector-set! envelope (+ attack-decay-duration i) 0.5)
          (adsr-sustain-helper envelope (+ i 1) attack-decay-duration sustain-duration))
        envelope)))

(define adsr-release-helper
  (lambda (envelope i attack-decay-sustain-duration release-duration)
    (if (< i release-duration)
        (begin
          (vector-set! envelope (+ attack-decay-sustain-duration i) (* 0.5 (- 1.0 (/ i release-duration))))
          (adsr-release-helper envelope (+ i 1) attack-decay-sustain-duration release-duration))
        envelope)))

(define adsr-envelope
  (lambda (relative-durations num-samples)
    (let* ([attack-duration (* (car relative-durations) num-samples)]
           [decay-duration (* (cdr relative-durations) num-samples)]
           [sustain-duration (* (cdr relative-durations) num-samples)]
           [release-duration (- num-samples attack-duration decay-duration sustain-duration)]
           [envelope (make-vector num-samples 0)])
      (begin
        (adsr-attack-helper envelope 0 attack-duration)
        (adsr-decay-helper envelope 0 attack-duration decay-duration)
        (adsr-sustain-helper envelope 0 (+ attack-duration decay-duration) sustain-duration)
        (adsr-release-helper envelope 0 (+ attack-duration decay-duration sustain-duration) release-duration)
        envelope))))

(define combine-samples-helper
  (lambda (sine-wave square-wave combined-wave i num-samples amplitude-sine amplitude-square)
    (if (< i num-samples)
        (begin
          (vector-set! combined-wave i (+ (* (vector-ref sine-wave i) amplitude-sine) (* (vector-ref square-wave i) amplitude-square)))
          (combine-samples-helper sine-wave square-wave combined-wave (+ i 1) num-samples amplitude-sine amplitude-square))
        combined-wave)))

(define apply-envelope-helper
  (lambda (combined-wave envelope i num-samples)
    (if (< i num-samples)
        (begin
          (vector-set! combined-wave i (* (vector-ref combined-wave i) (vector-ref envelope i)))
          (apply-envelope-helper combined-wave envelope (+ i 1) num-samples))
        combined-wave)))

(define generate-note
  (lambda (sample-quality frequency duration amplitude-sine amplitude-square envelope-list)
    (let* ([sine-wave (sine-sample-helper sample-quality duration frequency)]
           [square-wave (square-sample-helper sample-quality duration)]
           [num-samples (vector-length sine-wave)]
           [combined-wave (make-vector num-samples 0)]
           [envelope (adsr-envelope envelope-list num-samples)])
      (begin
        (combine-samples-helper sine-wave square-wave combined-wave 0 num-samples amplitude-sine amplitude-square)
        (apply-envelope-helper combined-wave envelope 0 num-samples)
        (sample-node combined-wave)))))


(generate-note medium-quality 600 10000 0.5 0.5 (list 0.1 0.3 0.2))



