(define count 1)
(define (demo x)
  (set! count (+ count 1))
  (+ x count))
(display (demo 3))(newline)
(display (demo 3))(newline)

(define (fact n)
  (define (iterate accumulator i)
    (if (> i n) 
      accumulator
      (iterate (* i accumulator) (+ i 1))))
  (iterate 1 1))

(display "Factorial (functional) 5: ")
(display (fact 5)) (newline)

(define (fact n)
  (let ((i 1) 
        (accumulator 1))
    (define (loop)
      (cond ((> i n) accumulator)
        (else 
          (set! accumulator (* i accumulator))
          (set! i (+ i 1))
          (loop))))
    (loop)))

(display "Factorial (imperative) 5: ")
(display (fact 5)) (newline)


