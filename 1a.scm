(display 3)
(newline)

(display (+ 3 4 8))
(newline)

(display (+ (* 3 (+ 7 19.5))))
(newline)

(display (+ (* 3 5)
            (* 47
               (- 20 6.8))
            12))
(newline)

(define a (* 5 5))
(display (* a a))
(newline)

(define b (+ a (* 5 a)))
(display b)
(newline)

(display (+ a (/ b 5)))
(newline)

(define (square x) (* x x))
(display (square 10))
(newline)

;Syntactic sugar
(define square (lambda (x) (* x x)))
(display (square 10))
(newline)

(display square)
(newline)

(define (average x y)
  (/ (+ x y) 2))

(define (mean-square x y)
  (average (square x)
           (square y)))

(display +)
(newline)

(define (abs x)
  (COND ((< x 0) (- x))
        ((= x 0) 0)
        ((> x 0) x)))

;Another definition of absolute value
(define (abs x)
  (if (< x 0)
    (- x)
    x))

(define (sqrt x) (try 1 x))

(define (try guess x)
  (IF (good-enough? guess x)
      guess
      (try (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 
     0.001))

(display (sqrt 2))
; (try 1 2)
; (try (improve 1 2) 2)
; (try (average 1 (/ 2 1)) 2)
; (try (average 1 2) 2)
; (try 1.5 2)
; (try (average 1.5 (/ 2 1.5)) 2)
; (try 1.3333 2)
; ...
; 577/408
(newline)

;Encapsulating the procedures
(define (sqrt x) 
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 
       0.001))
  (define (try guess)
    (IF (good-enough? guess)
        guess
        (try (improve guess))))
  (try 1))

(display (sqrt 2))
(newline)


