(display (+ (/ 1 2) (/ 1 4)))
(newline)

; Constructor
; (make-rational numerator denominator) -> {N, D} (a cloud)
;
; Selectors
; (numerator {N, D}) -> numerator
; (denominator {N, D}) -> denominator

(define (make-rational numerator denominator)
  (cons numerator denominator))

(define (numerator x) (car x))
(define (denominator x) (cdr x))

(define (add-rational  x y)
  (make-rational (+ (* (numerator x) (denominator y))
                    (* (numerator y) (denominator x)))
                 (* (denominator x) (denominator y))))

(define (multiply-rational x y)
  (make-rational (* (numerator x) (numerator y))
                 (* (denominator x) (denominator y))))

(define a (make-rational 1 2))
(define b (make-rational 1 4))

(define ans (add-rational a b))

(display "Numerator: ")
(display (numerator ans))
(newline)

(display "Denominator: ")
(display (denominator ans))
(newline)

(define (make-rational numerator denominator)
  (let ((g (gcd numerator denominator)))
    (cons (/ numerator g) (/ denominator g))))

; Values are already calculated, so you need to do it again
(define ans (add-rational a b))

(display "Numerator: ")
(display (numerator ans))
(newline)

(display "Denominator: ")
(display (denominator ans))
(newline)

; representing vectors in the plane
(define (make-vector x y) (cons x y))
(define (xcor v) (car v))
(define (ycor v) (cdr v))

; representing line segments
(define (make-segment p q) (cons p q))
(define (segment-start s) (car s))
(define (segment-end s) (cdr s))

(define (average a b) (/ (+ a b) 2))

(define (midpoint s)
  (let ((a (segment-start s))
        (b (segment-end   s)))
    (make-vector (average (xcor a) (xcor b))
                 (average (ycor a) (ycor b)))))

(define (square x) (* x x))

(define (length s)
  (let
    ((dx (- (xcor (segment-end s))
            (xcor (segment-start s))))
     (dy (- (ycor (segment-end s))
            (ycor (segment-start s)))))
    (sqrt (+ (square dx)
             (square dy)))))

(define segment (make-segment (make-vector 1 2)
                              (make-vector 5 6)))

(display "Midpoint: ")
(display (midpoint segment))
(newline)

(display "Length: ")
(display (length segment))
(newline)


