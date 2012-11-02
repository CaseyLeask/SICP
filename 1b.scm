(define (sos x y)
  (define (square x) (* x x))
  (+ (square x) (square y)))

(display (sos 3 4))
(newline)

; Kinds of expressions
;   Numbers
;   Symbols
;   Lambda Expressions
;   Definitions
;   Conditionals
;   Combinations

; Note: Doesn't work in gambit-scheme
; Iteration
; time  = O(x)
; space = O(1)
;(define (+ x y)
;  (if (= x 0)
;    y
;    (+ (-1+ x) (1+ y))))

(display (+ 3 4))
(newline)

;(define (+ x y)
;  (if (= x 0)
;    y
;    (1+ (+ (-1+ x) (y)))))

(display (+ 3 4))
(newline)

; time  = O(fib(n))
; space = O(n)
(define (fib n)
  (IF (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(display (fib 10))
(newline)


