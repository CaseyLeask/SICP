(define (square a) (* a a))
(define (sum-int a b)
  (if (> a b)
    0
    (+ a
       (sum-int (+ a 1) b))))

(display (sum-int 1 4))
(newline)

(define (sum-sq a b)
  (if (> a b)
    0
    (+ (square a)
       (sum-sq (+ a 1) b))))

(display (sum-sq 3 4))
(newline)

(define (pi-sum a b)
  (if (> a b)
    0
    (+ (/ 1 (* a (+ a 2)))
       (pi-sum (+ a 4) b))))

(display (pi-sum 1 9))
(newline)

; Recursive higher-order procedure
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (sum-int a b)
  (define (identity a) a)
  (define (next a) (+ a 1))
  (sum identity a next b))

(display (sum-int 1 4))
(newline)

(define (pi-sum a b)
  (sum (lambda(i) (/ 1 (* i (+ i 2))))
       a
       (lambda(i) (+ i 4))
       b))

(display (pi-sum 1 9))
(newline)

; Iterative higher-order procedure
(define (sum term a next b)
  (define (iter j ans)
    (if (> j b)
      ans
      (iter (next j)
            (+ (term j) ans))))
  (iter a 0))

(display (sum-int 1 4))
(newline)

(display (pi-sum 1 9))
(newline)

(define (fixed-point f start)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.00001))
  (define (iter old new)
    (if (close-enough? old new)
      new
      (iter new (f new))))
  (iter start (f start)))

(define (sqrt x)
  (define (average x y) (/ (+ x y) 2))
  (fixed-point (lambda(y) (average (/ x y) y))
               1))

(display (sqrt 9))
(newline)

(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close-enough? u v)
    (< (abs (- u v)) tolerance))
  (define (iter old new)
    (if (close-enough? old new)
      new
      (iter new (f new))))
  (iter start (f start)))

(display (sqrt 9))
(newline)

(define (average x y) 
  (/ (+ x y) 2))

(define average-dampener (lambda(f) 
                           (lambda(x)
                             (average (f x) x))))

(define (sqrt x)
  (fixed-point (average-dampener (lambda(y) (/ x y)))
               1))

(display (sqrt 9))
(newline)

(define dx 0.000001)

(define deriv (lambda(f)
                (lambda(x)
                  (/ (- (f (+ x dx))
                        (f x))
                     dx))))

(define (newton f guess)
  (define df (deriv f))
  (fixed-point (lambda(x) (- x (/ (f x) (df x))))
               guess))

(define (sqrt x)
  (newton (lambda(y) (- x (square y)))
          1))

(display (sqrt 9))
(newline)
