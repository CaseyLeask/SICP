(define (make-rectangular real-part imag-part)
  (cons real-part imag-part))

(define (make-polar r a)
  (cons (* r (cos a)) (* r (sin a))))

(define real-part car)
(define imag-part cdr)

(define (magnitude z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (+c z1 z2)
  (make-rectangular (+ (real-part z1) (real-part z2))
                    (+ (imag-part z1) (imag-part z2))))

(define (-c z1 z2)
  (make-rectangular (- (real-part z1) (real-part z2))
                    (- (imag-part z1) (imag-part z2))))

(define (*c z1 z2)
  (make-polar (* (magnitude z1) (magnitude z2))
              (+ (angle z1) (angle z2))))

(define (/c z1 z2)
  (make-polar (/ (magnitude z1) (magnitude z2))
              (- (angle z1) (angle z2))))

(display "A basic add/subtract test")(newline)

(let ((z1 (make-rectangular 1 2))
      (z2 (make-rectangular 3 4)))
  (let ((z3 (+c z1 z2)))
    (display "Addition of 1,2 & 3,4")(newline)
    (display "Real: ")(display (real-part z3))(newline)
    (display "Imaginary: ")(display (imag-part z3))(newline))
  (let ((z3 (-c z2 z1)))
    (display "Subtraction of 1,2 from 3,4")(newline)
    (display "Real: ")(display (real-part z3))(newline)
    (display "Imaginary: ")(display (imag-part z3))(newline)))

;;; Support mechanism for manifest types

(define (attach-type type contents)
  (cons type contents))

(define type car)
(define contents cdr)

;;; Type predicates

(define (rectangular? z)
  (eq? (type z) 'rectangular))

(define (polar? z)
  (eq? (type z) 'polar))

;;; Rectangular package

(define (make-rectanglular x y)
  (attach-type 'rectangular (cons x y)))

(define real-part-rectangular car)
(define imag-part-rectangular cdr)

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))

;;; Polar package

(define (make-polar r a)
  (attach-type 'polar (cons r a)))

(define magnitude-polar car)
(define angle-polar cdr)

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (real-part z)
  (cond ((rectangular? z) (real-part-rectangular
                            (contents z)))
        ((polar? z) (real-part-polar 
                      (contents z)))))

(define (imag-part z)
  (cond ((rectangular? z) (real-part-rectangular 
                            (contents z)))
        ((polar? z) (real-part-polar 
                      (contents z)))))

(define (magnitude z)
  (cond ((rectangular? z) (magnitude-rectangular 
                            (contents z)))
        ((polar? z) (magnitude-polar 
                      (contents z)))))

(define (angle z)
  (cond ((rectangular? z) (angle-rectangular
                            (contents z)))
        ((polar? z) (angle-polar
                      (contents z)))))

; This compatibility between different data types is called 'Dispatch on type'

;; Operation, type -> procedure
;; Dispatch table.
;; 
(define op-table (make-table))

(define (put op type proc)
  (table-set! op-table (list op type) proc))

(define (get op type)
  (table-ref op-table (list op type) '()))

;;; Installing the rectangular operations in the table

(put 'rectangular 'real-part real-part-rectangular)
(put 'rectangular 'imag-part imag-part-rectangular)
(put 'rectangular 'magnitude magnitude-rectangular)
(put 'rectangular 'angle     angle-rectangular    )

;;; Installing the polar operations in the table

(put 'polar 'real-part real-part-polar)
(put 'polar 'imag-part imag-part-polar)
(put 'polar 'magnitude magnitude-polar)
(put 'polar 'angle     angle-polar    )

(define (operate op object)
  (let ((proc (get (type obj) op)))
    (if (null? proc)
      (error "Undefined operation")
      (proc (contents obj)))))

(define (real-part obj)
  (operate 'real-part obj))

(define (imag-part obj)
  (operate 'imag-part obj))

(define (magnitude obj)
  (operate 'magnitude obj))

(define (angle obj)
  (operate 'angle obj))

; Hal calls this Data-Directed Programming
; If you store the procedures in the data object, it's message passing

; Current operators are +c, -c, *c & /c
; Hal has begun talking about more generic operators
; proposed new operators: add, sub, mul, div

(define (make-rational x y)
  (attach-type 'rational (cons x y)))

(define numerator car)
(define denominator cdr)

(define (add-rational x y)
  (make-rational (+ (* (numerator x) (denominator y))
                    (* (denominator x) (numerator y)))
                 (* (denominator x) (denominator y))))

(define (subtract-rational x y)
  (make-rational (- (* (numerator x) (denominator y))
                    (* (denominator x) (numerator y)))
                 (* (denominator x) (denominator y))))

(define (multiply-rational x y)
  (make-rational (* (numerator x) (numerator y))
                 (* (denominator x) (denominator y))))

(define (divide-rational x y)
  (make-rational (* (numerator x) (denominator y))
                 (* (denominator x) (numerator y))))

(put 'rational 'add add-rational)
(put 'rational 'sub subtract-rational)
(put 'rational 'mul multiply-rational)
(put 'rational 'div divide-rational)

(define (add x y)
  (operate-2 'add x y))

(define (operate-2 operator arg1 arg2)
  (if (eq? (type arg1) (type arg2))
    (let ((proc (get (type arg1) operator)))
      (if (null? proc)
        (error "Op undefined on type")
        (proc (contents arg1)
              (contents arg2))))
    (error "Args not same type")))

;;; Installing complex numbers

(define (make-complex z)
  (attach-type 'complex z))

(define (add-complex z1 z2)
  (make-complex (+c z1 z2)))

(define (subtract-complex z1 z2)
  (make-complex (-c z1 z2)))

(define (multiply-complex z1 z2)
  (make-complex (*c z1 z2)))

(define (divide-complex z1 z2)
  (make-complex (/c z1 z2)))

(put 'complex 'add add-complex)
(put 'complex 'sub subtract-complex)
(put 'complex 'mul multiply-complex)
(put 'complex 'div divide-complex)

(define (make-polynomial variable term-list)
  (attach-type 'polynomial (cons variable term-list)))

(define (variable polynomial)
  (car (contents polynomial)))

(define (term-list polynomial)
  (cdr (contents polynomial)))

(define (same-variable? p1 p2)
  (eq? (variable p1) (variable p2)))

(define (add-terms list1 list2)
  (cond ((empty-termlist? l1) l2)
        ((empty-termlist? l2) l1)
        (else
          (let ((term1 (first-term list1))
                (term2 (first-term list2)))
            (cond ((> (order term1) (order term2))
                   (adjoin-term 
                     term1
                     (add-terms (rest-terms list1) list2)))
                  ((< (order term1) (order term2))
                   (adjoin-term
                     term2
                     (add-terms list1 (rest-terms list2))))
                  (else
                    (adjoin-term
                      (make-term (order term1)
                                 (add (coefficient term1)
                                      (coefficient term2)))
                      (add-terms (rest-terms list1)
                                 (rest-terms list2)))))))))

; The examples he is giving are starting to resemble fractals

(define (add-polynomial p1 p2)
  (if (same-variable? p1 p2)
    (make-polynomial (variable p1)
                     (add-terms (term-list p1)
                                (term-list p2)))
    (error "Polynomials are not in the same variable")))

(put 'polynomial 'add add-polynomial)

(define (add-rational x y)
  (make-rational (add (mul (numerator x) (denominator y))
                      (mul (denominator x) (numerator y)))
                 (mul (denominator x) (denominator y))))

(define (subtract-rational x y)
  (make-rational (sub (mul (numerator x) (denominator y))
                      (mul (denominator x) (numerator y)))
                 (mul (denominator x) (denominator y))))

(define (multiply-rational x y)
  (make-rational (mul (numerator x) (numerator y))
                 (mul (denominator x) (denominator y))))

(define (divide-rational x y)
  (make-rational (mul (numerator x) (denominator y))
                 (mul (denominator x) (numerator y))))

; Decentralised control
