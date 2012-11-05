(define (atomic? expression) 
  (not (pair? expression)))

(define (constant? expression variable)
  (and (atomic? expression)
       (not (eq? expression variable))))

(define (same-variable? expression variable)
  (and (atomic? expression)
       (eq? expression variable)))

(define (sum? expression)
  (and (not (atomic? expression))
       (eq? (car expression) '+)))

(define (make-sum a1 a2)
  (list '+ a1 a2))

(define a1 cadr)
(define a2 caddr)

(define (product? expression)
  (and (not (atomic? expression))
       (eq? (car expression) '*)))

(define (make-product m1 m2)
  (list '* m1 m2))

(define m1 cadr)
(define m2 caddr)

(define (derivative expression variable)
  (cond ((constant? expression variable) 0)
        ((same-variable? expression variable) 1)
        ((sum? expression)
         (make-sum (derivative (a1 expression) variable)
                   (derivative (a2 expression) variable)))
        ((product? expression)
         (make-sum
           (make-product (m1 expression)
                         (derivative (m2 expression) variable))
           (make-product (derivative (m1 expression) variable)
                         (m2 expression))))))

(display "testing the derivative procedure")
(newline)

(define foo
  '(+ (* a (* x x))
      (+ (* b x)
         c)))

(display (derivative foo 'x))
(newline)


