(define deriv-rules
  '(
    ((dd (?c c) (? v)) 0)
    ((dd (?v v) (? v)) 1)
    ((dd (?v u) (? v)) 0)

    ((dd (+ (? x1) (? x2)) (? v))
     (+ (dd (: x1) (: v))
        (dd (: x2) (: v))))

    ((dd (* (? x1) (? x2)) (? v))
     (+ (* (: x1) (dd (: x2) (: v)))
        (* (dd (: x1) (: v)) (: x2))))

    ((dd (** (? x) (?c n)) (? v))
     (* (* (: n)
           (** (: x) (: (- n 1))))
        (dd (: x) (: v))))
    ))

(define algebra-rules
  '(
    (((? op) (?c e1) (?c e2))
     (: (op e1 e2)))

    (((? op) (? e1) (?c e2))
     ((: op) (: e2) (: e1)))

    ((+ 0 (? e)) (: e))
    ((* 1 (? e)) (: e))
    ((* 0 (? e)) 0)

    ((* (?c e1) (* (?c e2) (?e3)))
     (* (: (* e1 e2)) (: e3)))

    ((* (* (? e1) (?c e2)) (? e3))
     (* (: e2) (* (: e1) (: e3))))

    ((* (* (? e1) (? e2)) (? e3))
     (* (: e1) (* (: e2) (: e3))))

    ((+ (?c e1) (+ (?c e2) (? e3)))
     (+ (: (+ e1 e2)) (: e3)))

    ((+ (? e1) (+ (?c e2) (? e3)))
     (+ (: e2) (+ (: e1) (: e3))))

    ((+ (+ (? e1) (? e2)) (? e3))
     (+ (: e1) (+ (: e2) (: e3))))

    ((+ (* (?c c) (? a)) (* (?c d) (? a)))
     (* (: (+ c d)) (: a)))

    ((* (? c) (+ (? d) (? e)))
     (+ (* (: c) (: d)) (* (: c) (: e))))
  ))

(define (atomic? expression) (not (pair? expression)))
(define (compound? expression) (pair? expression))
(define (constant? expression) (number? expression))
(define (variable? expression) (atomic? expression))
(define (pattern rule) (car rule))
(define (skeleton rule) (cadr rule))

(define (arbitrary-constant? expression)
  (if (compound? expression) (eq? (pattern expression) '?c) #f))

(define (arbitrary-variable? expression)
  (if (compound? expression) (eq? (pattern expression) '?v) #f))

(define (arbitrary-expression? expression)
  (if (compound? expression) (eq? (pattern expression) '?) #f))

(define (variable-name expression) (cadr expression))

(define (skeleton-evaluation? expression)
  (if (compound? expression) (eq? (pattern expression) ':) false))

(define (empty-dictionary) '())

(define (extend-dictionary pattern datum dictionary)
  (let ((name (variable-name pattern)))
    (let ((v (assq name dictionary)))
      (cond ((eq? #f v)
             (cons (list name datum) dictionary))
            ((eq? (cadr v) datum) dictionary)
            (else 'failed)))))

(define (lookup variable dictionary)
  (let ((v (assq variable dictionary)))
    (if (eq? #f v)
      variable
      (cadr v))))


(define (simplifier the-rules)
  (define (simplify-expression expression)
    (try-rules (if (compound? expression)
                 (map simplify-expression expression)
                 expression)))

  (define (try-rules expression)
    (define (scan rules)
      (if (null? rules)
        expression
        (let ((dictionary
                (match (pattern (car rules))
                       expression
                       (empty-dictionary))))
          (if (eq? dictionary 'failed)
            (scan (cdr rules))
            (simplify-expression
              (instantiate
                (skeleton (car rules))
                dictionary))))))
    (scan the-rules))

  simplify-expression)

(define derivative-simplifier
  (simplifier deriv-rules))

(define (match pattern expression dictionary)
  (cond ((eq? dictionary 'failed) 'failed)
        ((atomic? pattern)
         (if (atomic? expression)
           (if (eq? pattern expression)
             dictionary
             'failed)
           'failed))
        ((arbitrary-constant? pattern)
         (if (constant? expression)
           (extend-dictionary pattern expression dictionary)
           'failed))
        ((arbitrary-variable? pattern)
         (if (variable? expression)
           (extend-dictionary pattern expression dictionary)
           'failed))
        ((arbitrary-expression? pattern)
         (extend-dictionary pattern expression dictionary))
        ; Pattern variable clauses
        ((atomic? expression) 'failed)
        (else
          (match (cdr pattern)
                 (cdr expression)
                 (match (car pattern)
                        (car expression)
                        dictionary)))))


(define (evaluate form dictionary)
  (if (atomic? form)
    (lookup form dictionary)
    (apply
      (eval (lookup (car form) dictionary)
            user-initial-environment)
      (map (lambda(v) (lookup v dictionary))
           (cdr form)))))

(define eval-expression cadr)

(define (instantiate skeleton dictionary)
  (define (loop s)
    (cond ((atomic? s) s)
          ((skeleton-evaluation? s)
           (evaluate (eval-expression s) dictionary))
          (else (cons (loop (car s))
                      (loop (cdr s))))))
  (loop skeleton))

(display "Test the derivative simplifier")
(newline)
(display (derivative-simplifier '(dd (+ x y) x)))
(newline)
