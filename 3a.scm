; representing vectors in the plane
(define make-vector cons)
(define xcor car)
(define ycor cdr)

(define (add-vectors v1 v2)
  (make-vector (+ (xcor v1) (xcor v2))
               (+ (ycor v1) (ycor v2))))

(define (scale s v)
  (make-vector (* s (xcor v))
               (* s (ycor v))))

(let ((v1 (make-vector 1 2))
      (v2 (make-vector 3 4)))
  (let ((added-vector (add-vectors v1 v2))
        (scaled-vector (scale 3 v1)))
    (display "Adding vectors 1,2 and 3,4: ")
    (display (xcor added-vector))
    (display ",")
    (display (ycor added-vector))
    (newline)
    (display "Scaling vector 1,2 by 3: ")
    (display (xcor scaled-vector))
    (display ",")
    (display (ycor scaled-vector))
    (newline)))

(define make-segment cons)
(define seg-start car)
(define seg-end cdr)

(define (scale-list s l)
  (if (null? l)
    '()
    (cons (* s (car l))
          (scale-list s (cdr l)))))

(display "Scale list 1, 2, 3, 4 by 10: ")
(display (scale-list 10 (list 1 2 3 4)))
(newline)

(define (map procedure list)
  (if (null? list)
    '()
    (cons (procedure (car list))
          (map procedure (cdr list)))))

(define (for-each procedure list)
  (cond ((null? list) "done")
        (else (procedure (car list))
              (for-each procedure (cdr list)))))

(define (scale-list scale list)
  (map (lambda(item) (* scale item)) list))

(display "Scale list 1, 2, 3, 4 by 10: ")
(display (scale-list 10 (list 1 2 3 4)))
(newline)

(display "Squaring all the items in the list 1, 2, 3, 4: ")
(display (map (lambda(item) (* item item)) (list 1 2 3 4)))
(newline)

(display "Adding 10 to each item in the list 1, 2, 3, 4: ")
(display (map (lambda(item) (+ item 10)) (list 1 2 3 4)))
(newline)

; Origin - vector
; vertical, horizontal - numbers
(define (make-rectangle origin horizontal vertical)
  (list origin horizontal vertical))

(define (origin rectangle) 
  (car rectangle))

(define (horiz rectangle)
  (car (cdr rectangle)))

(define (vert rectangle)
  (car (cdr (cdr rectangle))))

(define (coord-map rectangle)
  (lambda(point)
    (add-vectors 
      (origin rectangle)
      (make-vector (* (xcor point)
                      (horiz rectangle))
                   (* (ycor point)
                      (vert rectangle))))))

(let ((rectangle (make-rectangle (make-vector 1 1) 1 1))
      (point (make-vector 1 1)))
  (display "Origin: ")
  (display (xcor (origin rectangle)))
  (display ",")
  (display (ycor (origin rectangle)))
  (newline)
  (display "Horiz: ")
  (display (horiz rectangle))
  (newline)
  (display "Vert: ")
  (display (vert rectangle))
  (newline)
  (display "Old point: ")
  (display (xcor point))
  (display ",")
  (display (ycor point))
  (newline)
  (let ((new-point ((coord-map rectangle) point)))
    (display "New point: ")
    (display (xcor new-point))
    (display ",")
    (display (ycor new-point))
    (newline)))

(display "Test for for-each: ")
(newline)
(display 
  (for-each (lambda(x) (display x) (newline)) (list 1 2 3 4)))
(newline)

; Emulating a graphic display with console output
(define (drawline v1 v2)
  (display "Drawing line from ")
  (display (xcor v1))
  (display ",")
  (display (ycor v1))
  (display " to ")
  (display (xcor v2))
  (display ",")
  (display (ycor v2))
  (newline))

(define (make-picture seglist)
  (lambda (rect)
    (for-each
      (lambda (s)
        (drawline
          ((coord-map rect) (seg-start s))
          ((coord-map rect) (seg-end s))))
      seglist)))

(display "Make a picture (emulated with console output)")
(newline)

(define rect (make-rectangle (make-vector 1 1) 2 2))
(define graphic 
    (list (make-segment (make-vector 0 1)
                        (make-vector 0.5 0.5))
          (make-segment (make-vector 0.5 0.5)
                        (make-vector 1 1))))

((make-picture graphic) rect)

(define (beside picture-one picture-two a)
  (lambda(rectangle)
    (picture-one (make-rectangle
                   (origin rectangle)
                   (* a (horiz rectangle))
                   (vert rectangle)))
    (picture-two (make-rectangle
                   (add-vectors (origin rectangle)
                                (make-vector (* a (horiz rectangle))
                                             0))
                   (* (- 1 a) (horiz rectangle))
                   (vert rectangle)))))

(define picture (make-picture graphic))

(display "Make two pictures beside each other")
(newline)

((beside picture picture 0.5) rect)

(display "Make three pictures beside each other")
(newline)

((beside (beside picture picture 0.5) picture 0.5) rect)
