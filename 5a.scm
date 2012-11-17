(define count 1)
(define (demo x)
  (set! count (+ count 1))
  (+ x count))
(display"Demo: ")
(display (demo 3))(newline)
(display"Demo: ")
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

; Once you add state and side effects,
; the substitution model no longer applies.
; The newer computational model  is called the environment model.
;
; (lambda(x) (+ x y)) 
; X is a bound variable
; Y is a free variable
;
; (lambda(x) ((lambda(y) (* x y)) 3))
; * is a free variable
;
; Frames:
; I: II, III
; x = 3
; y - 5
; Environments: C, D
;
; II
; z = 6
; x = y
; Environments: A
;
; III
; m = 1
; y = 2
; Environments: B
;
; A, B, C, D are environments.
; I, II and III are frames.
; z and x are bound in II
; 
; A
; -->|--|
;    V  V
;    C  B
; A Procedure is made of two parts
; A is (a pointer to) a PROCEDURE OBJECT.
; B is (a pointer to) an environment.
; C is the code of the procedure.
; ('V is used as a down chevron)
;
; 
