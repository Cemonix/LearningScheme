(define (factorial x)
  (define (inner x acc)
    (cond ((zero? x) acc)
          (else (inner (- x 1) (* x acc)))))
  (inner x 1)
)

(display (factorial 5))

(newline)

(define (fibonacci x)
  (cond ((eq? x 0) 0)
        ((eq? x 1) 1)
        (else (+ (fibonacci (- x 1)) (fibonacci (- x 2)))))
)

(display (fibonacci 10))

(newline)

(define (my_map func lst)
  (define (inner func lst buffer)
    (cond ((null? lst) (reverse buffer))
          (else (inner func (cdr lst) (cons (func (car lst)) buffer)))
    )
  ) 
  (inner func lst '())
)

(display (my_map (lambda (x) (* x x)) '(1 2 3)))
