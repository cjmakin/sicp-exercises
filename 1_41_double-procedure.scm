; Exercise 1.41 - Define a procedure 'double' that takes a procedure of one argument
; as argument and returns a procedure that applies the original procedure twice.

(define (inc x) (+ x 1))

(define (times x) (* x 2))

(define (double p)
  (lambda (x) (p (p x))))

((double inc) 5)

((double times) 5)

(((double (double double)) inc) 5) 
