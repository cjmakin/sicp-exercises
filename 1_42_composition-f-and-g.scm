; Exercise 1.42 - Define a procedure 'compose' that implements composition.

(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)
