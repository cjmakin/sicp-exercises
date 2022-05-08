; Exercise 1.42 Define a procedure 'compose' that implements composition.
; Ex: Let f and g be two one-argument functions. The composition f after g is
; define to be the function x -> f(g(x))

(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)