; Exercise 1.43 Write a procedure that takes as inputs a procedure that computes f
; and a positive integer n and returns the procedure that computes the nth repeated
; application of f.

(define (compose f g)
  (lambda (x) (f (g x))))

(define (f x)
  (+ x 1))

(define (square x) (* x x))

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (compose f (repeated f (- n 1)))))

((repeated f 4) 0) 
      
      