; Exercise 1.44 -  Write a procedure 'smooth that takes as input a procedure that computes f
; and returns a procedure that computes smoothed f. Show how to generate the n-fold smoothed
; function of any given function using 'smooth' and 'repeated'.

(define dx 0.1)

(define (square x) (* x x))

(define (f x)
  (- (square x) (/ x 2)))

(define (compose f g)
  (lambda (x) (f (g x)))) 

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (compose f (repeated f (- n 1)))))

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

(define (n-fold-smooth f n) 
  (lambda (x) (((repeated smooth n) f) x)))

((n-fold-smooth square 3) 4)
