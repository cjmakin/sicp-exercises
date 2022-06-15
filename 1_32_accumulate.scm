; Exercise 1.32 - Implement a procedure that combines a collection of terms, using some general 
; accumulation function.

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (identity x) x)

(define (factorial n)
  (define (f-next x)
    (+ x 1))
  (product identity 1 f-next n))

(factorial 6)
