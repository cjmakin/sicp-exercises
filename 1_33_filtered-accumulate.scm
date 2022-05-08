; Exercise 1.33: Accumulate procedure with filter.

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (filter-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a)
         (combiner (term a)
                   (filter-accumulate filter combiner null-value term (next a) next b)))
        (else (filter-accumulate filter combiner null-value term (next a) next b))))

(define (inc x)
  (+ x 1))

(define (identity x) x)

(define (sum-evens a b)
  (filter-accumulate even? + 0 identity a inc b))

(sum-evens 1 20)

