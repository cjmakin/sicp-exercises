; Exercise 1.33: Implement a variation of accumulate that specifies a filter.
; Show how to epress the following using filtered-accumulate:
;     a. The sum of squares of the primes numbers in the interval a to b.
;     b. The product of all positive integers less than n that are relatively 
;         prime to n.

(define (filter-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a)
         (combiner (term a)
                   (filter-accumulate filter combiner null-value term (next a) next b)))
        (else (filter-accumulate filter combiner null-value term (next a) next b))))

(define (inc x)
  (+ x 1))

(define (identity x) x)

; ----- Prime check procedure + helpers -----
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; ----- Relative prime check + helpers ------
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (relatively-prime? a b)
  (if (= (gcd a b) 1)
    #t
    #f))
;a
(define (sum-primes a b)
  (- (filter-accumulate prime? + 0 identity a inc b) 1))

;b
(define (product-relative-prime n)
  (filter-accumulate (lambda (a) (relatively-prime? a n)) * 1 identity 1 inc n))
