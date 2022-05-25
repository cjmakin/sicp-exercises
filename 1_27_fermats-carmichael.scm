; Exercise 1.27 - Use Fermat's method to check the primality of the following
; Carmichael numbers: 561, 1105, 1729, 2465, 2821, 6601.

(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n a)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (- a 10)))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n (- n 11)) (fast-prime? n (- times 1)))
        (else #f)))

(fast-prime? 561 2)
(fast-prime? 1105 2)
(fast-prime? 1729 2)
(fast-prime? 2465 2)
(fast-prime? 2821 2)
(fast-prime? 6601 2)
