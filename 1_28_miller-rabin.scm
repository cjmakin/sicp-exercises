; Exercise 1.28 - Implement the Miller-Rabin test for primality.

(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((
        ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (miller-rabin n a)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
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
