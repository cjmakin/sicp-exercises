; Exercise 1.28 - Implement the Miller-Rabin test for primality.

(define (square x)
  (* x x))

(define (expmod base exp m a)
  (let
      (cond ((= exp 0) a)
        ((even? exp)
         (expmod (remainder (square base) m) (/ exp 2) m a))
        (else
         (expmod base (- exp 1) m (remainder (* base a) m)))))      

(define (miller-rabin n a)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (- a 10)))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n (- n 11)) (fast-prime? n (- times 1)))
        (else #f)))

