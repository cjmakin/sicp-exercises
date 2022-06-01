; Exercise 1.28 - Implement the Miller-Rabin test for primality with tail-recursive
; modular exponentiation function.

(define (square x)
  (* x x))

(define (expmod base exp m a)
      (cond ((= exp 0) a)
        ((even? exp)
         (if (and
              (not (or (= base 1) (= base (- m 1))))
              (= (remainder (square base) m) 1))
             0
             (expmod (remainder (square base) m) (/ exp 2) m a)))
        (else 
         (expmod base (- exp 1) m (remainder (* base a) m)))))

(define (miller-rabin n a) 
  (define (try-it a)
    (= (expmod a (- n 1) n 1) 1))  
  (try-it (remainder (+ n 2) n)))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin n (remainder (+ n 2) n)) (fast-prime? n (- times 1)))
        (else #f)))


(define (first-p-primes n p a)
  (cond ((= p 0)
         (display "Done."))
        ((fast-prime? n a)
         (report-primes n p a))
        (else
         (first-p-primes (+ n 2) p a))))
    
(define (report-primes n p a)
  (display "Prime: ")
  (display n)
  (newline)
  (if (even? n)
      (first-p-primes (+ n 1) (- p 1) a)
      (first-p-primes (+ n 2) (- p 1) a)))
  

(report-primes 2 100 1)  
