; Exercise 1.22 - Write a procedure, search-for-primes, that checks the primality of 
; consecutive odd integers in a specified range. Note the time needed to fine the
; three smallest primes larger than 1,000, larger than 10,000, larger than 100,000, 
; and larger than 1,000,000.

(define (search-for-primes n)
  (if (= (remainder n 2) 0) 
    (search-for-primes (+ n 1))
    (timed-prime-test n)))

(define (timed-prime-test n)
  (start-prime-test n (real-time-clock) 0)) 

(define (start-prime-test n start-time count)
  (cond ((= count 3)  (display "Done.")
		      (newline)
		      (newline))
    	((prime? n) (report-prime n (- (real-time-clock) start-time) count))
	(else (start-prime-test (+ n 2) (real-time-clock) count))))

(define (report-prime n elapsed-time count)
  (display " *** ")
  (newline)
  (display "Prime: ")
  (display n)
  (newline)
  (display "Elapsed time: ")
  (display elapsed-time)
  (newline)
  (start-prime-test (+ n 2) (real-time-clock) (+ count 1)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n ) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next d)
  (if (= d 2)
      3
      (+ d 2)))
      
(define (square n)
  (* n n))

(define (prime? n)
  (= n (smallest-divisor n)))

(search-for-primes 1000)
(search-for-primes 10000)
(search-for-primes 100000)
(search-for-primes 1000000)
