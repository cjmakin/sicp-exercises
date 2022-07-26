; Exercise 1.24 - Use the Fermat method to check the primality of consecutive odd 
; integers in a specified range. Note the time needed to fine the three smallest 
; primes larger than 1,000, larger than 10,000, larger than 100,000, and larger 
; than 1,000,000.

(define (search-for-primes n)
  (if (= (remainder n 2) 0) 
    (search-for-primes (+ n 1))
    (timed-prime-test n)))

(define (timed-prime-test n)
  (start-prime-test n (runtime) 0)) 

(define (start-prime-test n start-time count)
  (cond ((= count 3)  (display "Done.")
		      (newline)
		      (newline))
    	((fast-prime? n 1) (report-prime n (- (runtime) start-time) count))
	(else (start-prime-test (+ n 2) (runtime) count))))

(define (report-prime n elapsed-time count)
  (display " *** ")
  (newline)
  (display "Prime: ")
  (display n)
  (newline)
  (display "Elapsed time: ")
  (display elapsed-time)
  (newline)
  (start-prime-test (+ n 2) (runtime) (+ count 1)))

(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((= 0) 1)
	((even? exp)
	 (remainder
	   (square (expmod base (/ exp 2) m))
	   m))
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

(fast-prime? 1000 1)
