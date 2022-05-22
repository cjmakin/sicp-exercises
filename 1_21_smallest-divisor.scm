; Exercise 1.21 - Find smallest divisor of 199, 1999, and 19999.

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? n d)
  (= (remainder n d) 0))
   
   
(define (find-divisor n d)
  (cond ((>= d (sqrt n)) #f)
        ((divides? n d) d)
        (else (find-divisor n (+ d 1)))))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
