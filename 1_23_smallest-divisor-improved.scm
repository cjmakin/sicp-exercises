; Exercise 1.21 - Smallest divisor procedure that doesn't check redundent even numbers.

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? n d)
  (= (remainder n d) 0))
 
(define (next d)
  (if (= d 2)
    3
    (+ d 2)))
   
(define (find-divisor n d)
  (cond ((>= d (sqrt n)) #f)
        ((divides? n d) d)
        (else (find-divisor n (next d)))))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
