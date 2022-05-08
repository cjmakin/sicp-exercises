; Exercise 1.16 - Exponent procedure in logarithmic number of steps.
; Recursive and iterative implementations.

(define (square x)
  (* x x))

;Recursive
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;Iterative
(define (fast-expt-iter b n)
  (expt-iter b n 1)) 
 
(define (expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n)
         (expt-iter (square b) (/ n 2) a))
        (else
         (expt-iter b (- n 1) (* a b))))) 