;Exercise 1.38: Write a program that approximates e based on Euler's continued fraction
; method.

(define (eulers-cf k)
  (define (e-helper d i k)
    (cond ((= i (+ k 2))
           0)
          ((= (modulo i 3) 0)
           (/ 1.0 (+ d (e-helper (+ d 2) (+ i 1) k))))
          (else
           (/ 1.0 (+ 1 (e-helper d (+ i 1) k))))))
  (+ 2 (e-helper 2 2 k)))  
 
(eulers-cf 10) 
