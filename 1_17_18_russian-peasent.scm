;Exercise 1.17 & 1.18 - Multiplication using only addition and subtraction
;in logarithmic number of steps. Recursive and iterative implementations.

(define (halve a)
  (if (= a 0)
      0
      (+ 1 (halve (- a 2)))))

(define (square x)
  (fast-times-iter x x))

(define (double a)
  (+ a a))

;ab = 2(a * b/2) = b/2 * (2 * a)
;Recursive
(define (fast-times a b)
  (cond ((= b 0) 0)
        ((even? b)
         (double (fast-times a (halve b))))
        (else
         (+ a (fast-times a (- b 1)))))) 

;Iterative
(define (fast-times-iter a b)
  (times-iter a b 0))

(define (times-iter a b p)  
  (cond ((= b 0) p)
        ((even? b)
         (times-iter (double a) (halve b) p))
        (else
         (times-iter a (- b 1) (+ a p)))))
