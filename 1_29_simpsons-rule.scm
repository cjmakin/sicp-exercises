; Exercise 1.29 Write a procedure for Simpson's rule of numerical integration.
; Use the procudure to integrate cube between 0 and 1 (with n = 100 and n = 1000).

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x)
  (* x x x))

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (add-h x)
    (+ x h))
  (define (s-term x)
    (if (even? (/ x h))
        (* 2 (f x))
        (* 4 (f x))))
  (* (/ h 3.0)
     (sum s-term a add-h b)))

(simpsons-rule cube 0 1 100)
(simpsons-rule cube 0 1 1000) 
