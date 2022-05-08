;; Exercise 1.37:
; Define a procedure cont-frac such that evaluating (cont-frac n d k) computes
; the value of the k-term finite continued fraction.
; Implement iteritavely and recursively.

(define tolerance .00001)

(define (average a b)
  (/ (+ a b) 2))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define golden-ratio (fixed-point (lambda (x) (average x (+ 1 (/ 1 x))))
             1.0)) 


;Recursive
(define (cont-fraction n d k)
  (if (= k 0)
      0
      (/ (n 1.0) (+ (d 1.0) (cont-fraction n d (- k 1))))))

;Iterative 
(define (cont-fraction-iter n d k)
  (define (iter n d k result)
    (if (= k 0)
        result
        (iter n d (- k 1) (/ (n 1.0) (+ (d 1.0) result)))))
    (iter n d k 1))


(cont-fraction (lambda (i) 1.0)
               (lambda (i) 1.0)
               11)

(cont-fraction-iter (lambda (i) 1.0)
               (lambda (i) 1.0)
               11)

(/ 1 golden-ratio)

((lambda (x) (= (/ x 3) 0)) 3)





