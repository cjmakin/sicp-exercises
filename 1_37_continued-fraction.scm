;; Exercise 1.37:
; Define a procedure cont-frac such that evaluating (cont-frac n d k) computes
; the value of the k-term finite continued fraction.
; Implement iteritavely and recursively.

(define tolerance .0001)

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


(/ 1 golden-ratio)

; k = 11 for an approximation accurate to 4 decimal places.
(cont-fraction (lambda (i) 1.0)
               (lambda (i) 1.0)
               11)

; k = 10 for an approximation accurate to 4 decimal places.
(cont-fraction-iter (lambda (i) 1.0)
               (lambda (i) 1.0)
               10)