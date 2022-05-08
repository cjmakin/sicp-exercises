; A procedure that finds the fixed-point of a given function. This approach of averaging
; successive approximations to a solution is called average damping.

(define tolerance 0.00001)

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

(define (sqrt-fp x)
  (fixed-point (lambda (y) (average y (/ x y)))
               4.0))


(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)
(sqrt-fp 4)