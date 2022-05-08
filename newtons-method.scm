; Newton's method (special case)

(define dx 0.00001)
(define tolerance 0.00001

(define (expo x n)
  (if (= n 0)
      1
      (* x (expt x (- n 1)))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))  
  (try first-guess))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (root-nm x n)
  (newtons-method
   ; find a zero of the function y = y^2 - x. x = 2 so the sqrt of x is what lets y^2 - x = 0.
   ; Initial guess is 1.
   (lambda (y) (- (expo y n) x)) 1.0))

(root-nm 27 2)

; g(x)  = y^2 - x ... (2)^2 - 4
; Dg(x) = 2y ... 2(2)
; f(x)  = x - g(x) / Dg(x) ... 2 - (2)^2 - 4 / 2(2)

