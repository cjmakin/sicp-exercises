; General fixed-point procedure for calculating square root.
; Implementation of both average damping and newtons method.

(define dx 0.00001)

(define tolerance 0.00001)

(define (square x) (* x x))

(define (average a b)
  (/ (+ a b) 2))

(define (expo x n)
  (if (= n 0)
      1
      (* x (expo x (- n 1)))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Newton's method
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) (guess)))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

; Average damping
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-ad x)
  (fixed-point-of-transform
   (lambda (y) (/ x y)) average-damp 1.0))

(define (sqrt-nm x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x)) newton-transform 1.0))

(sqrt-nm 2)
(newline)
(sqrt-ad 2)

