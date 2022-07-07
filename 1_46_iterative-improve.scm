; Exercise 1.46 - Write a procedure, iterative-improve, that takes two procedures
; as arguments: a method for telling whether a guess is good enough and a method 
; for improving a guess.

(define tolerance 0.00001)

(define (average a b)
  (/ (+ a b) 2))

(define (square x)
  (* x x))

(define (f x) 
  (/ (log 1000) (log x)))

;-------------------------------------
(define (iterative-improve good-enough? improve-guess)
  (lambda (guess) (if (good-enough? guess)
                  guess 
                  ((iterative-improve good-enough? improve-guess)
                   (improve-guess guess)))))  

; Fixed point using iterative-improve procedure
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (define (good-enough-fp? guess)
        (< (abs (- guess next))
           tolerance))
      (define (improve-fp guess)
        (try next))
      ((iterative-improve good-enough-fp? improve-fp) guess)))
  (try first-guess))

; Square root using iterative-improve procedure
(define (sqrt-ii x)
  (define (good-enough-sqrt? guess)
    (< (abs (- (square guess) x)) tolerance))
  (define (improve-sqrt guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough-sqrt? improve-sqrt) 1.0))

(sqrt-ii 2)