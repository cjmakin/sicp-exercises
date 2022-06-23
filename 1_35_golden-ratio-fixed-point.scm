; Exercise 1.35 - Show that the golden ratio is a fixed point of the transformation
; x -> 1 + 1/x, and use this fact to compute the golden ratio by means of the fixed
; point procedure.

(define tolerance 0.00001)

(define (average a b)
  (/ (+ a b) 2))

(define (close-enough? v1 v2)
  (if (< (abs (- v1 v2)) tolerance)
      #t
      #f))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define (golden-ratio x)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               x))

(golden-ratio 2)
