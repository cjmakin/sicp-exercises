; Newton's cube root

(define (cube x)
  (* x x x))

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) .001))

(define (cube-root x)
  (cube-iter 1.0 x))

(define (cube-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-iter (/ (+ (* 2 guess)
                   (/ x (square guess)))
                   3) x)))
  