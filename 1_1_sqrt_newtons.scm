(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x)(square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (absolute x)
  (cond ((< x 0) x)
        ((= x 0) 0)
        ((> x 0) (- x))))

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

(define (largest-sos x y z)
  (cond ((and (< x y) (< x z)) (sum-of-squares y z))
        ((and (< y x) (< y z)) (sum-of-squares x z))
        ((and (< z x) (< z y)) (sum-of-squares x y))))

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

(define (new-sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))


      







