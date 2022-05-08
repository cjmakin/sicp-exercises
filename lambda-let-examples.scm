

((lambda (x) (+ x 4)) 4)

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x ((lambda (x) (* x x)) a))
       (* y b)
       (* a b))))

(define (square x) (* x x))
(define x 3)
(let ((x 4)
      (y x))
  (* x y))

(define (f g) (g 3))

(f square)

(f (lambda (z) (* z (+ z 1))))
