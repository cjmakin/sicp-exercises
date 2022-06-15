; Exercise 1.31: Product accumulation procedure, iterative and recursive, with factorial
; and pi calculation (John Wallis' method).

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter (term a) 1))

(define (inc x)
  (+ x 1))

(define (square x) (* x x))

(define (identity x) x)

(define (factorial n)
  (product identity 1 inc n))

(define (pi-product a b)
  (define (pi-next x)
    (+ x 2.0))
  (if (even? b)
      (/ (* a (product square (+ a 2.0) pi-next b))
         (* b (product square (+ a 1.0) pi-next b)))
      (/ (* a (product square (+ a 2.0) pi-next b))
         (/ (product square (+ a 1.0) pi-next b) b))))

(define (wallis-pi n)
  (* 4 (pi-product 2 n)))

(wallis-pi 70)
