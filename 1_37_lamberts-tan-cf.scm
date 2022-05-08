;; Exercise 1.39: Define a procedure (tan-cf x k) that computes an approximation to
;  the tangent function based on Lambert’s formula.



(define (square x) (* x x))

(define (cont-fraction n d k)
  (if (= k 0)
      0
      (/ (n 1.0) (+ (d 1.0) (cont-fraction n d (- k 1))))))

(define (tan-cf x k)
  (define (t-helper x k d)
    (if (= k 0)
        0
        (/ x (- d (t-helper x (- k 1) (+ d 2))))))
  (/ (t-helper (square x) k 1) 10))


(tan-cf 10.0 22)

(tan 10)