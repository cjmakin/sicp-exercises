; Exercise 1.10 Ackermann's function
(define (A m n)
  (cond ((= m 0) (* 2 n))
        ((and (>= m 1) (= n 0)) 0)
	((and (>= m 1) (= n 1)) 2)
        (else (A (- m 1) (A m (- n 1))))))


;(f n) = 2n
(define (f n) (A 0 n))

;(g n) = 2^n
(define (g n) (A 1 n))

;(h n) = 2^h(n-1)^2
(define (h n) (A 2 n))

(define (fact n)
  (if (= n 1)
      1
      (* n (fact (- n 1)))))

