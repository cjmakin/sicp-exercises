; Fermat test

(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond (( = exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fast-exp a n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp a (/ n 2))))
        (else (* a (fast-exp a (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-exp base exp) m))










