;Exercise 1.11

;recursive
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (iter-f n)
  (f-iter 2 1 0 n))

(define (f-iter a b c count)
  (if (= count 0)
      c
      (f-iter (f-helper a b c) a b (- count 1))))
      

(define (f-helper a b c)
  (+ a (* 2 b) (* 3 c)))
        
      

      