;Exercise 1.11 - Write a procedure that computes the function f by means of 
;a recursive process and by an iterative process.

;recursive
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

;iterative
(define (f-iter n)
  (f-helper 2 1 0 n))

(define (f-helper a b c count)
  (if (= count 0)
      c
      (f-helper (+ a (* 2 b) (* 3 c)) a b (- count 1))))

(f 5)
(f-iter 5)
      


        
      

      
