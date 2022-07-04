; Exercise 1.45 - Experiment to determine how many average-damps are required to compute 
; nth roots as a fixed-point search based upon repeated average damping of y -> x/y^(n-1). 
; Use this to implement a simple procedure for computing nth roots using fixed-point, 
; average-damp, and the repeated procedure.

(define tolerance 0.00001)

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      (lambda (x) (f x))
      (compose f (repeated f (- n 1)))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))  
  (try first-guess))

(define (nth-root x n)
  (fixed-point ((repeated average-damp (floor (log n 2)))
                 (lambda (y) (/ x (expt y (- n 1)))))
               1.0))



(nth-root 512 9)

; ---- Number of average damps required for convergence per nth root ----

;nth root  times damped
;2         1
;3         1 
;4         2 
;5         2 
;6  	   2
;7         2
;8         3
;9         3
;10        3
;11        3
;12	   3
;13	   3
;14	   3
;15	   3
;16	   4

; Conclusion:
; Required number of times damped appears to increase by 1 whenever n reaches the next power of 2.
; Number of times damped = floor(log_2(n))