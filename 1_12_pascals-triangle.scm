; Exercise 1.11 - Write a procedure that generates Pascal's triangle by means of a 
; recursive process.

(define (pT n)
  (display (list 1))
  (newline)
  (pT-gen (list 1) (list 1) n))

(define (pT-gen prev new count)
  (if (= count 1)
      (display "End")
      (pT-gen (next-row prev new) (list 1) (- count 1)))) 

(define (next-row prev new)
  (if (null? (cdr prev))
      (display-pT (append new (list 1)))
      (next-row (cdr prev) (append new (list (+ (car prev) (car (cdr prev))))))))

(define (display-pT lst)
  (display lst)
  (newline)
  lst)
