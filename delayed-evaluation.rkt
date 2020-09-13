#lang racket


(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))


(define (bad-if e1 e2 e3)
  (if e1 e2 e3))

(define (bad-fact n )
  (bad-if (= n 0) 1 (* n (bad-fact (- n 1)))))

(define (ok-if e1 e2 e3)
  (if e1 (e2) (e3)))
(define (ok-fact n)
  (ok-if (= n 0) (lambda () 1) (lambda () (* n (ok-fact (- n 1))))))