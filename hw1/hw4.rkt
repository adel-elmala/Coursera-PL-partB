
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Question 1
(define (sequence low high stride)
  (cond [(<= low high) (cons low (sequence (+ low stride) high stride))]
        [(> low high) null]
                ))

;; Question 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; Question 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t
         (let ([i (remainder n (length xs))])
           (car (list-tail xs i)))]
  ))
;; Question 4
(define  (stream-for-n-steps s n)
  (let ([next (car (s))]
        [rest (cdr (s))]
        )
  (cond [(> n 0) (cons next (stream-for-n-steps rest (- n 1) ))]
        [(= n 0) null]
        [#t (error "stream-for-n-steps Something is Wrong!")]

      )
    )
  )
;; test stream
(define ones (lambda ()(cons 1 ones)))

;; Question 5
(define (make-stream proc first)
  (letrec ([f (lambda (x) (cons x (lambda () (f (proc x first)) )))])
    (lambda () (f first))
    ))
(define funny-add
  (lambda (x y)
             (let* ([addition (+ (abs x) y)])
             (cond [(= (remainder addition 5) 0) (- addition)]
                   [#t addition])
               )
             )
           )

(define funny-number-stream
  (make-stream funny-add 1))

;;(define 5s (make-stream + 5))

;; Question 6


;;(define dan-then-dog
;;  (let ([dan (mcons "dan.jpg" "junk")]
;;        [dog (mcons "dog.jpg" "junk")])
;;    (begin (set-mcdr! dan (lambda () dog))
;;           (set-mcdr! dog (lambda () dan))
;;           (lambda () dan))))

(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
        [dog (lambda () (cons "dog.jpg" dan))])
         dan))
           
    
  

  