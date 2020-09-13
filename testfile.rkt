#lang racket

(provide (all-defined-out))


(define (max-list lst)
  (cond [ (null? lst) (error "Empty List")]
        ;[ (null? (cdr lst)) (car lst)]
        
        [ (list? (car lst)) (max-list (car lst))]
        [ (number? (car lst))  (let ( [max-tl (max-list (cdr lst))] )
                               (if (> max-tl (car lst)) max-tl (car lst)))]
        [#t (max-list (cons 0 (cdr lst)))]
        ))

(define (sum list)
  (cond [(null? list) 0 ]
        [(list? (car list))(+  (sum (car list)) (sum (cdr list)))]
        [(number? (car list)) (+ (car list) (sum (cdr list)))]
        [#t (sum (cdr list))]
        ))



(define list1  [list 1 9 2 3 62 565 5])
(define list2 [list 1 9 [list 2000 #t] 3 62 565 "a" 5])