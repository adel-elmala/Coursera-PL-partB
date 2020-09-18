
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
           
    
  
;; Question 7

(define (s-map func stream)
  (letrec ([f (lambda (s) (cons (func (car (s)) ) (lambda () (f (cdr (s))))))])
    (lambda () (f stream))
    )
  )
(define (stream-add-zero s)
  (s-map (lambda (x) (cons 0 x)) s))

;; Question 8

(define (stream-from-list l)
  (letrec ([helper (lambda (lst)
                     (cond [(null? lst) l]
                           [#t lst]))]
           [f (lambda (ls) (cons (car (helper ls)) (lambda () (f (helper (cdr ls))))))])

    (lambda () (f l))
    )) 

(define (combine-streams s1 s2)
  (letrec ([f (lambda (st1 st2) (cons (cons (car (st1)) (car (st2))) (lambda () (f (cdr (st1)) (cdr (st2)))) ))  ])
    (lambda () (f s1 s2))))



(define s1 (stream-from-list (list 1 2 3)))

(define s2 (stream-from-list (list "A" "B")))


(define (cycle-lists l1 l2)
  (let    ([s1 (stream-from-list l1)]
           [s2 (stream-from-list l2)])
    (combine-streams s1 s2)))



;; Question 9
           
(define (vector-assoc val vec)
  (letrec ([len (vector-length vec)]
           [helper (lambda (pos)
                     (if (< pos len) ;; stay in bounds

                         (let ([elt (vector-ref vec pos)])
                           (cond [(pair? elt)
                                  (if (equal? (car elt) val)
                                      elt
                                      (helper (+ pos 1) ))]
                                 [#t (helper (+ pos 1) )]
                                 ))

                         #f) ;; not found
                     
                     )]
           )
    (helper 0)
    )


  )
                             
                                                        
;; Question 10

(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)] ;; initialize the empty cache
        [current-pos 0])
    ;; search in the cache
    (letrec ([f (lambda (v)
                  (let ([ans (vector-assoc v cache)])
                    (if ans
                        (begin
                         ;; (println "in cache")
                          ;;(println cache)
                          ;;(println current-pos)
                          ans );; found and returned
                        (let ([new-ans (assoc v xs) ])  ;; not in cache ; find it 
                          (if new-ans
                          (begin (vector-set! cache current-pos new-ans)
                                 (set! current-pos (remainder (+ current-pos 1) n ))
                                 ;;(println "not in cache\n")
                                 ;;(println cache)
                                 ;;(println current-pos)
                                 new-ans
                                 )
                          new-ans)
                          ))))
                ])
      f

      ) 
    ))


                                                        

