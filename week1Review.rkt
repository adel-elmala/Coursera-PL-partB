#lang racket

(define (bad-if x y z) (if x y z))

(define (bad-fact n)
  (bad-if (= n 0)
          1
          (* n (bad-fact (- n 1)))))

(define (ok-if x y z) (if x (y) (z)))

(define (ok-fact n)
  (ok-if (= n 0)
          (lambda () 1)
          (lambda () (* n (ok-fact (- n 1))))))

(define (fib n)
  (if (or (= n 1) (= n 2))
      1
      (+ (fib (- n 1)) (fib (- n 2)))))


(define (dyn-fib n)
  (letrec ([f (lambda (acc1 acc2 y)
              (if (= n y) (+ acc1 acc2)
                  (f (+ acc1 acc2) acc1 (+ y 1)))
              )])
    (if (or (= n 1) (= n 2))
      1
      (f 1 1 3)
      )))


(define mem-fib
  (letrec ([mem null]
            [f (lambda (n)
                 (let ([ans (assoc n mem)])
                   (if ans
                       (cdr ans)
                       (let ([new-ans
                              (if (or (= n 1) (= n 2)) 
                                  1
                                  
                              (+ (f (- n 1)) (f (- n 2)))
                              ) ])
                         (begin (set! mem (cons (cons n new-ans) mem))
                                new-ans))
                   )

                 ))]
            )
    f 
    ))



  
  