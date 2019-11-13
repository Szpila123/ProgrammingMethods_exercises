#lang racket
(define (abs x) (if (> x 0) x (- x) ) )
(define (cube x) (* x x x) )
(define (dist x y) (abs (- x y) ) )
(define (root_3 x)
  (define ( closer d ) (/ (+ (* 2 d)
                             (/ x (* d d))
                             )
                          3
                          )
    )
    (define ( iter d ) (if ( Good_enough d )
                           d
                           (iter (closer d))
                           )
      )
    (define ( Good_enough d ) (< (dist x (cube d)) 0.000001 ))
    ( iter 1 )
)

(root_3 27.0)
(root_3 8.0)
(root_3 -8.0)
(root_3 0.0)
