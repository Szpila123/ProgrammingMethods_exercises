#lang racket
;Zadanie 2
(define (compose ffun gfun)
  (lambda (x) (ffun (gfun x))))

(define (square x) (* x x))
(define (inc x) (+ x 1))

;Zadanie 3
(define (identity n) n)
(define (repeted p n)
  (if (>= n 1)
      (compose p (repeted p (- n 1)))
      identity))


;Zadanie 4
(define (product-rec val next start end) 
  (if (> start end)
      1
      (* (val start)
         (product-rec val next (next start) end))))

(define (product-iter val next start end prod)
  (if (> start end)
      prod
      ( product-iter val next (next start) end (* prod (val start)))))
  

(* (product-rec (lambda (x) (/ (+ 2 (- x (modulo x 2)))
                            (+ 1 (+ x (modulo x 2)))))
             inc
             1
             100) 4.0)
(* (product-iter (lambda (x) (/ (+ 2 (- x (modulo x 2)))
                            (+ 1 (+ x (modulo x 2)))))
             inc
             1
             100
             1)
   4.0)

;Zadanie 5
(define (accumulate-rec combiner null-value term a next b)
  (if( a > b )
     null-value
     (combiner (term a) (accumulate-rec combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (if (a > b )
      null-value
      (accumulate-iter combiner (combiner null-value (term a)) (next a) next b)))

;Zadanie 6
(define (cont-frac-rec num den k)
  (define (rec i)
    (if (> i k)
        0
        (/ (num i) (+ (den i) (rec (+ i 1))))))
  (rec 1))

(define (cont-frac-iter num den k)
  (define (iter i rat)
    (if (= i 0)
        rat
        (iter (- i 1) (/ (num i) (+ (den i) rat)))))
  (iter k 0))

;Zadanie 7
(+ 3 (cont-frac-rec (lambda (i) (square (* (- (* 2 i) 1)))) (lambda (i) 6.0) 100))  

;Zadanie 8
(define (atan-cf x)
  (/ x (+ 1 (cont-frac-rec (lambda (i) (square (* i x)))
                  (lambda (i) (+ (* i 2.0) 1.0)) 10000.0))))
;Zadanie 9
(define (build n d b) (/ n (+ d b)))

(define (repeted-build k n d b)
  ((repeted (lambda (x) (build n d x )) k) b))
                           