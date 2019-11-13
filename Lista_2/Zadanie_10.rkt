#lang racket
(define (abs x) (if (> x 0) x (* -1 x)))
(define (square x) (* x x))

(define (calc-frac num den acc)
  (define (iter f-last-num s-last-num f-last-den s-last-den i)
    (if (and (> i 1) (<= (abs (- (/ f-last-num f-last-den) (/ s-last-num s-last-den))) acc))
        (/ f-last-num f-last-den)
        (iter (+ (* (den i) f-last-num)
                 (* (num i) s-last-num))
              f-last-num
              (+ (* (den i) f-last-den)
                 (* (num i) s-last-den))
              f-last-den
              (+ i 1))))
  (iter 0 1 1 0 1))

;Odwrotnosc phi z dokladnoscia do 2 liczb po przecinku
(calc-frac (lambda (i) 1.0) (lambda (i) 1.0) 0.001)
;Pi z dokladnoscia do pierwszej cyfry po przecinku
(+ 3 (calc-frac (lambda (i) (square (- (* 2 i) 1))) (lambda (i) 6.0) 0.01))
;Pi z dokladnoscia do 4 cyfr po przecinku
(+ 3 (calc-frac (lambda (i) (square (- (* 2 i) 1))) (lambda (i) 6.0) 0.00001))
;Arcus tangens (dokladonsc w tym wypadku nie tyczy sie calego wyniku, poniewaz fcja calc-frac oblicza druga czesc sumy pierwszego licznika
(define (atan-new x)
  (/ x (+ 1.0 (calc-frac (lambda (i) (square (* i x))) (lambda (i) (+ (* 2 i) 1 )) 0.0001))))
                 