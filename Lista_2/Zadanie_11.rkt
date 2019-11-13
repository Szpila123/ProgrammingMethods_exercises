#lang racket
(define (identity x) x)
(define (compose f g) (lambda (x) (f (g x))))
(define (log-2 x)
  (define (iter i z)
    (if (> z 1)
        (iter (+ i 1) (/ z 2))
        i))
  (iter 0 x))
         
(define (repeated p n)
  (if (>= n 1)
      (compose p (repeated p (- n 1)))
      identity))
(define (average x y) (/ ( + x y) 2))
(define (good-enough? x y) (< (abs (- x y)) 0.000001))

(define (fixed-point f s)
  (define (iter k)
    (let ((new-k (f k)))
      (if (good-enough? k new-k)
          k
          (iter new-k))))
  (iter s))

(define (average-damp f)
  (lambda (x) (average x (f x))))


;Testy na liczbę tłumień
;Dla pierwiastka stopnia 2 jedno uśrednienie
(define (sqrt-ad x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
(sqrt-ad 9)
(sqrt-ad 16)

;Dla pierwiastka stopnia 3 jedno uśrednienie
(define (3rd-root x)
  (fixed-point (average-damp (lambda (y) (/ x (* y y)))) 1.0))
(3rd-root 27)
(3rd-root 64)

;Dla pierwiastka stopnia 4 dwa uśrednienia
(define (4th-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (* y y y))))) 1.0))
(4th-root 16)
(4th-root 81)

;Dla pierwiastka stopnia 5 dwa uśrednienia
(define (5th-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (* y y y y))))) 1.0))
(5th-root 32)
(5th-root 1024)

;Dla pierwiastka stopnia 6 trzy uśrednienia
(define (6th-root x)
  (fixed-point (average-damp (average-damp (average-damp (lambda (y) (/ x (* y y y y y)))))) 1.0))
(6th-root 64)
(6th-root 4096)

;Dla pierwiastka stopnia 32 pięć uśrednień
(define (32nd-root x)
  (fixed-point (average-damp (average-damp (average-damp (average-damp (average-damp (lambda (y) (/ x
                                      ((repeated (lambda (z) (* z y)) 30 ) y)) )))))) 1.0))
(32nd-root 4294967296)

;Stąd wzór na ilość tłumień dla pierwiastka n-tego stopinia:
;część całkowita z log2 n
(define (nth-root x root)
  (if (= root 1)
      x
      (fixed-point (   (repeated average-damp (log-2 root))
                       (lambda (y) (/ x
                                      ((repeated (lambda (z) (* z y)) (- root 2) ) y))))
                   1.0)))
(nth-root 64 6)
(nth-root 225 2)
(nth-root 27 3)
(nth-root -27 3)
(nth-root 4294967296 32)
(nth-root 129140163 17)
(nth-root 123 1)
;pierwiastki z ujemnych liczb parzystego stopnia nie mają rozwiązań rzeczywistych, więc (nth-root -64 6) się zapętli
;pierwiastki stopnia mniejszego niż 1 nie istnieją, więc (nth-root 500 0) się zapętli
;(nth-root 500 1) ;cond w wyrażeniu jest wymagany ponieważ zaczynamy uśrednianie od 1.0, a w wypadku pierwiastka stopnia jeden nie dojdzie do uśrednienia, bo 1/2 < 1


 
