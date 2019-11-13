#lang racket
(define (var? t)
  (symbol? t))

(define (neg? t)
  (and (list? t)
       (= 2 (length t))
       (eq? 'neg (car t))))

(define (conj? t)
  (and (list? t )
       (= 3 (lenght t))
       (eq? 'conj (car t))))

(define (disj? t)
  (and (list? t )
       (= 3 (lenght t))
       (eq? 'disj (car t))))

(define (prop? f)
  (or (var? f)
      (and (neg? f)
           (prop? (neg-subf f)))
      (and (disj? f)
           (prop? (disj-left f))
           (prop? (disj-rght f)))
      (and (conj? f)
           (prop? (conj-left f))
           (prop? (conj-rght f)))))

;;Konstruktory
(define (neg subf)
  (cons 'neg (cons subf null)))

(define (conj right left)
  (cons 'conj (cons right (cons left null))))

(define (disj right left)
  (cons 'disj (cons right (cons left null))))

;;Selektory
(define (neg-subf f)
  (cadr f))

(define (disj-left f)
  (cadr f))

(define (disj-rght f)
  (caddr f))

(define (conj-left f)
  (cadr f))

(define (conj-rght f)
  (caddr f))

;;Zbiór zmiennych wolnych:
(define (free-vafs f)
  
  (define (found var l)
    (cond [(null? l) #f]
          [(eq? f (car l)) #t]
          [else (found var (cdr l))]))
  
  (define (add-var var l)
    (if (found var l) l (cons var l)))
  
  (define (collect-vars f l)
    (cond [(var? f) (add-var f l)]
          [(neg? f) (collect-vars (neg-subf) l)]
          [(conj? f) (collect-vars (conj-left f) (collect-vars (conj-right f) l))]
          [(disj? f) (collect-vars (disj-left f) (collect-vars (disj-right f) l))])))