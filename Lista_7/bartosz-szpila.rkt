#lang racket
(require rackunit)
(require rackunit/text-ui)

(struct const (val) #:transparent)
(struct op (symb l r) #:transparent)
(struct variable () #:transparent)
(struct derivative (f) #:transparent)
(define (expr? e)
  (match e
    [(variable) true]
    [(const n)  (number? n)]
    [(op s l r) (and (member s '(+ *))
                     (expr? l)
                     (expr? r))]
    [(derivative f) (expr? f)]
    [_          false]))


(define (∂ f)
  (match f
    [(const n)   (const 0)]
    [(variable)  (const 1)]
    [(op '+ f g) (op '+ (∂ f) (∂ g))]
    [(op '* f g) (op '+ (op '* (∂ f) g)
                        (op '* f (∂ g)))]
    [(derivative s) (∂(∂ s))]))
 
(define (eval f val)
  (match f
    [(const n) n]
    [(variable) val]
    [(op '+ l r) (+ (eval l val) (eval r val))]
    [(op '* l r) (* (eval l val) (eval r val))]
    [(derivative s) (eval (∂ s) val)]))

(define evaluation-tests
  (test-suite
   "Tests for evaluating expressions"
   (check-equal? (eval (const 7) 10) 7 "Evaluating of const")
   (check-equal? (eval (variable) 10) 10 "Evaluating of variable")
   (check-equal? (eval (op '* (const 3) (variable)) 3) 9 "Evaluation of formula consisting of var, cons and operator *")
   (check-equal? (eval (op '+ (op '* (const 2) (variable)) (variable)) 4) 12 "Evaluation of formula consisting of var, cons and operators * and +")
   (check-equal? (eval (derivative (const 7)) 10) 0 "Evaluating of derivative of const")
   (check-equal? (eval (derivative (variable)) 10) 1 "Evaluating of derivative of variable")
   (check-equal? (eval (derivative (op '* (variable) (variable))) 10) 20 "Evaluating of derivative of squared variable")
   (check-equal? (eval (derivative (op '* (const 3) (variable))) 3) 3 "Evaluation of formula of derivative consisting of var, cons and operator *")
   (check-equal? (eval (derivative (op '+ (op '* (const 2) (variable)) (variable))) 6) 3 "Evaluation of derivative consisting of var, cons and operators * and +")
   (check-equal? (eval (derivative (derivative (op '* (variable) (variable)))) 3) 2 "Evaluation of double derivatice on squared var")
   (check-equal? (eval (derivative (derivative (op '+ (op '* (const 2) (variable)) (op '* (derivative (op '* (const 3) (variable))) (op '* (op '* (variable) (variable)) (variable)))))) 34) 612 "Testing of complex formula evaluation")
   ))

(run-tests evaluation-tests)