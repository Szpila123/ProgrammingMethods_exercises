#lang racket
(require rackunit)
(require rackunit/text-ui)
;; definicja wyrażeń

(struct variable     (x)        #:transparent)
(struct const        (val)      #:transparent)
(struct op           (symb l r) #:transparent)
(struct let-expr     (x e1 e2)  #:transparent)
(struct if-expr      (b t e)    #:transparent)
(struct cons-expr    (l r)      #:transparent)
(struct car-expr     (p)        #:transparent)
(struct cdr-expr     (p)        #:transparent)
(struct pair?-expr   (p)        #:transparent)
(struct null-expr    ()         #:transparent)
(struct null?-expr   (e)        #:transparent)
(struct symbol-expr  (v)        #:transparent)
(struct symbol?-expr (e)        #:transparent)
(struct lambda-expr  (xs b)     #:transparent)
(struct app-expr     (f es)     #:transparent)
(struct apply-expr   (f e)      #:transparent)

(define (expr? e)
  (match e
    [(variable s)       (symbol? s)]
    [(const n)          (or (number? n)
                            (boolean? n))]
    [(op s l r)         (and (member s '(+ *))
                             (expr? l)
                             (expr? r))]
    [(let-expr x e1 e2) (and (symbol? x)
                             (expr? e1)
                             (expr? e2))]
    [(if-expr b t e)    (andmap expr? (list b t e))]
    [(cons-expr l r)    (andmap expr? (list l r))]
    [(car-expr p)       (expr? p)]
    [(cdr-expr p)       (expr? p)]
    [(pair?-expr p)     (expr? p)]
    [(null-expr)        true]
    [(null?-expr p)     (expr? p)]
    [(symbol-expr v)    (symbol? v)]
    [(symbol?-expr p)   (expr? p)]
    [(lambda-expr xs b) (and (list? xs)
                             (andmap symbol? xs)
                             (expr? b)
                             (not (check-duplicates xs)))]
    [(app-expr f es)    (and (expr? f)
                             (list? es)
                             (andmap expr? es))]
    [(apply-expr f e)   (and (expr? f)
                             (expr? e))]
    [_                  false]))

;; wartości zwracane przez interpreter

(struct val-symbol (s)   #:transparent)
(struct closure (xs b e) #:transparent) ;Tak jak w przypadku 'zwykłych' fcji będziemy potrzebowali domknięcia, aby poprawnie wiązać zmienne wolne w fcji
                          ;Domknięcie tym razem będzie brało listę argumentów, ponieważ fcje są wieloargumentowe

(define (my-value? v)
  (or (number? v)
      (boolean? v)
      (and (pair? v)
           (my-value? (car v))
           (my-value? (cdr v)))
      ; null-a reprezentujemy symbolem (a nie racketowym
      ; nullem) bez wyraźnej przyczyny
      (and (symbol? v)
           (eq? v 'null))
      (and (val-symbol? v)
           (symbol? (val-symbol-s v)))
      (and (closure? v) ;domknięcie jest strukturą składającą się z listy zmiennych, wyrażenia oraz środowiska
           (list? (closure-xs v))
           (andmap symbol? (closure-xs v))
           (expr? (closure-b v))
           (env? (closure-e v)))))

;; wyszukiwanie wartości dla klucza na liście asocjacyjnej
;; dwuelementowych list

(define (lookup x xs)
  (cond
    [(null? xs)
     (error x "unknown identifier :(")]
    [(eq? (caar xs) x) (cadar xs)]
    [else (lookup x (cdr xs))]))

;; kilka operatorów do wykorzystania w interpreterze

(define (op-to-proc x)
  (lookup x `(
              (+ ,+)
              (* ,*)
              (- ,-)
              (/ ,/)
              (> ,>)
              (>= ,>=)
              (< ,<)
              (<= ,<=)
              (= ,=)
              (eq? ,(lambda (x y) (eq? (val-symbol-s x)
                                       (val-symbol-s y))))
              )))

;; interfejs do obsługi środowisk

(define (env-empty) null)
(define env-lookup lookup)
(define (env-add x v env) (cons (list x v) env))

(define (env? e)
  (and (list? e)
       (andmap (lambda (xs) (and (list? e)
                                 (= (length e) 2)
                                 (symbol? (first e)))))))
;; zamiana abstrakcyjnej listy na racketowa
(define (abs-rclist ls)
  (if (eq? 'null ls)
      null
      (cons (car ls) (abs-rclist (cdr ls)))))
;; interpretacja wyrażeń

(define (eval e env)
  (match e
    [(const n) n]
    [(op s l r)
     ((op-to-proc s) (eval l env)
                     (eval r env))]
    [(let-expr x e1 e2)
     (let ((v1 (eval e1 env)))
       (eval e2 (env-add x v1 env)))]
    [(variable x) (env-lookup x env)]
    [(if-expr b t e) (if (eval b env)
                         (eval t env)
                         (eval e env))]
    [(cons-expr l r)
     (let ((vl (eval l env))
           (vr (eval r env)))
       (cons vl vr))]
    [(car-expr p)      (car (eval p env))]
    [(cdr-expr p)      (cdr (eval p env))]
    [(pair?-expr p)    (pair? (eval p env))]
    [(null-expr)       'null]
    [(null?-expr e)    (eq? (eval e env) 'null)]
    [(symbol-expr v)   (val-symbol v)]
    [(lambda-expr xs b) (closure xs b env)]
    [(app-expr f es)   (let ((vf (eval f env)) ;app-expr przujmuje listę racketową wyrażeń w składni abstrakcyjnej
                             (ves (map eval es (make-list (length es) env))))
                         (match vf
                           [(closure xs b c-env)
                            (if (= (length xs) (length ves))
                                (eval b (foldr env-add c-env xs ves))
                                (error "numbers of arguments expected and given dont match"))]
                           [_ (error "application not a function")]))]
    [(apply-expr f e) (let ((vf (eval f env)) ;apply-expr przyjmuje listę abstrakcyjną wyrażeń w składni abstrakcyjnej
                            (ves  (abs-rclist (eval e env))))
                            
                        (match vf
                          [(closure xs b c-env)
                           (if (= (length xs) (length ves))                           
                               (eval b (foldr env-add c-env xs ves))
                               (error "numbers of arguments expected and given dont match"))]
                          [_ (error "application not a funcion")]))]))

(define (run e)
  (eval e (env-empty)))

(define lambda-expr-tests
  (test-suite
   "Tests for lambda-expr funcion"
   (check-equal? (run (lambda-expr '(x y z) (cons-expr (const 1) (const 2)))) (closure '(x y z) (cons-expr (const 1) (const 2)) (env-empty))
                 "Checking the value returned by lambda-expr")
   (check-equal? (run (lambda-expr '(x y z) (op '+ (variable 'x) (variable 'y)))) (closure '(x y z) (op '+ (variable 'x) (variable 'y)) (env-empty))
                 "Checking the value returned by lambda-expr")))

(run-tests lambda-expr-tests)

(define app-expr-tests
  (test-suite
   "Tests for corectness of app-expr funcion"
   (check-equal? (run (app-expr (lambda-expr '(x y z) (cons-expr (const 1) (const 2)))
                                (list (const 1) (const 2) (cons-expr (const 4) (const 5)))))
                 (cons 1 2)
                 "Checking the value returned by app-expr, when env is empty and using only constants")
   (check-equal? (run (app-expr (lambda-expr '(x y z) (op '+ (variable 'x) (variable 'y)))
                                (list (const 1) (const 2) (const 3) )))
                 3
                 "Checking the value returned by app-expr, when env is empty and using arguments")
   (check-equal? (eval (app-expr (lambda-expr '(x y z) (op '+ (variable 'x) (variable 'y)))
                                 (list (variable 'x) (variable 'w) (const 3)))
                       '((w 1)(x 2)(y 30)))
                 3
                 "Checking the value returned by app-expr, when using variables as argumnets")
   (check-equal? (eval (app-expr (lambda-expr '(a b) (car-expr (cdr-expr (cons-expr (variable 'a) (variable 'd)))))
                                 (list (const 1) (cons-expr (const 1) (variable 'd))))
                       '((d (10 2 3))))
                 10
                 "Checking the value returned by app-expr, when using free variables in lambda-expr")))

(run-tests app-expr-tests)

(define apply-expr-tests
  (test-suite
   "Tests for corectness of apply-expr funcion"
   (check-equal? (run (apply-expr (lambda-expr '(x y z) (cons-expr (const 1) (const 2)))
                                  (cons-expr (const 1) (cons-expr (const 2) (cons-expr (cons-expr (const 4) (const 5)) (null-expr) )))))
                 (cons 1 2)
                 "Checking the value returned by apply-expr, when env is empty and using only constants")
   (check-equal? (run (apply-expr (lambda-expr '(x y z) (op '+ (variable 'x) (variable 'y)))
                                (cons-expr (const 1) (cons-expr (const 2) (cons-expr (const 3) (null-expr))))))
                 3
                 "Checking the value returned by app-expr, when env is empty and using arguments")
   (check-equal? (eval (apply-expr (lambda-expr '(x y z) (op '+ (variable 'x) (variable 'y)))
                                   (cons-expr (variable 'x) (cons-expr (variable 'w) (cons-expr (const 3) (null-expr)))))
                       '((w 1)(x 2)(y 30)))
                 3
                 "Checking the value returned by app-expr, when using variables as argumnets")
   (check-equal? (eval (apply-expr (lambda-expr '(a b) (car-expr (cdr-expr (cons-expr (variable 'a) (variable 'd)))))
                                 (cons-expr (const 1) (cons-expr (cons-expr (const 1) (variable 'd)) (null-expr))))
                       '((d (10 2 3))))
                 10
                 "Checking the value returned by app-expr, when using free variables in lambda-expr")))

(run-tests apply-expr-tests)