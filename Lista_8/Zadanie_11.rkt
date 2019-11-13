#lang racket
;; PROGRAMY TESTOWANE W DRRACKETCIE Z MEM. LIMIT ustawionym na 1024MB
(require rackunit)
(require rackunit/text-ui)
(provide (all-defined-out))
;; Po kilkukrotnym uruchomieniu wszystkich testów, można zauważyć że oba algorytmy mają podobne czasy, przy czym biorąc pod uwagę metodę sprawdzania złożoności, ciężko stwierdzić który jest szybszy

;;Z testów wynika że zoptymalizowana wersja osiąga podobne wyniki do normalnej wersji.
;;biorąc pod uwagę iż większość czasu poświęcona jest na tłumaczeniu kodu przez interpreter oraz
;;nasza metoda określania złożoności jest poważnie uzależniona od aktualnego środowiska systemowego (różnice nawet do 30% średniego wyniku),
;;ciężko jest jednoznacznie określić czy optymalizacja przyśpiesza działanie programów
;;---------------------------------------------------------------------------------------------------------------------------------------
;; definicja wyrażeń z let-wyrażeniami i if-wyrażeniami
(module optimized racket
  (provide (all-defined-out))
  (struct variable (x)         #:transparent)
  (struct const    (val)       #:transparent)
  (struct op       (symb l r)  #:transparent)
  (struct let-expr (x e1 e2)   #:transparent)
  (struct if-expr  (b t e)     #:transparent)

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
      [_                  false]))

  ;; definicja instrukcji w języku WHILE

  (struct skip      ()       #:transparent) ; skip
  (struct comp      (s1 s2)  #:transparent) ; s1; s2
  (struct assign    (x e)    #:transparent) ; x := e
  (struct while     (b s)    #:transparent) ; while (b) s
  (struct if-stm    (b t e)  #:transparent) ; if (b) t else e
  (struct var-block (x e s)  #:transparent) ; var x := e in s

  (define (stm? e)
    (match e
      [(skip) true]
      [(comp s1 s2)   (and (stm? s1) (stm? s2))]
      [(assign x e)   (and (symbol? x) (expr? e))]
      [(while b s)    (and (expr? b) (stm? s))]
      [(if-stm b t e) (and (expr? b) (stm? t) (stm? e))]
      [_ false]))
	  
  ;; wyszukiwanie wartości dla klucza na liście asocjacyjnej
  ;; dwuelementowych list

  (define (lookup-mut x xs)
    (cond
      [(null? xs)
       (error x "unknown identifier :(")]
      [(eq? (mcar (mcar xs)) x) (mcar (mcdr (mcar xs)))]
      [else (lookup-mut x (mcdr xs))]))

  (define (lookup x xs)
    (cond
      [(null? xs)
       (error x "unknown identifier :(")]
      [(eq? (caar xs) x) (cadar xs)]
      [else (lookup x (cdr xs))]))
  ;; aktualizacja środowiska dla danej zmiennej (koniecznie już
  ;; istniejącej w środowisku!)

  (define (update x v xs)
    (begin
      (cond
        [(null? xs)
         (error x "unknown identifier :(")]
        [(eq? (mcar (mcar xs)) x)
         (set-mcar! (mcdr (mcar xs)) v)]
        [else
         (update x v (mcdr xs))])
      xs))
  ;; DOPISANE zamiana listy asocjacyjnej na środowisko oraz pomocnicza zamiana listy na mlistę
  (define (assoc-list->env xs)
    (cond [(null? xs) null]
          [else (mcons (list->mlist (car xs)) (assoc-list->env (cdr xs)))]))

  (define (list->mlist xs)
    (cond [(null? xs) null]
          [else (mcons (car xs) (list->mlist (cdr xs)))]))
  ;; kilka operatorów do wykorzystania w interpreterze

  (define (op-to-proc x)
    (lookup x `((+ ,+)
                (* ,*)
                (- ,-)
                (/ ,/)
                (%, modulo)
                (> ,>)
                (>= ,>=)
                (< ,<)
                (<= ,<=)
                (= ,=)
                (!= ,(lambda (x y) (not (= x y)))) 
                (&& ,(lambda (x y) (and x y)))
                (|| ,(lambda (x y) (or x y)))
                )))

  ;; interfejs do obsługi środowisk

  (define (env-empty) null)
  (define env-lookup lookup-mut)
  (define (env-add x v env) (mcons (mcons x (mcons v null)) env))
  (define env-update update)
  (define env-discard mcdr)
  (define (env-from-assoc-list xs) (assoc-list->env xs))

  ;; ewaluacja wyrażeń ze środowiskiem

  (define (eval e env)
    (match e
      [(const n) n]
      [(op s l r) ((op-to-proc s) (eval l env)
                                  (eval r env))]
      [(let-expr x e1 e2)
       (let ((v1 (eval e1 env)))
         (eval e2 (env-add x v1 env)))]
      [(variable x) (env-lookup x env)]
      [(if-expr b t e) (if (eval b env)
                           (eval t env)
                           (eval e env))]))

  ;; interpretacja programów w języku WHILE, gdzie środowisko m to stan
  ;; pamięci. Interpreter to procedura, która dostaje program i początkowy
  ;; stan pamięci, a której wynikiem jest końcowy stan pamięci. Pamięć to
  ;; aktualne środowisko zawierające wartości zmiennych

  (define (interp p m)
    (match p
      [(skip) m]
      [(comp s1 s2) (interp s2 (interp s1 m))]
      [(assign x e)
       (env-update x (eval e m) m)]
      [(while b s)
       (if (eval b m)
           (interp p (interp s m))
           m)]
      [(var-block x e s)
       (env-discard
        (interp s (env-add x (eval e m) m)))]
      [(if-stm b t e) (if (eval b m)
                          (interp t m)
                          (interp e m))]))

  ;; silnia zmiennej i

  (define fact-in-WHILE
    (var-block 'x (const 0)                                           ; var x := 0 in
               (comp (assign 'x (const 1))                                    ;   x := 1
                     (comp (while (op '> (variable 'i) (const 0))                   ;   while (i > 0)
                                  (comp (assign 'x (op '* (variable 'x) (variable 'i))) ;     x := x * i
                                        (assign 'i (op '- (variable 'i) (const 1)))))   ;     i := i - 1
                           (assign 'i (variable 'x))))))                            ;   i := x

  (define (factorial n)
    (env-lookup 'i (interp fact-in-WHILE
                           (env-from-assoc-list `((i ,n))))))

  ;; najmniejsza liczba pierwsza nie mniejsza niż i

  (define find-prime-in-WHILE
    (var-block 'c (variable 'i)                                         ; var c := i in
               (var-block 'continue (const true)                                   ; var continue := true in
                          (comp
                           (while (variable 'continue)                                        ; while (continue)
                                  (var-block 'is-prime (const true)                                 ;   var is-prime := true in
                                             (var-block 'x (const 2)                                           ;   var x := 2 in
                                                        (comp
                                                         (while (op '&& (variable 'is-prime)                             ;   while (is-prime &&
                                                                    (op '< (variable 'x) (variable 'c)))             ;            x < c)
                                                                (comp (if-stm (op '= (op '% (variable 'c) (variable 'x))     ;     if (c % x =
                                                                                  (const 0))                              ;                 0)
                                                                              (assign 'is-prime (const false))               ;       is-prime := false
                                                                              (skip))                                        ;     else skip
                                                                      (assign 'x (op '+ (variable 'x) (const 1)))))          ;     x := x + 1 
                                                         (if-stm (variable 'is-prime)                                    ;   if (is-prime)
                                                                 (assign 'continue (const false))                        ;     continue := false
                                                                 (comp (assign 'continue (const true))                   ;   else continue := true
                                                                       (assign 'c (op '+ (variable 'c) (const 1))))))))) ;        c := c + 1
                           (assign 'i (variable 'c))))))                                      ; i := c

  (define (find-prime-using-WHILE n)
    (env-lookup 'i (interp find-prime-in-WHILE
                           (env-from-assoc-list `((i ,n) (is-prime ,true))))))

  (define nth-fib-elem-in-WHILE
    (var-block 'c (variable 'i)
               (var-block 'first (const 0)
                          (var-block 'second (const 1)
                                     (comp
                                      (while (op '> (variable 'c) (const 1))
                                             (comp
                                              (assign 'first (op '+ (variable 'first) (variable 'second)))
                                              (comp
                                               (assign 'second (op '+ (variable 'first) (variable 'second)))
                                               (assign 'c (op '- (variable 'c) (const 2))))))
                                      (if-stm (op '= (variable 'c) (const 0))
                                              (assign 'i (variable 'first))
                                              (assign 'i (variable 'second))))))))
  (define (nth-fib-elem n)
    (env-lookup 'i (interp nth-fib-elem-in-WHILE
                           (env-from-assoc-list `((i ,n))))))
                                                                                                                        
  )

(module normal racket
  (provide (all-defined-out))
  (struct variable (x)         #:transparent)
  (struct const    (val)       #:transparent)
  (struct op       (symb l r)  #:transparent)
  (struct let-expr (x e1 e2)   #:transparent)
  (struct if-expr  (b t e)     #:transparent)

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
      [_                  false]))

  ;; definicja instrukcji w języku WHILE

  (struct skip      ()       #:transparent) ; skip
  (struct comp      (s1 s2)  #:transparent) ; s1; s2
  (struct assign    (x e)    #:transparent) ; x := e
  (struct while     (b s)    #:transparent) ; while (b) s
  (struct if-stm    (b t e)  #:transparent) ; if (b) t else e
  (struct var-block (x e s)  #:transparent) ; var x := e in s

  (define (stm? e)
    (match e
      [(skip) true]
      [(comp s1 s2)   (and (stm? s1) (stm? s2))]
      [(assign x e)   (and (symbol? x) (expr? e))]
      [(while b s)    (and (expr? b) (stm? s))]
      [(if-stm b t e) (and (expr? b) (stm? t) (stm? e))]
      [_ false]))

  ;; wyszukiwanie wartości dla klucza na liście asocjacyjnej
  ;; dwuelementowych list

  (define (lookup x xs)
    (cond
      [(null? xs)
       (error x "unknown identifier :(")]
      [(eq? (caar xs) x) (cadar xs)]
      [else (lookup x (cdr xs))]))

  ;; aktualizacja środowiska dla danej zmiennej (koniecznie już
  ;; istniejącej w środowisku!)

  (define (update x v xs)
    (cond
      [(null? xs)
       (error x "unknown identifier :(")]
      [(eq? (caar xs) x)
       (cons (list (caar xs) v) (cdr xs))]
      [else
       (cons (car xs) (update x v (cdr xs)))]))

  ;; kilka operatorów do wykorzystania w interpreterze

  (define (op-to-proc x)
    (lookup x `((+ ,+)
                (* ,*)
                (- ,-)
                (/ ,/)
                (%, modulo)
                (> ,>)
                (>= ,>=)
                (< ,<)
                (<= ,<=)
                (= ,=)
                (!= ,(lambda (x y) (not (= x y))))
                (&& ,(lambda (x y) (and x y)))
                (|| ,(lambda (x y) (or x y)))
                )))

  ;; interfejs do obsługi środowisk

  (define (env-empty) null)
  (define env-lookup lookup)
  (define (env-add x v env) (cons (list x v) env))
  (define env-update update)
  (define env-discard cdr)
  (define (env-from-assoc-list xs) xs)

  ;; ewaluacja wyrażeń ze środowiskiem

  (define (eval e env)
    (match e
      [(const n) n]
      [(op s l r) ((op-to-proc s) (eval l env)
                                  (eval r env))]
      [(let-expr x e1 e2)
       (let ((v1 (eval e1 env)))
         (eval e2 (env-add x v1 env)))]
      [(variable x) (env-lookup x env)]
      [(if-expr b t e) (if (eval b env)
                           (eval t env)
                           (eval e env))]))

  ;; interpretacja programów w języku WHILE, gdzie środowisko m to stan
  ;; pamięci. Interpreter to procedura, która dostaje program i początkowy
  ;; stan pamięci, a której wynikiem jest końcowy stan pamięci. Pamięć to
  ;; aktualne środowisko zawierające wartości zmiennych

  (define (interp p m)
    (match p
      [(skip) m]
      [(comp s1 s2) (interp s2 (interp s1 m))]
      [(assign x e)
       (env-update x (eval e m) m)]
      [(while b s)
       (if (eval b m)
           (interp p (interp s m))
           m)]
      [(var-block x e s)
       (env-discard
        (interp s (env-add x (eval e m) m)))]
      [(if-stm b t e) (if (eval b m)
                          (interp t m)
                          (interp e m))]))

  ;; silnia zmiennej i

  (define fact-in-WHILE
    (var-block 'x (const 0)                                           ; var x := 0 in
               (comp (assign 'x (const 1))                                    ;   x := 1
                     (comp (while (op '> (variable 'i) (const 0))                   ;   while (i > 0)
                                  (comp (assign 'x (op '* (variable 'x) (variable 'i))) ;     x := x * i
                                        (assign 'i (op '- (variable 'i) (const 1)))))   ;     i := i - 1
                           (assign 'i (variable 'x))))))                            ;   i := x

  (define (factorial n)
    (env-lookup 'i (interp fact-in-WHILE
                           (env-from-assoc-list `((i ,n))))))

  ;; najmniejsza liczba pierwsza nie mniejsza niż i

  (define find-prime-in-WHILE
    (var-block 'c (variable 'i)                                         ; var c := i in
               (var-block 'continue (const true)                                   ; var continue := true in
                          (comp
                           (while (variable 'continue)                                        ; while (continue)
                                  (var-block 'is-prime (const true)                                 ;   var is-prime := true in
                                             (var-block 'x (const 2)                                           ;   var x := 2 in
                                                        (comp
                                                         (while (op '&& (variable 'is-prime)                             ;   while (is-prime &&
                                                                    (op '< (variable 'x) (variable 'c)))             ;            x < c)
                                                                (comp (if-stm (op '= (op '% (variable 'c) (variable 'x))     ;     if (c % x =
                                                                                  (const 0))                              ;                 0)
                                                                              (assign 'is-prime (const false))               ;       is-prime := false
                                                                              (skip))                                        ;     else skip
                                                                      (assign 'x (op '+ (variable 'x) (const 1)))))          ;     x := x + 1
                                                         (if-stm (variable 'is-prime)                                    ;   if (is-prime)
                                                                 (assign 'continue (const false))                        ;     continue := false
                                                                 (comp (assign 'continue (const true))                   ;   else continue := true
                                                                       (assign 'c (op '+ (variable 'c) (const 1))))))))) ;        c := c + 1
                           (assign 'i (variable 'c))))))                                      ; i := c

  (define (find-prime-using-WHILE n)
    (env-lookup 'i (interp find-prime-in-WHILE
                           (env-from-assoc-list `((i ,n) (is-prime ,true))))))

  (define nth-fib-elem-in-WHILE
    (var-block 'c (variable 'i)
               (var-block 'first (const 0)
                          (var-block 'second (const 1)
                                     (comp
                                      (while (op '> (variable 'c) (const 1))
                                             (comp
                                              (assign 'first (op '+ (variable 'first) (variable 'second)))
                                              (comp
                                               (assign 'second (op '+ (variable 'first) (variable 'second)))
                                               (assign 'c (op '- (variable 'c) (const 2))))))
                                      (if-stm (op '= (variable 'c) (const 0))
                                              (assign 'i (variable 'first))
                                              (assign 'i (variable 'second))))))))
  (define (nth-fib-elem n)
    (env-lookup 'i (interp nth-fib-elem-in-WHILE
                           (env-from-assoc-list `((i ,n))))))
  )

(require (prefix-in opt: 'optimized))
(require (prefix-in nor: 'normal))

;;TEST Z ZADANIA 10
(define (test-find-prime-big-num1)
  (begin
    (display "wait...\n")
    (flush-output (current-output-port))
    (test-performance1)))

(define (test-performance1)
  (let-values
      (((r1 cpu1 real1 gc1) (time-apply opt:find-prime-using-WHILE(list 1111111)))
       ((r2 cpu2 real2 gc2) (time-apply nor:find-prime-using-WHILE(list 1111111))))
    (begin
      (display "Optimized alg.: (cpu, real, gc): ")
      (display cpu1)  (display ", ")
      (display real1) (display ", ")
      (display gc1)   (display "\n")
      (display "Normal impl.: (cpu, real, gc): ")
      (display cpu2)  (display ", ")
      (display real2) (display ", ")
      (display gc2)   (display "\n"))))


;;ODRÓCONY TEST test-find-prime-big-num1
(define (test-find-prime-big-num2)
  (begin
    (display "wait...\n")
    (flush-output (current-output-port))
    (test-performance2)))

(define (test-performance2)
  (let-values
      (((r1 cpu1 real1 gc1) (time-apply nor:find-prime-using-WHILE(list 1111111)))
       ((r2 cpu2 real2 gc2) (time-apply opt:find-prime-using-WHILE(list 1111111))))
    (begin
      (display "Normal impl.: (cpu, real, gc): ")
      (display cpu1)  (display ", ")
      (display real1) (display ", ")
      (display gc1)   (display "\n")
      (display "Optimized alg.: (cpu, real, gc): ")
      (display cpu2)  (display ", ")
      (display real2) (display ", ")
      (display gc2)   (display "\n"))))

;;TEST PIERWSZOŚCI DLA MAŁYCH LICZBA (1000..2)
(define (test-find-prime-small-nums)
  (begin
    (display "wait...\n")
    (flush-output (current-output-port))
    (test-performance3)))

(define (test-performance3)
  (let-values
      (((r1 cpu1 real1 gc1) (time-apply find-small-primes (list 1)))   ;nor:find-prime-using-WHILE (list 1111111)))                            
       ((r2 cpu2 real2 gc2) (time-apply find-small-primes (list 2))))  ;opt:find-prime-using-WHILE (list 1111111))))
    (begin
      (display "Normal impl.: (cpu, real, gc): ")
      (display cpu1)  (display ", ")
      (display real1) (display ", ")
      (display gc1)   (display "\n")
      (display "Optimized alg.: (cpu, real, gc): ")
      (display cpu2)  (display ", ")
      (display real2) (display ", ")
      (display gc2)   (display "\n"))))

(define (find-small-primes x)
  (define (iter n)
    (if (= n 2)
        null
        (begin
          (if (= x 1) (nor:find-prime-using-WHILE n) (opt:find-prime-using-WHILE n))
          (iter (- n 1)))))
  (iter 1000))
          
         
;;TEST DLA ALGORYTMU LICZENIA N-TEGO EL. CIĄGU FIB.

;;DUŻA LICZBA
(define (test-fib-sq-big-num)
  (begin
    (display "wait...\n")
    (flush-output (current-output-port))
    (test-performance4)))

(define (test-performance4)
  (let-values
      (((r1 cpu1 real1 gc1) (time-apply opt:nth-fib-elem(list 100000)))
       ((r2 cpu2 real2 gc2) (time-apply nor:nth-fib-elem(list 100000))))
    (begin
      (display "Optimized alg.: (cpu, real, gc): ")
      (display cpu1)  (display ", ")
      (display real1) (display ", ")
      (display gc1)   (display "\n")
      (display "Normal impl.: (cpu, real, gc): ")
      (display cpu2)  (display ", ")
      (display real2) (display ", ")
      (display gc2)   (display "\n"))))


;;MAŁE LICZBY

(define (test-fib-small-nums)
  (begin
    (display "wait...\n")
    (flush-output (current-output-port))
    (test-performance5)))

(define (test-performance5)
  (let-values
      (((r1 cpu1 real1 gc1) (time-apply count-small-fibs (list 1)))                          
       ((r2 cpu2 real2 gc2) (time-apply count-small-fibs (list 2)))) 
    (begin
      (display "Normal impl.: (cpu, real, gc): ")
      (display cpu1)  (display ", ")
      (display real1) (display ", ")
      (display gc1)   (display "\n")
      (display "Optimized alg.: (cpu, real, gc): ")
      (display cpu2)  (display ", ")
      (display real2) (display ", ")
      (display gc2)   (display "\n"))))

(define (count-small-fibs x)
  (define (iter n)
    (if (= n 2)
        null
        (begin
          (if (= x 1) (nor:nth-fib-elem n) (opt:nth-fib-elem n))
          (iter (- n 1)))))
  (iter 1000))