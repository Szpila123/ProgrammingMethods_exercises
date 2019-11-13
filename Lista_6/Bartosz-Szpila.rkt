#lang racket
(require rackunit)
(require rackunit/text-ui)

;; procedury pomocnicze
(define (tagged-tuple? tag len x)
  (and (list? x)
       (=   len (length x))
       (eq? tag (car x))))

(define (tagged-list? tag x)
  (and (pair? x)
       (eq? tag (car x))
       (list? (cdr x))))

(define (same-thing? a b) ;;Sprawdzenie czy a to b
  (cond   [(and (null? a) (null? b)) #t]
          [(or (null? a) (null? b)) #f]
          [(and (pair? a) (pair? b))
           (and (same-thing? (car a) (car b))
                (same-thing? (cdr a) (cdr b)))]
          [(and (boolean? a) (boolean? b)) (not (xor a b))]
          [(and (number? a) (number? b)) (= a b)]
          [(and (symbol? a) (symbol? b)) (eq? a b)]
          [else #f]))

(define (contains? l elem) ;;Sprawdzenie czy lista zawiera element
  (cond [(null? l) #f]
        [(same-thing? (car l) elem) #t]
        [else (contains? (cdr l) elem)]))

(define (remove l elem) ;;Usuniecie wszystkich elementów równych elem z listy
  (cond [(null? l) null]
        [(same-thing? (car l) elem) (remove (cdr l) elem)]
        [else (cons (car l) (remove (cdr l) elem))]))


;; reprezentacja formuł w CNFie
;; zmienne
(define (var? x)
  symbol? x)

(define (var x)
  x)

(define (var-name x)
  x)

(define (var<? x y)
  (symbol<? x y))

;; literały
(define (lit pol var)
  (list 'lit pol var))
(require rackunit)
(require rackunit/text-ui)

(define (pos x)
  (lit true (var x)))

(define (neg x)
  (lit false (var x)))

(define (lit? x)
  (and (tagged-tuple? 'lit 3 x)
       (boolean? (second x))
       (var? (third x))))

(define (lit-pol l)
  (second l))

(define (lit-var l)
  (third l))

;; klauzule
(define (clause? c)
  (and (tagged-list? 'clause c)
       (andmap lit? (cdr c))))

(define (clause . lits)
  (cons 'clause lits))

(define (clause-lits c)
  (cdr c))

(define (cnf? f)
  (and (tagged-list? 'cnf f)
       (andmap clause? (cdr f))))

(define (cnf . clauses)
  (cons 'cnf clauses))

(define (cnf-clauses f)
  (cdr f))

;; definicja rezolucyjnych drzew wyprowadzenia
(define (axiom? p)
  (tagged-tuple? 'axiom 2 p))

(define (axiom c)
  (list 'axiom c))

(define (axiom-clause a)
  (second a))

(define (res? p)
  (tagged-tuple? 'resolve 4 p))

(define (res x pf-pos pf-neg)
  (list 'resolve x pf-pos pf-neg))

(define (res-var p)
  (second p))
(define (res-proof-pos p)
  (third p))
(define (res-proof-neg p)
  (fourth p))

(define (proof? p)
  (or (and (axiom? p)
           (clause? (axiom-clause p)))
      (and (res? p)
           (var? (res-var p))
           (proof? (res-proof-pos p))
           (proof? (res-proof-neg p)))))


;;Zadanie 1
(define (proof-result pf prop-cnf)
  ;;-----
  ;;Procedury sprawdzające czy axiom jest poprawny, czyli czy istnieje taka sama klauzula w naszej formule cnf
  
  (define (check prop-lits ax-lits) ;;Procedura sprawdza czy dwa zbiory literałow są sobie równe
    (cond [(and (null? ax-lits) (null? prop-lits))   #t]
          [(or (null? ax-lits) (null? prop-lits))    #f]
          [(not (contains? prop-lits (car ax-lits))) #f]
          [(check (remove prop-lits (car ax-lits)) (cdr ax-lits))]))
              
  (define (check-clause ax-clause prop-clauses) ;;Procedura sprawdza czy klauzula znajduje się w formule cnf
    (cond [(null? prop-clauses) #f]
          [(check (clause-lits (car prop-clauses)) (clause-lits ax-clause)) #t]
          [else (check-clause ax-clause (cdr prop-clauses))]))
  ;;--------
  ;;Procedury usuwające przeciwny element z klauzul i merge-ujące je
  (define (check-remove-merge var clause-pos clause-neg)
    (define (merge poss negs)
      (cond [(and (null? negs) (null? poss)) null]
            [(null? negs) (if (same-thing? (car poss) (lit #t var))
                              (merge (cdr poss) null)
                              (cons (car poss) (merge (cdr poss) null)))]
            [(same-thing? (car negs) (lit #f var)) (merge poss (cdr negs))]
            [else (cons (car negs) (merge poss (cdr negs)))]))

    (cond [(or (not (clause? clause-pos)) (not (clause? clause-neg))) #f]
          [(and (contains? clause-pos (lit #t var))
                (contains? clause-neg (lit #f var)))
           (cons 'clause (merge (clause-lits clause-pos) (clause-lits clause-neg)))]
          [else #f]))                                    

  ;;-------
  ;;Rekurencyjne odwołania
  (cond [(res? pf) (check-remove-merge (res-var pf)
                           (proof-result (res-proof-pos pf) prop-cnf)
                           (proof-result (res-proof-neg pf) prop-cnf))]
        [(axiom? pf) (if (check-clause (axiom-clause pf) (cnf-clauses prop-cnf)) (axiom-clause pf) #f)]
  )
)

(define (check-proof? pf prop)
  (let ((c (proof-result pf prop)))
    (and (clause? c)
         (null? (clause-lits c)))))


;;Zestaw testów do zadania pierwszego
(define cnf-1
  '(cnf (clause (lit #f p) (lit #t q))
        (clause (lit #t p) (lit #t q))
        (clause (lit #f q) (lit #t r))
        (clause (lit #f q) (lit #f r))))
(define proof-ok-1
  '(resolve q
            (resolve p (axiom (clause (lit #t p) (lit #t q)))
                       (axiom (clause (lit #f p) (lit #t q))))
            (resolve r (axiom (clause (lit #f q) (lit #t r)))
                       (axiom (clause (lit #f q) (lit #f r))))))
(define proof-ok1-1
  '(resolve p
            (resolve q (axiom (clause (lit #t p) (lit #t q)))
                       (resolve r (axiom (clause (lit #f q) (lit #t r)))
                                  (axiom (clause (lit #f q) (lit #f r)))))
            (resolve q (axiom (clause (lit #f p) (lit #t q)))
                       (resolve r (axiom (clause (lit #f q) (lit #t r)))
                                  (axiom (clause (lit #f q) (lit #f r)))))))

 (define cnf-2
   '(cnf (clause (lit #f r) (lit #f p))
         (clause (lit #t r) (lit #f p))
         (clause (lit #t p))))

(define proof-ok-2
  '(resolve p
            (axiom (clause (lit #t p)))
            (resolve r (axiom (clause (lit #t r) (lit #f p)))
                       (axiom (clause (lit #f r) (lit #f p))))))


(define proof-bad1-1
  '(resolve q
            (resolve k (axiom (clause (lit #t p) (lit #t q)))
                       (axiom (clause (lit #f p) (lit #t q))))
            (resolve r (axiom (clause (lit #f q) (lit #t r)))
                       (axiom (clause (lit #f q) (lit #f r))))))

(define proof-bad2-1
  '(resolve q
            (resolve p (axiom (clause (lit #f p) (lit #t q)))
                       (axiom (clause (lit #t p) (lit #t q))))
            (resolve r (axiom (clause (lit #f q) (lit #t r)))
                       (axiom (clause (lit #f q) (lit #f r))))))

(define proof-bad3-1
  '(resolve q
            (resolve p (axiom (clause (lit #t p) (lit #t q)))
                       (axiom (clause (lit #f p) (lit #t p))))
            (resolve r (axiom (clause (lit #f q) (lit #t r)))
                       (axiom (clause (lit #f q) (lit #f r))))))

(define proof-checking-tests
  (test-suite
   "Testing the proof cehck procedure"
   (check-equal? (check-proof?  proof-ok-1 cnf-1) #t
                 "Checking a correct proof")
   (check-equal? (check-proof?  proof-ok1-1 cnf-1) #t
                 "Checking a correct proof")
   (check-equal? (check-proof?  proof-ok-2 cnf-2) #t
                 "Checking a correct proof")
   (check-equal? (check-proof? proof-bad1-1 cnf-1) #f
                 "Checking an incorrect proof, where resolve is made with a veriable that isnt in a clause")
   (check-equal? (check-proof? proof-bad2-1 cnf-1) #f
                 "Checking an incorrect proof, where clauses with positive and negative literal are switched")
   (check-equal? (check-proof? proof-bad3-1 cnf-1) #f
                 "Checking an incorrect proof, where clauses within axiom dont occure in the formula")))

(run-tests proof-checking-tests)



;;---------------------------------------------------------------------------------------------------------------
;;ZADANIE 2

;; Wewnętrzna reprezentacja klauzul

(define (sorted? ord? xs)
  (or (null? xs)
      (null? (cdr xs))
      (and (ord? (car xs)
                (cadr xs))
           (sorted? ord? (cdr xs)))))

(define (sorted-varlist? xs)
  (and (andmap var? xs)
       (sorted? var<? xs)))

(define (res-clause pos neg pf)
  (list 'res-clause pos neg pf))

(define (res-clause-pos rc)
  (second rc))
(define (res-clause-neg rc)
  (third rc))
(define (res-clause-proof rc)
  (fourth rc))

(define (res-clause? p)
  (and (tagged-tuple? 'res-clause 4 p)
       (sorted-varlist? (second p))
       (sorted-varlist? (third  p))
       (proof? (fourth p))))

;; implementacja zbiorów / kolejek klauzul do przetworzenia
(define (subsumes res-cl1 res-cl2)
  (define (check-lits lits1 lits2)
    (cond [(null? lits1) #t]
          [(contains? lits2 (car lits1)) (check-lits (cdr lits1) lits2)]
          [else #f]))
  (and (check-lits (res-clause-neg res-cl1) (res-clause-neg res-cl2))
       (check-lits (res-clause-pos res-cl1) (res-clause-pos res-cl2))))

(define (single? cl)
  (or (and (not (null? (res-clause-pos cl)))
           (null? (cdr (res-clause-pos cl)))
           (null? (res-clause-neg cl)))
      (and (null? (res-clause-pos cl))
           (not (null? (res-clause-neg cl)))
           (null? (cdr (res-clause-neg cl))))))

(define (contains-easier-cl ls-cl ncl)
  (cond [(null? ls-cl) #f]
        [(subsumes (car ls-cl) ncl) #t]
        [else (contains-easier-cl (cdr ls-cl) ncl)]))


(define (single-lit cl) ;;the result is the literal of single lit. clause
  (if (not (single? cl))
      (error "Something went wrong, cnot single clause is said to be single")
      (if (null? (res-clause-pos cl))
          (car (res-clause-neg cl))
          (car (res-clause-pos cl)))))

(define (clause-set-map proc set)
  (cond [(null? set) null]
        [(boolean? (proc (car set))) (cons (car set) (clause-set-map proc (cdr set)))]
        [else (cons (proc (car set)) (clause-set-map proc (cdr set)))]))


(define clause-set-empty
  '(stop () ()))

(define (clause-set-add rc rc-set)

  
  (define (eq-cl? sc)
    (and (equal? (res-clause-pos rc)
                 (res-clause-pos sc))
         (equal? (res-clause-neg rc)
                 (res-clause-neg sc))))

  
  (define (add-to-stopped sset)
    (let ((procd  (cadr  sset))
          (toproc (caddr sset)))
      (cond
       [(null? procd) (list 'stop (list rc) '())]
       [(or (memf eq-cl? procd)
            (memf eq-cl? toproc))
        sset]
       [(single? rc) (list 'stop ;;If rc is one literal clause, remove all easier clauses and resolve l with everything left
                           (filter (λ (x) (not (contains? x (single-lit rc)))) (clause-set-map (λ (x) (rc-resolve rc x)) procd))
                           (filter (λ (x) (not (contains? x (single-lit rc)))) (clause-set-map (λ (x) (rc-resolve rc x)) toproc)))]
       [else (list 'stop ;When adding to stopped, remove all easier clauses from to-process
                   procd
                   (if (or (contains-easier-cl procd rc) ;if harder clause isnt already in the set then add rc
                           (contains-easier-cl toproc rc))
                       (filter (λ (x) (not (subsumes rc x))) toproc)
                       (cons rc (filter (λ (x) (not (subsumes rc x)))toproc))))])))

  
  (define (add-to-running rset)
    (let ((pd  (second rset))
          (tp  (third  rset))
          (cc  (fourth rset))
          (rst (fifth  rset)))
      (cond
        [(or (memf eq-cl? pd)
             (memf eq-cl? tp)
             (eq-cl? cc)
             (memf eq-cl? rst))
         rset]
        [(single? rc) (list 'run ;If the new clause is a single literal
                            (filter (λ (x) (not (contains? x (single-lit rc)))) (clause-set-map (λ (x) (rc-resolve rc x)) pd));;remove easier clauses from everything, and resolve rc with everything
                            (filter (λ (x) (not (contains? x (single-lit rc)))) (clause-set-map (λ (x) (rc-resolve rc x)) tp))
                            (if (rc-resolve cc rc)
                                (rc-resolve cc rc)
                                cc)
                            (filter (λ (x) (not (contains? x (single-lit rc)))) (clause-set-map (λ (x) (rc-resolve rc x)) rst)))]
                            
        [else (list 'run
                    (filter (λ (x) (not (subsumes rc x))) pd);When adding to running, remove all easier clauses from processed and rest
                    tp
                    cc
                    (if (or (contains-easier-cl pd rc)
                            (contains-easier-cl tp rc)
                            (contains-easier-cl rst rc)
                            (subsumes cc rc))
                        (filter (λ (x) (not (subsumes rc x))) rst) ;If harder clause is already in resolution-set dont add new clause
                        (cons rc (filter (λ (x) (not (subsumes rc x))) rst))))])))

  
  (if (eq? 'stop (car rc-set)) ;;Definition of clause-set-add
      (add-to-stopped rc-set)
      (add-to-running rc-set)))

(define (clause-set-done? rc-set)
  (and (eq? 'stop (car rc-set))
       (null? (caddr rc-set))))

(define (clause-set-next-pair rc-set)
  (define (aux rset)
    (let* ((pd  (second rset))
           (tp  (third  rset))
           (nc  (car tp))
           (rtp (cdr tp))
           (cc  (fourth rset))
           (rst (fifth  rset))
           (ns  (if (null? rtp)
                    (list 'stop (cons cc (cons nc pd)) rst)
                    (list 'run  (cons nc pd) rtp cc rst))))
      (cons cc (cons nc ns))))
  (if (eq? 'stop (car rc-set))
      (let ((pd (second rc-set))
            (tp (third  rc-set)))
        (aux (list 'run '() pd (car tp) (cdr tp))))
      (aux rc-set)))

(define (clause-set-done->clause-list rc-set)
  (and (clause-set-done? rc-set)
       (cadr rc-set)))

;; konwersja z reprezentacji wejściowej na wewnętrzną

(define (clause->res-clause cl)
  (let ((pos (filter-map (lambda (l) (and (lit-pol l) (lit-var l)))
                         (clause-lits cl)))
        (neg (filter-map (lambda (l) (and (not (lit-pol l)) (lit-var l)))
                         (clause-lits cl)))
        (pf  (axiom cl)))
    (res-clause (sort pos var<?) (sort neg var<?) pf)))
;;-------------------------
;; tu zdefiniuj procedury pomocnicze, jeśli potrzebujesz
(define (merge l1 l2)
  (cond [(null? l1) l2]
        [(null? l2) l1]
        [(eq? (car l1) (car l2)) (cons (car l1) (merge (cdr l1) (cdr l2)))]
        [(var<? (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2))]
        [else (cons (car l2) (merge l1 (cdr l2)))]))

(define (common l1 l2)
  (cond [(null? l1) #f]
        [(contains? l2 (car l1)) (car l1)] ;;definicja contains? w zad. 1
        [else (common (cdr l1) l2)]))
;;--------------------------
;; PROCEDURY Z ZADANIA                  
(define (rc-trivial? rc)
  (symbol? (common (res-clause-pos rc) (res-clause-neg rc)))) ;;klauzula jest trywialna, gdy zawiera tę samą zmienną z negacją i bez

(define (rc-resolve rc1 rc2)                                                 
  (let ((var-rmv (common (res-clause-pos rc1) (res-clause-neg rc2)))) ;;var-rmv to literał do usuniecia z poz. lit. rc1 i neg. rc2 lub #f jeśli takiego nie ma
    (if (not (symbol? var-rmv))  ;;jeśli takiego nie ma (czyli var-rmv to  #f) to sprawdzmy neg. rc1 i poz. rc2
        (let ((var-rmv (common (res-clause-neg rc1) (res-clause-pos rc2))))
          (if (symbol? var-rmv)
              (res-clause (merge (res-clause-pos rc1) (remove (res-clause-pos rc2) var-rmv)) ;;w przyp. znalezienia takiego lit budowa klauzuli analogiczna do tej na dole
                          (merge (remove (res-clause-neg rc1) var-rmv) (res-clause-neg rc2)) ;;definicja remove w zad. 1
                          (res var-rmv (res-clause-proof rc2) (res-clause-proof rc1)))                                 
               #f))  ;;fałsz zwracamy w przypadku gdy nie znaleźliśmy żadnego literału do usunięcia
        (res-clause (merge (remove (res-clause-pos rc1) var-rmv) (res-clause-pos rc2)) ;;gdy usuwana zmienna jest poz w rc1 i neg w rc2 zwróc klauzulę taką że, 
                    (merge (res-clause-neg rc1) (remove (res-clause-neg rc2) var-rmv)) ;;z poz rc1 i neg rc2 usuwamy wszystkie wystąpienia var-rmv
                    (res var-rmv (res-clause-proof rc1) (res-clause-proof rc2))))))    ;;i merg. pozostałe poz rc1 z pos rc2 oraz neg rc1 z pozostałymi neg rc2 zachowując porządek
                                                                                       ;;dowód takiej klauzuli to odpowiedznie połączenie dowodów rc1 i rc2 za pomocą res
;;----------------------------
(define (fixed-point op start)
  (let ((new (op start)))
    (if (eq? new false)
        start
        (fixed-point op new))))

(define (cnf->clause-set f)
  (define (aux cl rc-set)
    (clause-set-add (clause->res-clause cl) rc-set))
  (foldl aux clause-set-empty (cnf-clauses f)))

(define (get-empty-proof rc-set)
  (define (rc-empty? c)
    (and (null? (res-clause-pos c))
         (null? (res-clause-neg c))))
  (let* ((rcs (clause-set-done->clause-list rc-set))
         (empty-or-false (findf rc-empty? rcs)))
    (and empty-or-false
         (res-clause-proof empty-or-false))))

(define (improve rc-set)
  (if (clause-set-done? rc-set)
      false
      (let* ((triple (clause-set-next-pair rc-set))
             (c1     (car  triple))
             (c2     (cadr triple))
             (rc-set (cddr triple))
             (c-or-f (rc-resolve c1 c2)))
        (if (and c-or-f (not (rc-trivial? c-or-f)))
            (clause-set-add c-or-f rc-set)
            rc-set))))

(define (prove cnf-form)
  (let* ((clauses (cnf->clause-set cnf-form))
         (sat-clauses (fixed-point improve clauses))
         (pf-or-false (get-empty-proof sat-clauses)))
    (if (eq? pf-or-false false)
        'sat
        (list 'unsat pf-or-false))))

;;Zestaw testów do zadania drugiego
(define cnf-3  ;;Unprovable
  '(cnf (clause (lit #t a) (lit #f b))))
(define cnf-4  ;;In 3-CNF, provable
  '(cnf (clause (lit #t x) (lit #t y) (lit #f z))
        (clause (lit #t x) (lit #f y) (lit #t z))
        (clause (lit #t x) (lit #f y) (lit #f z))
        (clause (lit #f x) (lit #t y) (lit #t z))
        (clause (lit #f x) (lit #t y) (lit #f z))
        (clause (lit #f x) (lit #f y) (lit #t z))
        (clause (lit #f x) (lit #f y) (lit #f z))
        (clause (lit #t x) (lit #t y) (lit #t z))))

(define cnf-5  ;;Same as above, but true for all true
  '(cnf (clause (lit #t x) (lit #t y) (lit #f z))
        (clause (lit #t x) (lit #f y) (lit #t z))
        (clause (lit #t x) (lit #f y) (lit #f z))
        (clause (lit #f x) (lit #t y) (lit #t z))
        (clause (lit #f x) (lit #t y) (lit #f z))
        (clause (lit #f x) (lit #f y) (lit #t z))
        (clause (lit #t x) (lit #t y) (lit #t z))))

(define cnf-6  ;;Formula trivial always false
  '(cnf (clause (lit #t p) (lit #f p))
        (clause (lit #t p))
        (clause (lit #f p))))

(define cnf-7
  '(cnf (clause (lit #t w) (lit #t k) (lit #t s))
        (clause (lit #f w) (lit #f k))
        (clause (lit #f k) (lit #t w))))

(define proof-making-tests
  (test-suite
   "Tests for creating proves"
   (check-equal? (check-proof? (cadr (prove cnf-1)) cnf-1) #t
                 "Proving for provable formula")
   (check-equal? (check-proof? (cadr (prove cnf-2)) cnf-2) #t
                 "Proving for provable formula")
   (check-equal? (prove cnf-3) 'sat
                 "Proving of unprovable formula")
   (check-equal? (check-proof? (cadr (prove cnf-4)) cnf-4) #t
                 "Proving of provable formula in 3-CNF")
   (check-equal? (prove cnf-5) 'sat
                 "Proving of unprovable formula in 3-CNF")
   (check-equal? (car (prove cnf-6)) 'unsat
                 "Proving of formula that is mostly trivial and always false")
 ))
(run-tests proof-making-tests)


;;------------------------------------------------------------------------------------------------------------------
;;LISTA 6 ZADANIE 1

;;subsumes oraz kilka funkcji pomocniczych zdefiniowane przy implementacji zbiorów / kolejek klauzul
;;reszta zmian w definicji clause-set-add