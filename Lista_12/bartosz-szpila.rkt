#lang racket
;; sygnatura: grafy
(define-signature graph^
  ((contracted
    [graph        (-> list? (listof edge?) graph?)]
    [graph?       (-> any/c boolean?)]
    [graph-nodes  (-> graph? list?)]
    [graph-edges  (-> graph? (listof edge?))]
    [edge         (-> any/c any/c edge?)]
    [edge?        (-> any/c boolean?)]
    [edge-start   (-> edge? any/c)]
    [edge-end     (-> edge? any/c)]
    [has-node?    (-> graph? any/c boolean?)]
    [outnodes     (-> graph? any/c list?)]
    [remove-node  (-> graph? any/c graph?)]
    )))

;; prosta implementacja grafów
(define-unit simple-graph@
  (import)
  (export graph^)

  (define (graph? g)
    (and (list? g)
         (eq? (length g) 3)
         (eq? (car g) 'graph)))

  (define (edge? e)
    (and (list? e)
         (eq? (length e) 3)
         (eq? (car e) 'edge)))

  (define (graph-nodes g) (cadr g))

  (define (graph-edges g) (caddr g))

  (define (graph n e) (list 'graph n e))

  (define (edge n1 n2) (list 'edge n1 n2))

  (define (edge-start e) (cadr e))

  (define (edge-end e) (caddr e))

  (define (has-node? g n) (not (not (member n (graph-nodes g)))))
  
  (define (outnodes g n)
    (filter-map
     (lambda (e)
       (and (eq? (edge-start e) n)
            (edge-end e)))
     (graph-edges g)))

  (define (remove-node g n)
    (graph
     (remove n (graph-nodes g))
     (filter
      (lambda (e)
        (not (eq? (edge-start e) n)))
      (graph-edges g)))))

;; sygnatura dla struktury danych
(define-signature bag^
  ((contracted
    [bag?       (-> any/c boolean?)]
    [empty-bag  (and/c bag? bag-empty?)]
    [bag-empty? (-> bag? boolean?)]
    [bag-insert (-> bag? any/c (and/c bag? (not/c bag-empty?)))]
    [bag-peek   (-> (and/c bag? (not/c bag-empty?)) any/c)]
    [bag-remove (-> (and/c bag? (not/c bag-empty?)) bag?)])))

;; struktura danych - stos
(define-unit bag-stack@
  (import)
  (export bag^)

;;implementacja stosu
  (define (bag? s) (and (list? s)
                        (eq? (length s) 2)
                        (eq? (car s) 'stack)))
  (define empty-bag (list 'stack '()))
  (define (bag-empty? s) (null? (cadr s)))
  (define (bag-insert s v) (list 'stack (cons v (cadr s))))
  (define (bag-peek s) (caadr s))
  (define (bag-remove s) (list 'stack (cdadr s))))

;; struktura danych - kolejka FIFO
;; do zaimplementowania przez studentów
(define-unit bag-fifo@
  (import)
  (export bag^)
  
;; implementacja kolejki
  (define (bag? s) (and (list? s)
                        (eq? (length s) 3)
                        (eq? (car s) 'queue)))
  (define empty-bag (list 'queue '() '() ))
  (define (bag-empty? s) (and (null? (cadr s))
                              (null? (caddr s))))
  (define (bag-insert s v) (flip-bag (list 'queue (cons v (cadr s)) (caddr s))))
  (define (bag-peek s) (caaddr s))
  (define (bag-remove s) (flip-bag (list 'queue (cadr s) (cdaddr s))))
  (define (flip-bag s) (if (null? (caddr s))
                           (list 'queue '() (reverse (cadr s)))
                           s)))

;; sygnatura dla przeszukiwania grafu
(define-signature graph-search^
  (search))

;; implementacja przeszukiwania grafu
;; uzależniona od implementacji grafu i struktury danych
(define-unit/contract graph-search@
  (import bag^ graph^)
  (export (graph-search^
           [search (-> graph? any/c (listof any/c))]))
  (define (search g n)
    (define (it g b l)
      (cond
        [(bag-empty? b) (reverse l)]
        [(has-node? g (bag-peek b))
         (it (remove-node g (bag-peek b))
             (foldl
              (lambda (n1 b1) (bag-insert b1 n1))
              (bag-remove b)
              (outnodes g (bag-peek b)))
             (cons (bag-peek b) l))]
        [else (it g (bag-remove b) l)]))
    (it g (bag-insert empty-bag n) '()))
  )

;; otwarcie komponentu grafu
(define-values/invoke-unit/infer simple-graph@)

;; graf testowy
(define test-graph-1
  (graph
   (list 1 2 3 4)
   (list (edge 1 3)
         (edge 1 2)
         (edge 2 4))))

;; inne testowe grafy
(define test-graph-2
  (graph
   (list 1 2 3 4)
   (list (edge 1 3)
         (edge 3 2)
         (edge 4 2))))

(define test-graph-no-edges
  (graph
   (list 1 2 3 4)
   null))

(define test-graph-empty
  (graph
   null
   null))

(define test-graph-3
  (graph
   (list 1 2 3 4 5)
   (list (edge 2 1)
         (edge 1 2)
         (edge 1 3)
         (edge 4 3)
         (edge 4 5))))


;; otwarcie komponentu stosu
 (define-values/invoke-unit/infer bag-stack@)
;; opcja 2: otwarcie komponentu kolejki
; (define-values/invoke-unit/infer bag-fifo@)

;; testy w Quickchecku
(require quickcheck)

;; test przykładowy: jeśli do pustej struktury dodamy element
;; i od razu go usuniemy, wynikowa struktura jest pusta
(quickcheck
 (property ([s arbitrary-symbol])
           (bag-empty? (bag-remove (bag-insert empty-bag s)))))
;; inne własności do sprawdzenia

;; po dodaniu elementu do pustej torby, torba nie jest pusta (już zagwarantowane przez kontrakt)
(quickcheck
 (property ([s arbitrary-symbol])
           (not (bag-empty? (bag-insert empty-bag s)))))

;; dodając elementy (lub nie jeśli lista wylosuje się pusta) do niepustej torby, torba będzie nie pusta
(quickcheck
 (property ([l (arbitrary-list arbitrary-integer)])
          (not (bag-empty? (foldl (λ (x bag) (bag-insert bag x)) (bag-insert empty-bag 1) l)))))

;; TYLKO STOS po dodaniu elementu do jakiejkolwiek torby, bag-peek powinien zwrócić ostatni dodany element
(quickcheck
 (property ([l (arbitrary-list arbitrary-symbol)] [s arbitrary-symbol])
           (eq? (bag-peek (bag-insert (foldr (λ (x bag) (bag-insert bag x)) empty-bag l) s)) s)))

;; TYLKO STOS po dodaniu (>= 2) elementów do jakiejkolwiek torby i usunięciu jednego elementu, bag-peek powinien zwrócić przedostatni dodany
(quickcheck
 (property ([l (arbitrary-list arbitrary-symbol)] [s1 arbitrary-symbol] [s2 arbitrary-symbol])
           (eq? (bag-peek (bag-remove (bag-insert (bag-insert (foldr (λ (x bag) (bag-insert bag x)) empty-bag l)  s2) s1))) s2)))

;; TYLKO KOLEJKA po dodaniu elementów do jakiejkolwiek torby, bag-peek powinien zwrócić pierwszy w kolejności dodany element
(quickcheck
 (property ([l (arbitrary-list arbitrary-symbol)] [s arbitrary-symbol])
           (eq? (bag-peek (foldr (λ (x bag) (bag-insert bag x)) (bag-insert empty-bag s) l)) s)))

;;TYLKO KOLEJKA po dodaniu elementów do jakiejkolwiek torby i usunięciu jednego, bag-peek powinien zwrócić drugi w kolejności dodany element
(quickcheck
 (property ([l (arbitrary-list arbitrary-symbol)] [s1 arbitrary-symbol] [s2 arbitrary-symbol])
           (eq? (bag-peek (bag-remove (foldr (λ (x bag) (bag-insert bag x)) (bag-insert (bag-insert empty-bag s2) s1) l))) s1)))
;; jeśli jakaś własność dotyczy tylko stosu lub tylko kolejki,
;; wykomentuj ją i opisz to w komentarzu powyżej własności

;; otwarcie komponentu przeszukiwania
(define-values/invoke-unit/infer graph-search@)

;; uruchomienie przeszukiwania na przykładowym grafie
(search test-graph-1 1)

;; przeszukiwanie na przykładowych grafach (wyniki są takie same, tylko węzły są odwiedzane w innej kolejności)
(search test-graph-2 1)
(search test-graph-no-edges 3)
(search test-graph-empty 1)
(search test-graph-3 1)
(search test-graph-3 2)
(search test-graph-3 4)

 