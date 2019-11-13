#lang racket
(require rackunit)
(require rackunit/text-ui)


(define (inc n)
  (+ n 1))

;;; tagged lists
(define (tagged-list? len-xs tag xs)
  (and (list? xs)
       (= len-xs (length xs))
       (eq? (first xs) tag)))

;;; ordered elements
(define (make-elem pri val)
  (cons pri val))

(define (elem-priority x)
  (car x))

(define (elem-val x)
  (cdr x))

;;; leftist heaps (after Okasaki)

;; data representation
(define leaf 'leaf)

(define (leaf? h) (eq? 'leaf h))

(define (hnode? h)
  (and (tagged-list? 5 'hnode h)
       (natural? (caddr h))))

(define (make-hnode elem heap-a heap-b)
  (cond [(leaf? heap-a) (list 'hnode elem 1 heap-b heap-a)]
        [(leaf? heap-b) (list 'hnode elem 1 heap-a heap-b)]
        [(>= (hnode-rank heap-a) (hnode-rank heap-b)) (list 'hnode elem (+ (hnode-rank heap-b) 1) heap-a heap-b)]
        [else (list 'hnode elem (+ (hnode-rank heap-a) 1) heap-b heap-a)])
  )

(define (hnode-elem h)
  (second h))

(define (hnode-left h)
  (fourth h))

(define (hnode-right h)
  (fifth h))

(define (hnode-rank h)
  (third h))

(define (hord? p h)
  (or (leaf? h)
      (<= p (elem-priority (hnode-elem h)))))

(define (heap? h)
  (or (leaf? h)
      (and (hnode? h)
           (heap? (hnode-left h))
           (heap? (hnode-right h))
           (<= (rank (hnode-right h))
               (rank (hnode-left h)))
           (= (rank h) (inc (rank (hnode-right h))))
           (hord? (elem-priority (hnode-elem h))
                  (hnode-left h))
           (hord? (elem-priority (hnode-elem h))
                  (hnode-right h)))))

(define (rank h)
  (if (leaf? h)
      0
      (hnode-rank h)))

;; operations

(define empty-heap leaf)

(define (heap-empty? h)
  (leaf? h))

(define (heap-insert elt heap)
  (heap-merge heap (make-hnode elt leaf leaf)))

(define (heap-min heap)
  (hnode-elem heap))

(define (heap-pop heap)
  (heap-merge (hnode-left heap) (hnode-right heap)))

(define (heap-merge h1 h2)
  (cond
   [(leaf? h1) h2]
   [(leaf? h2) h1]
   [(< (elem-priority (hnode-elem h1)) (elem-priority (hnode-elem h2)) ) (make-hnode (hnode-elem h1) (hnode-left h1) (heap-merge (hnode-right h1) h2))]
   [else (make-hnode (hnode-elem h2) (hnode-left h2) (heap-merge (hnode-right h2) h1))] 
   ))


;;; heapsort. sorts a list of numbers.
(define (heapsort xs)
  (define (popAll h)
    (if (heap-empty? h)
        null
        (cons (elem-val (heap-min h)) (popAll (heap-pop h)))))
  (let ((h (foldl (lambda (x h)
                    (heap-insert (make-elem x x) h))
            empty-heap xs)))
    (popAll h)))

;;; check that a list is sorted (useful for longish lists)
(define (sorted? xs)
  (cond [(null? xs)              true]
        [(null? (cdr xs))        true]
        [(<= (car xs) (cadr xs)) (sorted? (cdr xs))]
        [else                    false]))

;;; generate a list of random numbers of a given length
(define (randlist len max)
  (define (aux len lst)
    (if (= len 0)
        lst
        (aux (- len 1) (cons (random max) lst))))
  (aux len null))

;;; Testing
(define list-1 (randlist 10 100))
(define list-2 (randlist 50 300))
(define list-3 '() )
(define list-4 '(1 2 3))
(define list-5 '(1))


(define heap-tests
  (test-suite
   "Testing the heap-merge and make-hnode procedures with heapsort procedure"
   (check-equal? (sorted? (heapsort list-1)) #t
                 "Sorting of a random list")
   (check-equal? (sorted? (heapsort list-2)) #t
                 "Sorting of a random list")
   (check-equal? (sorted? (heapsort list-3)) #t
                 "Sorting of an empty list")
   (check-equal? (sorted? (heapsort list-4)) #t
                 "Sorting of a sorted list")
   (check-equal? (sorted? (heapsort list-5)) #t
                 "Sorting of a one-elem list")))

(run-tests heap-tests)
