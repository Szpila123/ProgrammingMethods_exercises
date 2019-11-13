#lang racket
(require rackunit)
(require rackunit/text-ui)

(define (length-it xs)
  (define (iter xs n)
    (if (null? xs)
        n
        (iter (cdr xs) (+ n 1))))
  (iter xs 0))

(define (append as bs)
  (if (null? as)
      bs
      (cons (car as)
            (append (cdr as) bs))))

(define (split l)
  (define (iter l xl)
    (if (>= (length-it xl) (length-it l))
        (cons xl l)
        (iter (cdr l) (append xl (cons (car l) null)))))
  (if (null? l)
      (cons null null)
      (if (= 1 (length-it l))
          (cons l null)
          (iter l null))))
    
(define (merge a b)
  (define (rec a b l)
    (cond [(null? a) (append l b)]
          [(null? b) (append l a)]
          [else (if (> (car a) (car b))
                    (rec a (cdr b) (append l (cons (car b) null)))
                    (rec (cdr a) b (append l (cons (car a) null))))]
          )
    )
  (rec a b null))

(define (merge-sort l)
  (cond [(null? l) null]
        [(> (length-it l) 1 )
         (let (( a (split l)))
           (merge (merge-sort (car a))
                  (merge-sort (cdr a))))]
        [else l]))

(define merge-tests
  (test-suite
   "Tests of lists merging"
   (check-equal? (merge '() '()) '()
                 "Merging of two empty lists is an empty list")
 
   (check-equal? (merge '(1 2 3) '()) '(1 2 3)
                  "Merging a list and an empty list is a first list")
   
   (check-equal? (merge '() '(1 2 3)) '(1 2 3)
                  "Merging an empty list and a second list")
  
   (check-equal? (merge '(2 4 6 7 9) '(1 3 4)) '(1 2 3 4 4 6 7 9)
                  "Merging two sorted lists is a sorted list")
   
    (check-equal? (merge '(2 2) '(1 3 4)) '(1 2 2 3 4)
                  "Merging two sorted lists is a sorted list"))
   )

(define split-tests
  (test-suite
   "Tests of list splitting"
   (check-equal? (split '()) '(())
                 "Splitting of an empty list is a pair of nulls")
 
   (check-equal? (split '(1)) '((1))
                  "Splitting a list of a length 1 is a pair of this list and null")
   
   (check-equal? (split '(1 2 3)) '((1 2) 3)
                  "Splitting a list creates a pair of two lists of the same lenght or first list one node longer than second")
   
   (check-equal? (split '(1 2 3 4)) '((1 2) 3 4)
                  "Splitting a list creates a pair of two lists of the same lenght or first list one node longer than second")
   
   (check-equal? (split '(1 3 5 7 9 0 2 4 6 8 )) '((1 3 5 7 9) 0 2 4 6 8)
                  "Splitting a list creates a pair of two lists of the same lenght or first list one node longer than second")))

(define merge-sort-tests
  (test-suite
   "Tests of sorting the lists with merge-sort"
   (check-equal? (merge-sort '()) '()
                 "Sorting an empty lists returns an empty list")

   (check-equal? (merge-sort '(1)) '(1)
                 "Sorting a lists of lenght one returns that list")

   (check-equal? (merge-sort '(1 2 3)) '(1 2 3)
                 "Sorting of a sorted list returns that list")

   (check-equal? (merge-sort '(6 78 3563 83 3 4 5 7 651 2 3 4 5)) '(2 3 3 4 4 5 5 6 7 78 83 651 3563)
                 "Sorting of an unsorted list returns sorted lists consisting of the same elements")))
           
    
      
  
      