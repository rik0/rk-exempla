;; Copyright (c) 2010 by Enrico Franchi

;; (define-syntax when
;;   (syntax-rules ()
;;     [(_ c b1 ...)
;;      (cond (c b1 ...) (else #f))]))

;; (define-syntax unless
;;   (syntax-rules ()
;;     [(_ c b1 ...)
;;      (cond (c #f) (else b1 ...))]))

;;; function sort from file "Sort.scm", Time-stamp: <2008-03-18 15:21:35 feeley>
;;; Copyright (c) 2006-2008 by Marc Feeley, All Rights Reserved.
(define (sort sequence less?)
  (define (sort-list lst less?)
    
    (define (mergesort lst)

      (define (merge lst1 lst2)
        (cond ((not (pair? lst1))
               lst2)
              ((not (pair? lst2))
               lst1)
              (else
               (let ((e1 (car lst1)) (e2 (car lst2)))
                 (if (less? e1 e2)
                     (cons e1 (merge (cdr lst1) lst2))
                     (cons e2 (merge lst1 (cdr lst2))))))))

      (define (split lst)
        (if (or (not (pair? lst)) (not (pair? (cdr lst))))
            lst
            (cons (car lst) (split (cddr lst)))))

      (if (or (not (pair? lst)) (not (pair? (cdr lst))))
          lst
          (let* ((lst1 (mergesort (split lst)))
                 (lst2 (mergesort (split (cdr lst)))))
            (merge lst1 lst2))))

    (mergesort lst))

  (cond ((not (procedure? less?))
         (error "procedure expected"))
        ((or (null? sequence)
             (pair? sequence))
         (sort-list sequence less?))
        ((vector? sequence)
         (list->vector (sort-list (vector->list sequence) less?)))
        (else
         (error "vector or list expected"))))


;; Copyright (c) 2010 by Enrico Franchi

(define (add1 n) (+ n 1))

(define (build-list how-many constructor)
  (let loop ([n 0] [lst '()])
    (cond
      ((= n how-many) lst)
      (else
        (loop (add1 n) (cons (constructor n) lst))))))

(define foldl
  (lambda (f z lst)
    (let loop ((acc z) (lst lst))
      (cond
       ((null? lst) acc)
       (else
	(loop (f (car lst) acc) (cdr lst)))))))

;; Uses a named let to perform recursion
(define simplify-by-weight-a
  (lambda (weight ch n digits)
    (let loop ([n n] [digits digits])
      (cond ((< n weight) (cons n digits))
            (else 
             (loop (- n weight) (cons ch digits)))))))


;; Uses a standard recursive call
(define simplify-by-weight-b
  (lambda (weight ch n digits)
    (letrec ([sbw 
              (lambda (n digits)
                (cond ((< n weight) (cons n digits))
                      (else 
                       (sbw (- n weight) (cons ch digits)))))])
      (sbw n digits))))

;; Uses simplify-by-weight-a, which
;; Uses a named let to perform recursion
(define simplify-all-a
  (lambda (weights n digits)
    (cond 
      ((null? weights) digits)
      (else
       (let* ([ret (simplify-by-weight-a (caar weights) (cadar weights) n digits)]
              [n (car ret)]
              [digits (cdr ret)])
         (simplify-all-a (cdr weights) n digits))))))

;; Uses simplify-by-weight-b, which
;; Uses a standard recursive call
(define simplify-all-b
  (lambda (weights n digits)
    (cond 
      ((null? weights) digits)
      (else
       (let* ([ret (simplify-by-weight-b (caar weights) (cadar weights) n digits)]
              [n (car ret)]
              [digits (cdr ret)])
         (simplify-all-b (cdr weights) n digits))))))

;; Embeds simplify-by-weight-a as a subprocedure
(define simplify-all-c
  (lambda (weights n digits)
    (cond 
     ((null? weights) digits)
      (else
       (let* ([weight (caar weights)]
              [ch (cadar weights)]
              [ret (let loop ([n n] [digits digits])
                     (cond ((< n weight) (cons n digits))
                           (else 
                            (loop (- n weight) (cons ch digits)))))]
              [n (car ret)]
              [digits (cdr ret)])
         (simplify-all-c (cdr weights) n digits))))))

;; Imperative procedure: uses state-change. 
;; Uses for-each.
(define simplify-all-d
  (lambda (weights n digits)
    (for-each 
     (lambda (weight-pair)
       (let  ([weight (car weight-pair)]
              [ch (cadr weight-pair)])
         (let simplify-again ()
           (cond
             ((< n weight) #f)
             (else
              (set! n (- n weight))
              (set! digits (cons ch digits))
              (simplify-again))))))
     weights)
    digits))
 
;; Uses two nested named loops. Functional.
(define simplify-all-e
  (lambda (weights n digits)
    (let next-weight ([n n]
                      [digits digits]
                      [weight-pair (car weights)]
                      [weights (cdr weights)])
      (let  ([weight (car weight-pair)]
             [ch (cadr weight-pair)])
        (let simplify-again ([n n] [digits digits])
          (cond
            ((< n weight)
             (cond 
               ((null? weights) digits)
               (else (next-weight n digits (car weights) (cdr weights)))))
            (else
             (simplify-again (- n weight) (cons ch digits)))))))))                   
                  

(define (integer->roman-builder simplify-all)
  (let ([weights (sort '((1 "I") (4 "IV") (5 "V") (9 "IX") (10 "X")
                                 (40 "XL") (50 "L") (90 "XC") (100 "C")
                                 (400 "CD") (500 "D") (900 "CM"))
                       (lambda (left right) (> (car left) (car right))))])
    (lambda (n)
      (let ([thousands (quotient n 1000)]
            [n (remainder n 1000)])
        (foldl string-append ""
               (simplify-all weights n
                             (build-list thousands (lambda (_) "M"))))))))
           
     

(define simplify-all-versions 
  (list simplify-all-a simplify-all-b simplify-all-c
                   simplify-all-d simplify-all-e))

(define benchmark
  (let ([numbers (build-list 2000 add1)])
    (lambda ()
      (for-each
       (lambda (simplify-all)
         (let* ([integer->roman (integer->roman-builder simplify-all)])
           (time (map integer->roman numbers))))
    simplify-all-versions))))
      
;;(benchmark)