;; (define-syntax while
;;   (syntax-rules ()
;;     [(_ c b1 b2 ...)
;;      (let loop ()
;;        (when c b1 b2 ... (loop)))]))

;; (define-syntax until
;;   (syntax-rules ()
;;     [(_ c b1 b2 ...) (while (not c) b1 b2 ...)]))

(define-syntax when
  (syntax-rules ()
    [(_ c b1 ...)
     (cond (c b1 ...) (else #f))]))

(define-syntax unless
  (syntax-rules ()
    [(_ c b1 ...)
     (cond (c #f) (else b1 ...))]))

;; (define-syntax for
;;   (syntax-rules ()
;;     [(_ ([var1 list1] [var2 list2] ...) body)
;;      (unless (null? list1)
;;        (let loop ([var1 (car list1)] [ list1 (cdr list1)])
;;          (for ([var2 list2] ...) body)
;;          (unless (null? list1)
;;            (loop (car list1) (cdr list1)))))]
;;     [(_ () body) body]))

(define (sort sequence less?)
;;  (declare (standard-bindings) (not safe))
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

(define (add1 n) (+ n 1))

(define (build-list how-many constructor)
  (let loop ([n 0] [lst '()])
    (cond
      ((= n how-many) lst)
      (else
        (loop (add1 n) (cons (constructor n) lst))))))


(define foldl
  (lambda (f z lst)
    (let loop ([acc z] [lst lst])
      (cond
        ((null? lst) z)
        (else
          (loop (f acc (car lst)) (cdr lst)))))))

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
             ((< n weight) null)
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
                  
;; Another imperative version
;; Uses non standard for
;; (define simplify-all-f
;;   (lambda (weights n digits)
;;     (for ([weight-pair weights])
;;       (let  ([weight (car weight-pair)]
;;              [ch (cadr weight-pair)])
;;         (let simplify-again ()
;;           (unless (< n weight)
;;             (set! n (- n weight))
;;             (set! digits (cons ch digits))
;;             (simplify-again)))))
;;     digits))

;; Another imperative version
;; Uses user defined macros and for
;; (define simplify-all-g
;;   (lambda (weights n digits)
;;     (for ([weight-pair weights])
;;       (let  ([weight (car weight-pair)]
;;              [ch (cadr weight-pair)])
;;         (until  (< n weight)
;;             (set! n (- n weight))
;;             (set! digits (cons ch digits)))))
;;     digits))

;(define integer->roman-nolet
;  ((lambda (weights)
;     (lambda (n)
;       (let-values ([(thousands n) (quotient/remainder n 1000)])
;         (foldl string-append ""
;           (simplify-all weights n(build-list thousands (lambda (_) "M")))))))
;   (sort '((1 "I") (4 "IV") (5 "V") (9 "IX") (10 "X")
;                   (40 "XL") (50 "L") (90 "XC") (100 "C")
;                   (400 "CD") (500 "D") (900 "CM"))
;         >
;         #:key car)))
           

(define (integer->roman-builder simplify-all)
  (let ([weights (sort '((1 "I") (4 "IV") (5 "V") (9 "IX") (10 "X")
                                 (40 "XL") (50 "L") (90 "XC") (100 "C")
                                 (400 "CD") (500 "D") (900 "CM"))
                       (lambda (left right) (< (car left) (car right))))])
    (lambda (n)
      (let ([thousands (fxquotient n 1000)]
            [n (fxremainder n 1000)])
        (foldl string-append ""
               (simplify-all weights n
                             (build-list thousands (lambda (_) "M"))))))))
           
     

(define simplify-all-versions 
  (list simplify-all-a simplify-all-b simplify-all-c
                   simplify-all-d simplify-all-e))

(define (benchmark how-many)
  (for ([simplify-all simplify-all-versions])
    (let* ([integer->roman (integer->roman-builder simplify-all)]
          [times (build-list how-many values)]
          [numbers (build-list 2000 add1)])
      (time (map (lambda (n)
                   (for-each
                     (lambda (i) (integer->roman n))
                     times))
              numbers)))))

      
(benchmark 200)