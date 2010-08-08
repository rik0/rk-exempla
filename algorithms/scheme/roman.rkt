#lang racket

(define-syntax while
  (syntax-rules (loop)
    [(_ c b1 b2 ...)
     (let loop ()
       (when c b1 b2 ... (loop)))]))

(define-syntax until
  (syntax-rules ()
    [(_ c b1 b2 ...) (while (not c) b1 b2 ...)]))


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
         (simplify-all (cdr weights) n digits))))))

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
         (simplify-all (cdr weights) n digits))))))

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
         (simplify-all (cdr weights) n digits))))))

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
(define simplify-all-f
  (lambda (weights n digits)
    (for ([weight-pair weights])
      (let  ([weight (car weight-pair)]
             [ch (cadr weight-pair)])
        (let simplify-again ()
          (unless (< n weight)
            (set! n (- n weight))
            (set! digits (cons ch digits))
            (simplify-again)))))
    digits))

;; Another imperative version
;; Uses user defined macros and for
(define simplify-all-g
  (lambda (weights n digits)
    (for ([weight-pair weights])
      (let  ([weight (car weight-pair)]
             [ch (cadr weight-pair)])
        (until  (< n weight)
            (set! n (- n weight))
            (set! digits (cons ch digits)))))
    digits))


(define integer->roman-nolet
  ((lambda (weights)
     (lambda (n)
       (let-values ([(thousands n) (quotient/remainder n 1000)])
         (foldl string-append ""
           (simplify-all weights n(build-list thousands (lambda (_) "M")))))))
   (sort '((1 "I") (4 "IV") (5 "V") (9 "IX") (10 "X")
                   (40 "XL") (50 "L") (90 "XC") (100 "C")
                   (400 "CD") (500 "D") (900 "CM"))
         >
         #:key car)))
           


(define integer->roman
  (let ([weights (sort '((1 "I") (4 "IV") (5 "V") (9 "IX") (10 "X")
                                 (40 "XL") (50 "L") (90 "XC") (100 "C")
                                 (400 "CD") (500 "D") (900 "CM"))
                       >
                       #:key car)])
    (lambda (n)
      (let-values ([(thousands n) (quotient/remainder n 1000)])
        (foldl string-append ""
               (simplify-all weights n
                             (build-list thousands (lambda (_) "M")))))))
  )
           
     
