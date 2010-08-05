#lang scheme

(define cp-partition
  (lambda (lst p? k)
    (letrec ([cpp 
              (lambda (lst k)
                (cond
                  ((null? lst) (k '() '()))
                  ((p? (car lst)) 
                   (cpp (cdr lst)
                        (lambda (p-true p-false)
                          (k (cons (car lst) p-true)
                             p-false))))
                  (else
                   (cpp (cdr lst)
                        (lambda (p-true p-false)
                          (k p-true
                             (cons (car lst) p-false)))))))])
      (cpp lst k))))

(define cp-append 
  (lambda (lst1 lst2 k)
    (cond ((null? lst1) (k lst2))
          (else (cp-append (cdr lst1) lst2 
                           (lambda (rest)
                             (k (cons (car lst1) rest))))))))
                                     
(define quicksort
  (lambda  (lst less?) 
    (letrec ([qs 
              (lambda (lst k)
                (cond ((null? lst) (k '()))
                      (else
                       (let ([pivot (car lst)]
                             [rest (cdr lst)])
                         (cp-partition rest 
                                       (lambda (x) (less? x pivot))
                                       (lambda (less-than greater-than)
                                      (qs greater-than
                                          (lambda (sorted-gt)
                                            (qs less-than
                                                (lambda (sorted-lt)
                                                  (cp-append
                                                   sorted-lt
                                                   (cons pivot sorted-gt) k)))))))))))])
      (qs lst (lambda (v) v)))))

(define (random-list max length)
  (letrec ([rl (lambda (length)
                 (cond ((= length 0) '())
                       (else (cons (random max)
                                   (rl (- length 1))))))])
    (rl length)))

(define (test-quicksort)
  (let ([tests-per-length 50]
        [random-top-integer 1000]
        [list-lengths '(0 1 10 11 25 40 64 128)])
    (for-each 
     (lambda (length)
       (do ([i tests-per-length (- i 1)])
         ((zero? i))
         (let* ([the-list (random-list random-top-integer length)]
                [lt-sorted-list (sort the-list <)]
                [lt-qsorted-list (quicksort the-list <)]
                [gt-sorted-list (sort the-list >)]
                [gt-qsorted-list (quicksort the-list >)])
           (when (not (equal? lt-sorted-list lt-qsorted-list))
             (printf "~a[<]:~n~a~n~a~n~n" the-list
                     lt-sorted-list lt-qsorted-list))
           (when (not (equal? gt-sorted-list gt-qsorted-list))
             (printf "~a[>]:~n~a~n~a~n~n" the-list
                     gt-sorted-list gt-qsorted-list)))))
     list-lengths)))

                       
           