#lang scheme

(define (mcons h t)
  (printf "[~a|~a]~n" h t)
  (cons h t))

(define cp-reverse
  (lambda (lst)
    (letrec ([cpr (lambda (lst k)
                 (cond 
                   ((null? lst) (k '()))
                   (else 
                    (cpr (cdr lst)
                         (lambda (v)
                           (cons (car lst) (k v)))))))])
      (cpr lst (lambda (v) v)))))

(define (sl-reverse lst)
  (cond
    ((null? lst) '())
    (else (append (sl-reverse (cdr lst)) (list (car lst))))))

(define (print-times min max step)
  (for ([m (in-range min max step)])
    (let ([lst (for/list ([i (in-range m)]) i)])
      (time (reverse lst))
      (time (cp-reverse lst))
      (time (sl-reverse lst)))))