#lang scheme

(define-syntax for-permutation
  (syntax-rules ()
    [(_ () s1 s2 ...)
     (begin s1 s2 ...)]
    [(_ ([v1 l1] [v2 l2] ...) s1 s2 ...)
     (for-each
       (lambda (v1)
         (for-permutation ([v2 l2] ...)
           s1 s2 ...)) l1)]))

(define-syntax for
  (syntax-rules ()
    [(_ () s1 s2 ...)
     (begin s1 s2 ...)]
    [(_ ([v1 l1] [v2 l2] ...) s1 s2 ...)
     (for-each
       (lambda (v1 v2 ...)
         (begin s1 s2 ...))
       l1 l2 ...)]))
