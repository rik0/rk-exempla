#lang scheme

(define-syntax while
  (syntax-rules ()
    [(_ c b1 b2 ...)
     (let loop ()
       (when c b1 b2 ... (loop)))]))

(define-syntax until
  (syntax-rules ()
    [(_ c b1 b2 ...) (while (not c) b1 b2 ...)]))


(let ([i 10]) 
  (until (zero? i) 
         (displayln i) 
         (set! i (- i 1))))