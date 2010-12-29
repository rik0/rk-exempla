(define range
  (case-lambda
    [(end) (range 0 end 1)]
    [(start end) (range start end 1)]
    [(start end step)
     (let r [(i start)]
       (if (>= i end)
           '()
           (cons i (r (+ i step)))))]))


(define xrange
  (case-lambda
    [(end) (xrange 0 end 1)]
    [(start end) (xrange start end 1)]
    [(start end step)
     ( )
     ]))

(define xrange-generator
  (let [(current-value #f)]
    (lambda (start stop end)
      (call/cc
        (lambda (k)
          (set! current-value (+ current-value 1))
          (k current-value)
          ))))))

