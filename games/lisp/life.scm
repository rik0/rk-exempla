(define *width* 80)
(define *height* 23)
(define *length* (* *width* *height*))

(define (remove obj lst)
  (cond
    ((null? lst) '())
    ((eq? obj (car lst)) (remove obj (cdr lst)))
    (else (cons (car lst) (remove obj (cdr lst))))))

(define (neighbours pos)
  (map (lambda (x) (fxmodulo x *length*))
    (apply append
      (remove pos
        (map (lambda (pos)
               (list (- pos 1)
                 pos
                 (+ pos 1)))
          (list pos
            (+ pos *width*)
            (- pos *width*)))))))

