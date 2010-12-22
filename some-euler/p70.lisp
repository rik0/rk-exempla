(defun erat-sieve (max)
  (let ((sieve (make-array (list max)
                           :initial-element 1
                           :element-type 'bit))
        (bound (isqrt max)))
    (setf (bit sieve 0) 0)
    (setf (bit sieve 1) 0)
    (loop for i from 2 to bound
       when (= (bit sieve i) 1)
       do (loop 
             for j from (+ i i) to (1- max) by i
             do (setf (bit sieve j) 0)))
    sieve))

(defun slow-primep (n)
  (not
   (loop for i from 2 to (1- n)
      thereis (= (mod n i) 0))))

(defparameter *primes* nil)
(defparameter *primes-table* nil)

(defun compute-primes (max)
  (let ((sieve (erat-sieve max)))
    (setq *primes*
          (loop for i from 0 to (1- max)
             append (when (/= (bit sieve i) 0) (list i)))
          *primes-table*
          sieve))
  (values))



(compute-primes 100000)
;;(print *primes*)
;;(print *primes-table*)    

(defun test-stuff ()
  (loop 
     for i from 0 below (length *primes-table*)
     always (eq (bit *primes-table* i)
                (slow-primep i))))

(test-stuff)