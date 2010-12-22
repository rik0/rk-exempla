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

(defparameter *primes-bound* 1)
(defparameter *primes* nil)
(defparameter *primes-table* nil)

(defun compute-primes (max &key (sieve-f #'erat-sieve))
  (let ((sieve (funcall sieve-f max)))
    (setq *primes*
          (loop for i from 0 to (1- max)
             append (when (/= (bit sieve i) 0) (list i)))
          *primes-table* sieve
	  *primes-bound* max))
  (values))

(compute-primes 10000)

(defun enough-primes (max)
  (when (< *primes-bound* max)
    (compute-primes max)))

(defun factorize (n &optional 
		  (factors *primes*) 
		  (factorization (make-hash-table)))
  (if (and factors (> n 1))
      (let ((p (car factors)))
	(multiple-value-bind (q r) (truncate n p)
	  (if (= r 0)
	      (progn
		(setf (gethash p factorization)
		      (1+ (or (gethash p factorization) 0)))
		(factorize q factors factorization))
	      (factorize n (cdr factors) factorization))))
      factorization))

(defun print-factorization (factorization &optional (stream t))
  (maphash (lambda (base exp)
	     (format stream "~d^~d * " base exp))
	   factorization)
  (princ 1))

(print-factorization (factorize (* 7 3 2 5 67 2 3 16 8 31 32 3257)))
