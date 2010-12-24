(defparameter *primes* nil)

;; ;;(let ((memo (make-hash-table)))
;;   (defun factorize (n &optional (primes *primes*))
;;     ;;    (or (gethash memo n)
;; 	(let ((factorization (make-hash-table))
;; 	      (bound (isqrt n)))
;; 	  (loop for p in primes
;; 	     when (< p bound) 
;; 	     if (= (mod n p) 0)
;; 	     do (progn 
;; 		  (setf (gethash factorization p) 1)
;; 		  (setq n (/ n p))
;; 		  (loop 
;; 		     while (= (mod n p) 0)
;; 		     do (progn 
;; 			  (incf (gethash factorization p))
;; 			  (setq n (/ n p))))))
;; 	  factorization));))
	 
(defun factorize (n &optional 
		  (factors *primes*) 
		  (factorization (make-hash-table)))
  (if factors
      (let ((p (car factors)))
	(multiple-value-bind (m d) (truncate n p)
	  (if (= d 0)
	      (progn
		(setf (gethash factorization p) 0)
;;		(setf (gethash factorization p)
;;		      (1+ (or nil; (gethash factorization p) 
;;			      0)))
		(factorize m factors factorization))
	      (factorize m (cdr factors) factorization))))
      factorization))
			     
	
  
	  
	  