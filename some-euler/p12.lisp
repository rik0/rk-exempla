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
	 
			     
	
  
	  
	  