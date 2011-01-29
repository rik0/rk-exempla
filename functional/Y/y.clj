(defn Y [F]
  (let [G (fn [K]
	    (F (fn [& A]
		 (apply (K K) A))))]
    (G G)))


(def fact
     (Y (fn [f]
	  (fn [n]
	    (if (<= n 0)
	      1
	      (* n (f (- n 1))))))))


(def x
     (Y (fn [r]
	  (

;; (def fact
;;      (Y (fn [r]
;; 	  (fn [n acc]
;; 	    (if (<= n 0)
;; 	      1
;; 	      (r (- n 1) (* n acc)))))))

