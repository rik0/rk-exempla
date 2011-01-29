(defn fibo
  ([n] (fibo n 0 1))
  ([n a b]
     (if (= n 0) a
	 (recur (- n 1) b (+ a b)))))

