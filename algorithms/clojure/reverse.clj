(defn cp-reverse [lst k]
  (loop [current-lst lst current-k k]
    (if (empty? current-lst) (current-k '())
        (recur (rest current-lst)
          (fn [v] (cons (first current-lst)
                        (current-k v)))))))

(cp-reverse '(1 2 3) (fn [v] v))