(ns sieves.erathostenes)
(import ['java.util 'BitSet])



(defn eratosthenes [max-size]
  (let [upper-bound (int (Math/sqrt max-size))
        sieve (new BitSet upper-bound)
        set-multiples-for!
          (fn [n]
            (loop [m (+ n n)]
              (when-not (> m max-size)
                (.set sieve m)
                (recur (+ m n)))))]
    (do
      (set-multiples-for! 2)
      (loop [k 3]
        (if (< k upper-bound)
          (set-multiples-for! k)
          (recur (+ k 2))))
      (fn [p] (if (< p max-size)
                (not (.get sieve p))
                (throw (IndexOutOfBoundsException. (format "%d is too big." p))))))))

(def is-prime? (eratosthenes 100000000))