(ns factorials (:gen-class))

(set! *warn-on-reflection* true)

(defn bad-factorial [n]
    (if (= n 1)
        1
        (* n (bad-factorial (- n 1)))))


(defn factorial [#^Integer n]
    (let [factorial-aux
      (fn [#^BigInteger acc n]
          (if (== n 1)
              acc
              (recur (* acc n) (- n 1))))]
      (factorial-aux 1 (int n))))

(def factorial-functions '(factorial bad-factorial))

(defmacro timed [expr]
  `(let [start# (System/nanoTime)
         result# ~expr]
    [result# (- (System/nanoTime) start#)]))

(defn time-variants [n]
  (let [[fast-result fast-time] (timed (factorial n))
        [slow-result slow-time] (timed (bad-factorial n))]
    {:slow-time slow-time :fast-time fast-time :match (= slow-result fast-result)
     :slow-result slow-result :fast-result fast-result}))

(defn -main
  [numberAsString]
    (let [result (time-variants (Integer/parseInt numberAsString))]
      (if (:match result)
        (do
          (println "Slow time:" (:slow-time result))
          (println "Fast time:" (:fast-time result)))
        (do
          (println "Results do not match!")
          (println result)))))

