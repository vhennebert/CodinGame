(ns Solution
  (:gen-class))

(defn -main [& args]
  (let [n (read)
        values (doall (repeatedly n read))
        freqs (into (sorted-map) (frequencies values))]
    (println
      (first
        (reduce
          (fn [[loss freqs] v]
            (let [f (freqs v)
                  freqs (if (= 1 f)
                          (dissoc freqs v)
                          (assoc freqs v (dec f)))]
              [(min loss (- (ffirst freqs) v)) freqs]))
          [0 freqs]
          (drop-last values))))))
