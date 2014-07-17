; Solved in 1:00
(ns Solution
  (:gen-class))

(defn -main [& args]
  (let [n (read)
        price (read)
        budgets (doall (repeatedly n read))
        max-bugdet (apply + budgets)]
    (if (> price max-bugdet)
      (println "IMPOSSIBLE")
      (loop [budget max-bugdet budgets (into (sorted-map-by >) (frequencies budgets))]
        (if (= price budget)
          (doseq [[contribution number] (rseq budgets)]
            (dotimes [_ number] (println contribution)))
          (let [[max-contribution max-contributor-count] (first budgets)
                max-reduction (if (<= 2 (count budgets))
                                (- max-contribution (first (fnext budgets)))
                                max-contribution)
                to-reduce (- budget price)
                to-reduce-per-head (min max-reduction (quot to-reduce max-contributor-count))
                [to-reduce-per-head to-reduce-count] (if (zero? to-reduce-per-head)
                                                       [1 to-reduce]
                                                       [to-reduce-per-head max-contributor-count])]
            (recur (- budget (* to-reduce-per-head to-reduce-count))
                   (update-in (if (= max-contributor-count to-reduce-count)
                                (dissoc budgets max-contribution)
                                (assoc budgets max-contribution (- max-contributor-count to-reduce-count)))
                              [(- max-contribution to-reduce-per-head)]
                              #(+ (if % % 0) to-reduce-count)))))))))
