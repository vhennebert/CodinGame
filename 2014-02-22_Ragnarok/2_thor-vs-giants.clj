; TODO Tests 9 and 10 not working
(ns Player
  (:gen-class))

(defn- debug [& params]
  (binding [*out* *err*]
    (apply println params)))

(defn -main [& args]
  (loop [tx (read)
         ty (read)]
    (let [strike-count (read)
          giant-count (read)
          giants (repeatedly giant-count #(vector (read) (read)))
          distance (fn [x0 y0 [x1 y1]] (max (Math/abs (- x1 x0)) (Math/abs (- y1 y0))))]
      (debug "tx" tx "ty" ty "giants" giants)
      (cond
        (= 0 giant-count) (println "WAIT")
        (every? (fn [[x y]] (every? #(<= -4 % 4) [(- tx x) (- ty y)])) giants) (println "STRIKE")
        :else
        (let [move-distances-by-variance
              (->>
                (for [dx [-1 0 1]
                      dy [-1 0 1]
                      :let [new-tx (+ tx dx)
                            new-ty (+ ty dy)]
                      :when (and (<= 0 new-tx 39)
                                 (<= 0 new-ty 17)
                                 (not-any? #{[new-tx new-ty]} giants))
                      :let [new-giants (for [[x y] giants]
                                         (let [sign #(cond
                                                       (> 0 %) -1
                                                       (= 0 %) 0
                                                       :else 1)
                                               move (fn [z tz]
                                                      (+ z (sign (- tz z))))]
                                           [(move x new-tx) (move y new-ty)]))]
                      :when (not-any? #{[new-tx new-ty]} new-giants)]
                  (let [distances (map (partial distance new-tx new-ty) new-giants)]
                    [dx dy (apply min distances) (apply max distances)]))
                (group-by (fn [[_ _ min-dist max-dist]] (- max-dist min-dist))))]
          (debug move-distances-by-variance)
          (if (empty? move-distances-by-variance)
            (do (println "STRIKE") (recur tx ty))
            (let [[_ move-distances-coll] (apply min-key key move-distances-by-variance)
                  get-min-dist (fn [[_ _ min-dist _]] min-dist)
                  min-dist (apply min (map get-min-dist move-distances-coll))
                  move-distances-coll (filter #(= min-dist (get-min-dist %)) move-distances-coll)
                  [dx dy] (apply min-key (fn [[dx dy _ _]] (distance 19.5 8.5 [(+ tx dx) (+ ty dy)])) move-distances-coll)
                  move (str ({-1 \N 1 \S} dy) ({-1 \W 1 \E} dx))]
              (debug "dx" dx "dy" dy)
              (if (seq move)
                (println move)
                (println "WAIT"))
              (recur (+ tx dx) (+ ty dy)))))))))
