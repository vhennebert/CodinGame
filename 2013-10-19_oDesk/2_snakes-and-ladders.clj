; Solved in 46min
(ns Solution
  (:gen-class))

(defn -main [& args]
  (let [S (symbol "S")
        E (symbol "E")
        R (symbol "R")
        n (read)
        board (into [] (repeatedly n read))
        end (.indexOf board E)
        jumps (fn [square]
                (let [value (board square)]
                  (if (#{S R} value)
                    (range (+ square 1) (min (+ square 7) (count board)))
                    [(mod (+ square value) (count board))])))
        ]
    (loop [turn 0
           squares #{(.indexOf board S)}
           visited #{}]
      (cond
        (empty? squares) (println "impossible")
        (squares end) (println turn)
        :else (let [visited (into visited squares)]
                (recur
                  (inc turn)
                  (reduce
                    (fn [next-squares square]
                      (into next-squares
                            (for [i (jumps square)
                                  :when (not (visited i))]
                              i)))
                    #{}
                    squares)
                  visited))))))
