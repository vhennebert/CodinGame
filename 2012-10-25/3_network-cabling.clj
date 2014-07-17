(ns Solution
  (:gen-class))

(defn -main [& args]
  (let [n (read)
        coords (doall (repeatedly n #(vector (read) (read))))
        xs (map first coords)
        minx (apply min xs)
        maxx (apply max xs)
        ys (map second coords)
        medy (let [ys (sort ys)]
               (if (odd? n)
                 (nth ys (quot n 2))
                 (quot (apply + (take 2 (drop (- (/ n 2) 1) ys))) 2)))]
    (println
      (apply + (- maxx minx)
             (map #(Math/abs (- % medy)) ys)))))
