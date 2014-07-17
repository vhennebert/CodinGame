(ns Player
  (:gen-class))

(defn -main [& args]
  (let [[lx ly tx ty] (repeatedly 4 read)
        xs (cond
             (< tx lx) (repeat (- lx tx) \E)
             (= tx lx) ()
             :else (repeat (- tx lx) \W))
        ys (cond
             (< ty ly) (repeat (- ly ty) \S)
             (= ty ly) ()
             :else (repeat (- ty ly) \N))
        move-count (max (Math/abs (- lx tx)) (Math/abs (- ly ty)))
        pad #(concat % (repeat (- move-count (count %)) nil))
        moves (map str (pad ys) (pad xs))]
    (doseq [m moves]
      (println m))))
