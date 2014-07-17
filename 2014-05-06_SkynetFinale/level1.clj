(ns Player
  (:gen-class))

(defn -main [& args]
  (let [node-count (read)
        link-count (read)
        exit-count (read)
        links (reduce
                (fn [links [n1 n2]]
                  (reduce (fn [links [n1 n2]]
                            (update-in links [n1] conj n2))
                          links
                          [[n1 n2] [n2 n1]]))
                {}
                (repeatedly link-count #(list (read) (read))))
        exits (into #{} (repeatedly exit-count read))]
    (loop [links links]
      (let [si (read)
            [n1 n2] (if-let [gateway (some exits (links si))]
                      [si gateway]
                      (first (filter second (for [[node neighbors] links]
                                              [node (some exits neighbors)]))))]
        (println n1 n2)
        (recur (reduce (fn [links [n1 n2]]
                         (let [neighbors (links n1)
                               neighbors (remove #{n2} neighbors)]
                           (if (empty? neighbors)
                             (dissoc links n1)
                             (assoc links n1 neighbors))))
                       links
                       [[n1 n2] [n2 n1]])))))) 
