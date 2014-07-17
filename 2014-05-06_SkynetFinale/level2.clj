(ns Player
  (:gen-class))

(defn -main [& args]
  (let [node-count (read)
        link-count (read)
        exit-count (read)
        update-links (fn [update-link links [node1 node2]]
                       (reduce (fn [links [n1 n2]]
                                 (update-link links n1 n2))
                               links
                               [[node1 node2] [node2 node1]]))
        links (reduce
                (partial update-links
                         (fn [links n1 n2] (update-in links [n1] conj n2)))
                {}
                (repeatedly link-count #(list (read) (read))))
        exits (into #{} (repeatedly exit-count read))]
    (loop [links links]
      (let [si (read)
            some-exit #(some exits (links %))
            [n1 n2]
            (if-let [gateway (some-exit si)]
              [si gateway]
              (let [get-exit-count #(count (filter exits %))
                    max-exit-count
                    (apply max (map (fn [[_ neighbors]] (get-exit-count neighbors))
                                    links))
                    nodes-to-severe
                    (->> links
                         (filter (fn [[_ neighbors]]
                                   (= max-exit-count (get-exit-count neighbors))))
                         (map first)
                         set)
                    paths
                    (loop [to-visit #{si} visited {}]
                      (if (every? visited nodes-to-severe)
                        (select-keys visited nodes-to-severe)
                        (let [newly-visited
                              (reduce
                                (fn [newly-visited node]
                                  (merge-with
                                    (fn [path1 path2]
                                      (if (some-exit (first path1)) path1 path2))
                                    newly-visited 
                                    (into {}
                                          (for [neighbor (links node)
                                                :when (not
                                                        (or
                                                          (exits neighbor)
                                                          (visited neighbor)))]
                                            [neighbor (cons node (visited node))]))))
                                {}
                                to-visit)]
                          (recur (keys newly-visited) (into visited newly-visited)))))
                    closest-node
                    (->> paths
                         (map (fn [[node path]]
                                [node (remove some-exit path)]))
                         (apply min-key (comp count second))
                         first)]
                [closest-node (some-exit closest-node)]))]
        (println n1 n2)
        (recur (update-links (fn [links n1 n2]
                               (let [neighbors (remove #{n2} (links n1))]
                                 (if (empty? neighbors)
                                   (dissoc links n1)
                                   (assoc links n1 neighbors))))
                             links
                             [n1 n2]))))))
