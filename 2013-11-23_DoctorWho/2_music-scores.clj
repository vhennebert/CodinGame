; Solved in 4:16
(ns Solution
  (:gen-class))

(defn -main [& args]
  (let [width (read)
        height (read)
        image (boolean-array (* width height))
        image (loop [i 0]
                (if-let [color (read *in* false nil)]
                  (let [black (= (symbol "B") color)
                        number (read)]
                    (doseq [x (range i (+ i number))]
                      (aset-boolean image x black))
                    (recur (+ i number)))
                  image))
        image (mapv vec (partition width image))
        lines (->> image
                   (map-indexed #(when (< (* width 1/2) (count (filter #{true} %2))) %1))
                   (partition-by nil?)
                   (remove (comp nil? first)))
        line-thickness (count (first lines))
        line-gap (- (first (second lines)) (ffirst lines) line-thickness)
        half-line-gap (quot line-gap 2)
        rows-following-lines (concat [(- (ffirst lines) line-gap)]
                                     (map (comp inc last) lines)
                                     [(+ (last (last lines)) line-gap line-thickness 1)])
        x-notes-between-lines (zipmap rows-following-lines [\G \E \C \A \F \D \B])
        x-notes-on-lines (zipmap rows-following-lines [\A \F \D \B \G \E \C])

        get-black-lines
        #(->>
           (for [x rows-following-lines
                 y (range width)]
             [x y (get-in image [x y])])
           (partition-by last)
           (filter (comp last first)))

        notes-between-lines
        (->>
          (get-black-lines)
          (filter #(< (* line-thickness 2) (count %) line-gap))
          (map #(let [[x y _] (first %)]
                  [y (str (x-notes-between-lines x)
                          (if (get-in image [(+ x half-line-gap) (+ y line-thickness)]) \Q \H))])))

        quarter-notes-on-lines
        (->>
          (get-black-lines)
          (filter #(<= (- line-gap line-thickness) (count %)))
          (map #(let [[x y _] (first %)] [y (str (x-notes-on-lines x) \Q)])))

        half-notes-on-lines
        (let [min-y (.indexOf (image (ffirst lines)) true)
              max-y (.lastIndexOf (image (ffirst lines)) true)]
          (->>
            (for [x (map first lines)
                  y (range min-y max-y)]
              [x y (get-in image [x y])])
            (partition-by last)
            (filter (comp not last first))
            (map #(let [[x y _] (first %)] [y (str (x-notes-on-lines (+ x line-thickness)) \H)]))))

        half-c-graves
        (let [x (dec (last rows-following-lines))]
          (->>
            (for [y (range width)]
              [y (get-in image [x y])])
            (partition-by last)
            (filter (comp last first))
            (filter #(> line-gap (count %)))
            (take-nth 2)
            (map #(let [[y _] (first %)] [y "CH"]))))
        ]
    (->>
      (concat notes-between-lines quarter-notes-on-lines half-notes-on-lines half-c-graves)
      (sort-by first)
      (map second)
      (interpose \space)
      (apply str)
      println)))
