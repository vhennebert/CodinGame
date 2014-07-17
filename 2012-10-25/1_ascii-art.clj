(ns Solution
  (:gen-class))

(defn -main [& args]
  (let [read-int #(Integer/parseInt (read-line))
        width (read-int)
        height (read-int)
        text (read-line)
        rows (repeatedly height read-line)
        indices (map (fn [letter]
                       (let [index (.indexOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                             (Character/toUpperCase (int letter)))]
                         (if (< index 0) 26 index)))
                     text)]
    (doseq [row rows]
      (doseq [index indices]
        (print (subs row (* width index) (* width (+ index 1)))))
      (println))))
