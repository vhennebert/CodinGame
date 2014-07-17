(ns Player
  (:gen-class))

(defn find-trail [road road-length moto-survive motos speed x]
  (if (>= x road-length) ["WAIT"]
    (first
      (for [command (concat (when (> 50 speed) ["SPEED"])
                            (when (< 0 speed) ["WAIT"])
                            (when (< 0 speed) ["JUMP"])
                            (when (< 0 (apply min motos)) ["UP"])
                            (when (> 3 (apply max motos)) ["DOWN"])
                            (when (< 1 speed) ["SLOW"]))
            :let [new-speed ((condp = command "SPEED" inc, "SLOW" dec, identity) speed)
                  dy (condp = command "UP" dec, "DOWN" inc, identity)
                  hole? (fn [y x length] (some #{\0} (take length (drop (+ x 1) (road y)))))
                  remaining-motos (remove
                                    (condp get command
                                      #{"SPEED" "WAIT" "SLOW"} #(hole? % x new-speed)
                                      #{"UP" "DOWN"} #(or (hole? % x (- new-speed 1))
                                                          (hole? (dy %) x new-speed))
                                      #(hole? % (+ x new-speed -1) 1))
                                    motos)]
            :when (<= moto-survive (count remaining-motos))
            :let [motos (map dy remaining-motos)
                  commands (find-trail road road-length moto-survive motos new-speed (+ x new-speed))]
            :when commands]
        (cons command commands)))))


(defn -main [& args]
  (let [read-int #(Integer/parseInt (read-line))
        moto-count (read-int)
        moto-survive (read-int)
        road (vec (repeatedly 4 read-line))
        road-length (count (first road))
        speed (read)
        motos (repeatedly moto-count #(do (read) (let [y (read)] (read) y)))
        debug (binding [*out* *err*] (println "moto-count" moto-count "moto-survive" moto-survive "speed" speed "motos" motos)
                (doseq [r road] (println r)))
        commands (find-trail road road-length moto-survive motos speed 0)]
    (doseq [command commands]
      (println command))))
