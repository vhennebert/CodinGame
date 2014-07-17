(ns Player
  (:gen-class))

(def ^:dynamic *debug* false)

(defn- debug [& params]
  (when *debug*
    (binding [*out* *err*]
      (apply println params))))

(defn find-trail 
  ([road gap landing s x]
   (debug "road" road "gap" gap "landing" landing)
   (debug "gap x" (str \[ road \space (+ road gap -1) \]) "max x" (+ road gap landing -1))
   (find-trail road gap landing s x []))
  ([road gap landing s x commands]
   (let [total-length (+ road gap landing)]
     (debug commands "x" x "s" s)
     (cond
       (>= x total-length) nil
       (>= x (+ road gap)) (when (and (= "JUMP" (last commands)) (< (+ x (* s (dec s) 1/2)) total-length))
                             (into commands (repeat s "SLOW")))
       (>= x road) nil
       (>= (+ x s) (+ road gap)) (find-trail road gap landing s (+ x s) (conj commands "JUMP"))
       :else (first
               (for [command (cons "SPEED"
                                   (condp < s
                                     1 ["WAIT" "SLOW"]
                                     0 ["WAIT"]
                                     []))
                     :let [new-speed (({"SPEED" inc, "WAIT" identity, "SLOW" dec} command) s)
                           commands (find-trail road gap landing new-speed (+ x new-speed) (conj commands command))]
                     :when commands]
                 commands))))))

(defn -main [& args]
  (let [road (read)
        gap (read)
        landing (read)
        speed (read)
        x0 (read)
        commands (find-trail road gap landing speed x0)]
    (doseq [command commands]
      (println command)
      (read)
      (read))))
