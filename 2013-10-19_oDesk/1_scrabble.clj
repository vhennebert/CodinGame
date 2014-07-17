; Solved in 28min
(ns Solution
  (:gen-class))

(defn -main [& args]
  (let [scores
        {\e 1
         \a 1
         \i 1
         \o 1
         \n 1
         \r 1
         \t 1
         \l 1
         \s 1
         \u 1
         \d 2
         \g 2
         \b 3
         \c 3
         \m 3
         \p 3
         \f 4
         \h 4
         \v 4
         \w 4
         \y 4
         \k 5
         \j 8
         \x 8
         \q 10
         \z 10}
        dict-size (Integer/parseInt (read-line))
        dict (doall (repeatedly dict-size read-line))
        letters (frequencies (read-line))]
    (println
      (second
        (reduce
          (fn [[best-score best-word] word]
            (let [score (first
                          (reduce
                            (fn [[score letters] letter]
                              (if (and score (letters letter))
                                [(+ score (scores letter))
                                 (let [number (letters letter)]
                                   (if (< 1 number)
                                     (assoc letters letter (dec number))
                                     (dissoc letters letter)))]
                                [nil letters]))
                            [0 letters]
                            word))]
              (if (and score (< best-score score))
                [score word]
                [best-score best-word])))
          [0 nil]
          dict)))))
