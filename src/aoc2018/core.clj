(ns aoc2018.core)

(defn parse-frequency [fs]
  (let [op-str  (subs fs 0 1)
        num-str (subs fs 1)
        op      (case op-str
                  "+" +
                  "-" -
                  (throw (Exception. (str "unknown operation '" op-str "'") )))
        num     (Integer/parseInt num-str)]
    [op num]))

(defn calibrate-frequencies [start freqs]
  (reduce (fn [acc [f & fs]]
            (let [[op num] (parse-frequence f)]
              (op acc num)))
          start
          freqs))
