(ns aoc2018.day1
  (:require [clojure.string :as string]))

(defn parse-drift [f]
  (let [op-str  (subs f 0 1)
        num-str (subs f 1)
        op      (case op-str
                  "+" +
                  "-" -
                  (throw (Exception. (str "unknown operation '" op-str "'") )))
        num     (Integer/parseInt num-str)]
    [op num]))

(defn calibrate-frequency [start offsets]
  (reduce (fn [acc offset]
            (let [[op num] (parse-drift offset)]
              (op acc num)))
          start
          offsets))

(def day1-input (-> "resources/day1.txt"
                  slurp
                  (string/split #"\n")))

(defn solve-day1-a []
  (let [input day1-input]
    (calibrate-frequency 0 input)))

;; Answer: 459


(defn stabilize-frequency [start offsets]
  (reduce (fn [[acc seen?] offset]
            (let [[op num] (parse-drift offset)
                  freq     (op acc num)]
              (if (seen? freq)
                (reduced freq)
                [freq (conj seen? freq)])))
          [start (hash-set)]
          (cycle offsets)))

(defn solve-day1-b []
  (let [input (-> "resources/day1.txt"
                  slurp
                  (string/split #"\n"))]
    (stabilize-frequency 0 input)))

;; Answer: 65474
