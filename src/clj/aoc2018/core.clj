(ns aoc2018.core
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

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

(defn solve-day1-a []
  (let [input (-> "resources/day1.txt"
                  slurp
                  (string/split #"\n"))]
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

(defn cardinalities [st]
  (->> st frequencies vals set))

(defn update-counts [[doubles triples] st]
  (let [card (cardinalities st)]
    [(if (contains? card 2) (inc doubles) doubles)
     (if (contains? card 3) (inc triples) triples)]))

(defn checksum-box-ids [ids]
  (->> ids
       (reduce update-counts [0 0])
       (apply *)))

(def day2-a-test-case
  ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])

(defn solve-day2-a []
  (checksum-box-ids (line-seq (io/reader "resources/day2.txt"))))

;; Answer: 6888

(defn count-differing [a b]
  (reduce (fn [acc [char-a char-b]]
            (if (= char-a char-b) acc (inc acc)))
          0
          (map vector a b)))

(def day2-b-test-case
  ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])

(defn close-match? [[a b]]
  (= 1 (count-differing a b)))

(defn neighboring-ids [ids]
  (->> (combo/combinations ids 2)
       (filter close-match?)
       first))

(defn remove-differing [[a b]]
  (->> (map vector a b)
       (filter #(apply = %))
       (map first)
       (apply str)))

(defn solve-day2-b []
  (->> (line-seq (io/reader "resources/day2.txt"))
       neighboring-ids
       remove-differing))

;; Answer: icxjvbrobtunlelzpdmfkahgs

(defn day3-a-test-case
  ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"])
