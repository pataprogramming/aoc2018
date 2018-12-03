(ns aoc2018.day2
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

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
