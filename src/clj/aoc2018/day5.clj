(ns aoc2018.day5
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.math.numeric-tower :as math])
  (:import  (java.lang Character)))

(def day5-test-case "dabAcCaCBAcCcaDA")

(defn reactive? [a b]
  (and (not (nil? a)) (not (nil? b))
       (= 32 (math/abs (- (int a) (int b))))))

(defn trigger-polymer [polymer]
  (->> polymer
       (reduce (fn [reacted right]
                 (let [left (last reacted)]
                   (if (reactive? left right)
                     (pop reacted)
                     (conj reacted right))))
               [])
       string/join))

(defn solve-day5-a []
  (-> (slurp "resources/day5.txt")
      string/trim
      trigger-polymer
      count))

;; Answer: 9704

(defn candidate-improvements [polymer]
  (let [units (->> polymer
                   string/lower-case
                   set
                   (map (fn [ch] #{ch (Character/toUpperCase ch)})))]
    (map (fn [unit] [unit (remove unit polymer)]) units)))

(defn improve-polymer [polymer]
  (apply min
   (map (fn [[unit candidate]]
          (print "Removing units" (apply str unit))
          (let [result (trigger-polymer candidate)
                length (count result)]
            (println " - length" length)
            length))
        (candidate-improvements polymer))))

(defn solve-day5-b []
  (-> (slurp "resources/day5.txt")
      string/trim
      improve-polymer))

;; Answer: 6942
