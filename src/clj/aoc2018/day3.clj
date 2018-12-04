(ns aoc2018.day3
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def day3-a-test-case
  ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"])

(defn parse-claim [line]
  (let [claim-re #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"
        [_ id x y w h] (re-matches claim-re line)]
    {:id     (Integer/parseInt id)
     :x      (Integer/parseInt x)
     :y      (Integer/parseInt y)
     :width  (Integer/parseInt w)
     :height (Integer/parseInt h)}))

(defn make-grid [[width height]]
  (vec (repeat height (vec (repeat width #{})))))

(defn render-grid [grid]
  (->> grid
       (map (fn [x] (map #(count %) x)))
       (map string/join)
       (string/join "\n")))

(defn print-grid [grid]
  (-> grid render-grid println))

(defn mark-square [grid id [x y :as coord]]
  ;;(print "id:" id "x,y:" coord)
  ;;(print-grid grid)
  (update-in grid [y x] conj id))

(defn stake-claim [grid {:keys [id x y width height]}]
  (reduce (fn [last-grid coord]
            (mark-square last-grid id coord))
          grid
          (for [xx (range x (+ x width)) yy (range y (+ y height))]
            [xx yy])))

(defn calculate-dimensions [claims]
  [(->> claims (map (fn [{:keys [x width]}] (+ x width))) (apply max) inc)
   (->> claims (map (fn [{:keys [y height]}] (+ y height))) (apply max) inc)])

(defn overlay-claims [raw-claims]
  (let [claims     (map parse-claim raw-claims)
        dimensions (calculate-dimensions claims)
        empty-grid (make-grid dimensions)]
    (-> (reduce stake-claim empty-grid claims))))

(defn count-conflicts [grid]
  (->> grid flatten (filter #(> (count %) 1)) count))

(defn resolve-claims [raw-claims]
  (->> raw-claims overlay-claims count-conflicts))

(defn solve-day3-a []
    (->> (line-seq (io/reader "resources/day3.txt"))
         resolve-claims))

;; Answer: 103806

(defn find-unique-claim [raw-claims]
  (->> raw-claims
       overlay-claims
       (apply concat)
       (reduce (fn [[sing mult] square]
                 (if (= 1 (count square) )
                   [(set/union sing square) mult]
                   [sing (set/union mult square)]))
               [#{} #{}])
       (apply set/difference)))

(defn solve-day3-b []
  (->> (line-seq (io/reader "resources/day3.txt"))
       find-unique-claim))

;; Answer: 625
