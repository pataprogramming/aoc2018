(ns aoc2018.day4
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def day4-test-case
"[1518-11-05 00:55] wakes up]
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep")

(defn organize-logs [raw-logs]
  (-> raw-logs
      (string/split #"\n")
      sort))

(defn parse-event [log-entry]
  (let [event-re #"(\d+)\] (?:(wakes up)|(falls asleep)|(?:Guard #(\d+) begins shift))"
        [_ minute wake sleep id :as all] (re-find event-re log-entry)]
    (cond
      wake {:wake (Integer/parseInt minute)}
      sleep {:sleep (Integer/parseInt minute)}
      :else {:begin (Integer/parseInt id)})))

(defn coalesce-events [events]
  (->> events
       (reduce (fn [[acc id slept] {:keys [begin wake sleep]}]
                 (cond
                   begin [acc begin nil]
                   sleep [acc id sleep]
                   :else [(conj acc {:id id
                                     :sleep slept
                                     :wake wake
                                     :duration (- wake slept)})
                          id nil]))
               [[] nil nil])
       first))

(defn analyze-guards [intervals]
  (->> intervals
       (reduce (fn [acc {:keys [id sleep wake duration]}]
                 (-> acc
                     (assoc-in [id :id] id)
                     (update-in [id :total] #(if % (+ % duration) duration))
                     (update-in [id :minutes] concat (range sleep wake))))
               {})))

(defn analyze-minutes [guards]
  (->> guards
       (reduce (fn [acc [id {:keys [minutes] :as guard}]]
                 (assoc-in acc [id :frequencies] (frequencies minutes)))
               guards)))

(defn pick-minute-per-guard [guards]
  (->> guards
       (reduce (fn [acc [id {:keys [frequencies]}]]
                 (assoc-in acc [id :sleepiest-minute]
                           (key (apply max-key val frequencies))))
               guards)))

(defn pick-minute-by-sleepiest-guard [guards]
  (-> guards
      vals
      (->> (apply max-key :total))
      (select-keys [:id :sleepiest-minute])))

(defn process-logs [raw-logs]
  (->> raw-logs
       organize-logs
       (map parse-event)
       coalesce-events
       analyze-guards
       analyze-minutes
       pick-minute-per-guard))

(defn first-strategy [raw-logs]
  (->> raw-logs
       process-logs
       pick-minute-by-sleepiest-guard))

(defn encode-answer [{:keys [id sleepiest-minute]}]
  (* id sleepiest-minute))

(defn solve-day4-a []
  (->> (slurp "resources/day4.txt")
       first-strategy
       encode-answer))

;; Ansewr: 102688

(defn pick-guard-by-sleepiest-minute [guards]
  (-> guards
      vals
      (->> (apply max-key (fn [{:keys [frequencies sleepiest-minute]}]
                            (get frequencies sleepiest-minute))))
      (select-keys [:id :sleepiest-minute])))

(defn second-strategy [raw-logs]
  (->> raw-logs
       process-logs
       pick-guard-by-sleepiest-minute))

(defn solve-day4-b []
  (->> (slurp "resources/day4.txt")
       second-strategy
       encode-answer))

;; Answer: 56901
