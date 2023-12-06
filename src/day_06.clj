(ns day_06
  "Day 6: Wait for it (https://adventofcode.com/2023/day/6)"
  (:require [clojure.string :as str]
            [clojure.math :as m]))

(defn read-file [f]
  (->> f slurp str/split-lines))

(defn parse-longs [ln]
  (->> ln (re-seq #"\d+") (mapv parse-long)))

(defn parse-input [input]
  (->> input (mapv parse-longs) (apply map vector)))

(defn roots
  "Find root of ax^2 + bx + c = 0"
  [a b c]
  (let [disc (m/sqrt (- (* b b) (* 4 a c)))]
    [(/ (+ (- b) disc) (* 2 a))
     (/ (- (- b) disc) (* 2 a))]))

(defn next-long [x] (long (if (= x (m/floor x)) (inc x) (m/ceil x))))
(defn prev-long [x] (long (if (= x (m/ceil x)) (dec x) (m/floor x))))

(defn winning-times [[t max-d]]
  (let [[r1 r2] (roots (- 1) t (- max-d))
        _ (println r1 r2)]
    [(next-long r1) (prev-long r2)]))

(defn ways-to-win [pairs]
  (->> pairs
     (map winning-times)
     (map (fn [[min max]] (inc (- max min))))
     (reduce *)))

; part 2
(defn parse-input-kern [input]
  [(->> input
        (map (fn [ln] (str/replace ln #"[^0-9]" "")))
        (map parse-long))])

(-> "input/day-06.txt" read-file parse-input ways-to-win)
(-> "input/day-06.txt" read-file parse-input-kern ways-to-win)

(comment
  (def example (read-file "input/example-06.txt"))
  (ways-to-win (parse-input example)) ; should be 288
  (ways-to-win (parse-input-kern example))) ; should be 71503




