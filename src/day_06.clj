(ns day_06
  "Day 6: Wait for it (https://adventofcode.com/2023/day/6)"
  (:require [clojure.string :as str]
            [clojure.math :as m]))

(defn roots
  "Find root of ax^2 + bx + c = 0, assuming a!=0"
  [a b c]
  (let [disc (m/sqrt (- (* b b) (* 4 a c)))]
    [(/ (+ (- b) disc) (* 2 a))
     (/ (- (- b) disc) (* 2 a))]))

(defn whole? [x] (= x (m/floor x)))
(defn next-long [x] (long (if (whole? x) (inc x) (m/ceil x))))
(defn prev-long [x] (long (if (whole? x) (dec x) (m/floor x))))

(defn winning-times
  "race_time = t - t_chosen
   velocity = t_chosen
   distance = race_time * velocity
            = (t - t_chosen) * t_chosen
  From there, we want to find t_chosen such that:
  - t_chosen^2 + t * t_chosen - distance > 0"
  [[t max-d]]
  (let [[r1 r2] (roots (- 1) t (- max-d))]
    [(next-long r1) (prev-long r2)]))

(defn ways-to-win [[min max]] (inc (- max min)))

(def read-file (comp str/split-lines slurp))
(def transpose (partial apply map vector))

(defn solve1 [lns]
  (let [parse-lns (comp (map (partial re-seq #"\d+"))
                        (map (partial map parse-long)))]
    (->> lns
         (sequence parse-lns)
         transpose
         (map winning-times)
         (map ways-to-win)
         (reduce *))))

(defn solve2 [lns]
  (let [parse-lns (comp (map (fn [ln] (str/replace ln #"[^0-9]" "")))
                        (map parse-long))]
    (->> lns
         (sequence parse-lns)
         winning-times
         ways-to-win)))

(solve1 (read-file "input/day-06.txt"))
(solve2 (read-file "input/day-06.txt"))

(comment
  (solve1 (read-file "input/example-06.txt")) ; 288
  (solve2 (read-file "input/example-06.txt"))) ; 71503
