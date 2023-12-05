(ns day_05
  "Day 5: If You Give A Seed A Fertilizer (https://adventofcode.com/2023/day/5)"
  (:require [clojure.string :as str]))

(def re-header #"[a-z]+(?=-to-)|(?<=-to-)[a-z]+")
(defn parse-longs [ln] (->> ln (re-seq #"\d+") (map parse-long)))
(defn or-seq [xs ys] (map #(or %1 %2) xs ys))
(defn nils [cnt] (take cnt (repeat nil)))

(defn apply-map [[dst-start src-start len] n]
  (let [diff (- n src-start)]
    (when (<= 0 diff (dec len))
      (+ dst-start diff))))

; exploring loop-recur
; it turns out that src & dst are actually not needed
(defn locations [[first-ln & rst-lns]]
  (loop [[ln & rst] (remove empty? rst-lns)
         src nil
         dst nil
         nums-at-entry (parse-longs first-ln)
         nums-mapped (nils (count nums-at-entry))]
    (if-let [[src' dst'] (re-seq re-header ln)]
      (recur rst
             src'
             dst'
             (or-seq nums-mapped nums-at-entry)
             (nils (count nums-mapped)))
      (let [almck-map (parse-longs ln)
            nums-mapped' (or-seq nums-mapped
                                 (->> nums-at-entry
                                      (map (partial apply-map almck-map))))]
        (if (nil? rst)
          (or-seq nums-mapped nums-at-entry)
          (recur rst src dst nums-at-entry nums-mapped'))))))

(let [input-lns (->> "input/day-05.txt" slurp str/split-lines)]
  {:part1 (->> input-lns
               locations
               (reduce min))})

(comment
  (locations example) ; should be (82 43 86 35)
  (def example
    ["seeds: 79 14 55 13"
     ""
     "seed-to-soil map:"
     "50 98 2"
     "52 50 48"
     ""
     "soil-to-fertilizer map:"
     "0 15 37"
     "37 52 2"
     "39 0 15"
     ""
     "fertilizer-to-water map:"
     "49 53 8"
     "0 11 42"
     "42 0 7"
     "57 7 4"
     ""
     "water-to-light map:"
     "88 18 7"
     "18 25 70"
     ""
     "light-to-temperature map:"
     "45 77 23"
     "81 45 19"
     "68 64 13"
     ""
     "temperature-to-humidity map:"
     "0 69 1"
     "1 0 69"
     ""
     "humidity-to-location map:"
     "60 56 37"
     "56 93 4"]))



