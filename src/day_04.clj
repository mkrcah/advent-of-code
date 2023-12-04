(ns day_04
  "
  Day 4: Scratchcards
  https://adventofcode.com/2023/day/4
  "
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.math :refer [pow]]))

(defn cards [lns]
  (->> lns
       (map #(str/split % #":"))
       (map second)
       (map #(str/split % #"\|"))
       (map (partial map #(re-seq #"\d+" %)))))

(defn count-matches [card]
  (->> card
    (map (partial set))
    (apply set/intersection)
    count))

(defn points [matches]
  (when (< 0 matches) (pow 2 (dec matches))))

(defn part1 [lines]
  (->> lines
       cards
       (map count-matches)
       (keep points)
       (reduce +)))

(let [input (->> "input/day-04.txt" slurp str/split-lines)]
  {:part1 (part1 input)})


(comment
  (part1 example) ;13
  (def example
    ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
     "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
     "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
     "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
     "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
     "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"]))


