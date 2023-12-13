(ns day_07
  "Day 7: Camel Cards (https://adventofcode.com/2023/day/7)"
  (:require [clojure.string :as str]))

(def type-strengths
  (zipmap (reverse [[5] [4 1] [3 2] [3 1 1] [2 2 1] [2 1 1 1] [1 1 1 1 1]])
          (range)))

(defn hand-type [hand]
  (->> hand frequencies vals sort reverse))

(defn hand-type-with-jokers [hand]
  (let [hand-wo-jokers (str/replace hand #"J" "")
        jokers (- (count hand) (count hand-wo-jokers))]
    (if (empty? hand-wo-jokers)
      [jokers]
      (let [[first & rst] (hand-type hand-wo-jokers)]
        (mapv identity (remove nil? (flatten [(+ first jokers) rst])))))))


(def read-file (comp str/split-lines slurp))

(def parse-xf
  (map (fn [ln] (str/split ln #" "))))


(defn solve1 [lns]
  (let [card-strengths (zipmap (reverse "AKQJT98765432") (range))]
    (->> lns
         (sequence parse-xf)
         (map (fn [[hand bid]] {:hand hand
                                :bid (parse-long bid)
                                :strengths [(type-strengths (hand-type hand))
                                            (mapv card-strengths hand)]}))
         (sort-by :strengths)
         (map-indexed vector)
         (map (fn [[idx {:keys [bid]}]] (* (inc idx) bid)))
         (reduce +))))

(defn solve2 [lns]
  (let [card-strengths (zipmap (reverse "AKQT98765432J") (range))]
    (->> lns
         (sequence parse-xf)
         (map (fn [[hand bid]] {:hand hand
                                :bid (parse-long bid)
                                :strengths [(type-strengths (hand-type-with-jokers hand))
                                            (mapv card-strengths hand)]}))
         (sort-by :strengths)
         (map-indexed vector)
         (map (fn [[idx {:keys [bid]}]] (* (inc idx) bid)))
         (reduce +))))


(def input (read-file "input/day-07.txt"))
(solve1 input)
(solve2 input)

(comment
  (def example (read-file "input/example-07.txt"))
  (solve1 example) ;6640
  (solve2 example)) ; 5905




