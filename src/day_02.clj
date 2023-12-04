(ns aoc_2023_02_gear-ratios
  "Key learnings:
  - put dev fiddles to a dev-tree, as mentioned in Functional Design in Clojure
  - merge-with; to merge maps with an aggregation function
  - establish a good vocabularly/grammar before
  "
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

; I assume the elegant way would involve some neat regex-trick
; or some better parsing option. As I don't know, I'll use str/split
(defn parse-hand [s]
  (->> (str/split s #", ")
       (map #(str/split % #" "))
       (map (fn [[cnt color]] [(keyword color) (parse-long cnt)]))
       (into {})))

(defn parse-game [line]
  (let [[game-segm sets-segm] (str/split line #": ")
        [_ id] (str/split game-segm #" ")
        set-strs (str/split sets-segm #"; ")]
    {:id (parse-long id)
     :sets (map parse-hand set-strs)}))

(def config
  {:red 12 :green 13 :blue 14})

(defn possible? [set]
  (every? true? (map (fn [[col cnt]] (<= cnt (col config))) set)))

(defn part1 [lines]
  (->> lines
       (map parse-game)
       (filter (fn [{:keys [sets]}] (every? possible? sets)))
       (map :id)
       (reduce +)))

; could be improved with merge-with, see learnings below
(defn fewest-of [color game]
  ( ->> (:sets game)
        (map color)
        (filter number?)
        (apply max 0)))

; could be improved with reduce, see learnings below
(defn min-power [game]
  (* (fewest-of :blue game)
     (fewest-of :red game)
     (fewest-of :green game)))

(defn part2 [lines]
  (->> lines
       (map parse-game)
       (map min-power)
       (reduce +)))

(let [input (->> "input/day-02.txt" slurp str/split-lines)]
  {:part1 (part1 input)
   :part2 (part2 input)})



(comment
  (def head-str "1 red, 2 green, 6 blue")
  (parse-hand head-str)
  (def line "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
  (parse-game line)
  (def some-hand {:blue 3, :red 4})
  (possible? some-hand)
  (possible? {:blue 100, :red 4})
  (possible? {})
  (def game {:id 1
             :sets (list {:blue 3 :red 4} {:red 1 :green 2 :blue 6} {:green 2})}))

(def example ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
              "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
              "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
              "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
              "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"])

(deftest t-game
  (is (= {:id 1
          :sets (list {:blue 3 :red 4} {:red 1 :green 2 :blue 6} {:green 2})}
         (parse-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))))

(deftest t-games
  (is (= 8 (part1 example)))
  (is (= 2286 (part2 example))))


; some learning from https://github.com/ChrisBlom/advent-of-code/blob/master/src/adventofcode/2023/day02.clj
(comment
  ; merge-with could be used to simplify the min-power
  (merge-with +
              {:a 1  :b 2}
              {:a 9  :b 98 :c 0})
  ;;=> {:c 0, :a 10, :b 100}

  (defn min-power [game]
    (->> (apply merge-with max (:sets game))
        vals
        (reduce *))))
