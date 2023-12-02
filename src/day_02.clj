(ns day-02
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))

; I assume the elegant way would involve some neat regex-trick
; or some better parsing option. As I don't know, I am falling back to
; the basics  of str/split

(defn parse-set-str [set-str]
  (->> (str/split set-str #", ")
       (map #(str/split % #" "))
       (map (fn [[cnt color]] [(keyword color) (parse-long cnt)]))
       (into {})))

(comment
  (def set-str "1 red, 2 green, 6 blue")
  (parse-set-str set-str))

(defn parse-line [line]
  (let [[game-segm sets-segm] (str/split line #": ")
        [_ id] (str/split game-segm #" ")
        set-strs (str/split sets-segm #"; ")]
    {:id (parse-long id)
     :sets (map parse-set-str set-strs)}))

(comment
  (def line "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
  (parse-line line))

(deftest t-game
  (is (= {:id 1
          :sets (list {:blue 3 :red 4} {:red 1 :green 2 :blue 6} {:green 2})}
         (parse-line "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))))

(def config
  {:red 12 :green 13 :blue 14})

(defn possible? [set]
  (every? true? (map (fn [[col cnt]] (<= cnt (col config))) set)))

(comment
  (def some-set {:blue 3, :red 4})
  (possible? some-set)
  (possible? {:blue 100, :red 4})
  (possible? {}))


(defn part1 [lines]
  (->> lines
       (map parse-line)
       (filter (fn [{:keys [sets]}] (every? possible? sets)))
       (map :id)
       (reduce +)))

(deftest t-games
  (let [lines ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
               "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
               "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
               "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
               "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"]]
    (is (= 8 (part1 lines)))))

(def input-lines
  (->> "input/day-02.txt"
       slurp
       str/split-lines))


(part1 input-lines)
