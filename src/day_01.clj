(ns day-01
  (:require [clojure.string :as str]
            [clojure.test :refer :all]))


; my first clunky attempt without knowing about re-seq and juxt
(defn calib-values [str]
  (let [digits (->> (str/split str #"")
                    (filter #(re-matches #"[0-9]" %))
                    (map #(Integer/parseInt %)))
        first (first digits)
        last (last digits)]
    (case (count digits)
      0 0
      (+ (* first 10) last))))

; really cool solution from https://github.com/vollcheck/aoc/blob/master/src/y23/day01.clj
; that uses re-seq and juxt combination; so powerful and expressive
(defn calib-values2 [str]
  (->> str
       (re-seq #"[0-9]")
       ((juxt first last))
       (str/join)
       parse-long))

(defn total-calib [lines]
  (println lines)
  (->> lines
    (map calib-values)
    (reduce + 0)))

(def input-lines
  (->> "/Users/marcel/projects/personal-finance/aoc/day-1.txt"
       slurp
       str/split-lines))

(total-calib input-lines)

(deftest t-calib-values
  (is (= 55 (calib-values "5")))
  (is (= 55 (calib-values "55")))
  (is (= 0 (calib-values "asdad")))
  (is (= 58 (calib-values "xxf5threefourtq58"))))

(deftest t-total-calib
  (is (= 142 (total-calib ["1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"]))))



; part 2
(def full-words {"one" "1"
                 "two" "2"
                 "three" "3"
                 "four" "4"
                 "five" "5"
                 "six" "6"
                 "seven" "7"
                 "eight" "8"
                 "nine" "9"})

; Left unfished - thinking of some recursive function, sth like
; (fix-line "one23two") would call (str "1" (fix-line "23two"))
(defn fix-line [line]
  (reduce (fn [acc [word-regex val]]
            (str/replace acc word-regex val)) line full-words))

; cool solution from https://github.com/vollcheck/aoc/blob/master/src/y23/day01.clj
; which uses look-ahead regexes, with the combination of juxt from part 1
(->> "eightwo"
  (re-seq #"(?=(one|two|three|four|five|six|seven|eight|nine|[0-9]))")
  (map #(second %))
  ((juxt first last)))

(defn total-calib-fixed [lines]
  (total-calib (map fix-line lines)))

(deftest t-fix-line
  (is (= "158" (fix-line "one5eight")))
  (is (= "12" (fix-line "onetwo")))
  (is (= "219" (fix-line "two1nine")))
  (is (= "8wo3" (fix-line "eightwothree")))
  (is (= "abc123xyz" (fix-line "abcone2threexyz")))
  (is (= "x1ne34" (fix-line "xtwone3four"))))

(deftest t-total-calib-fixed
  (is (= 281 (total-calib-fixed
               ["two1nine"
                "eightwothree"
                "abcone2threexyz"
                "xtwone3four"
                "4nineeightseven2"
                "zoneight234"
                "7pqrstsixteen"]))))