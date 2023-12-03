(ns dev_03
  "
  Vocab:
  - engine schematic: visual repr of an engine, consisting of numbers, symbols and dots
  - adjacent - right next to horizontally, vertically or diagonally
  - part number - any number adjacent to a symbol
  - gear - a * symbol adjacent to exactly two part numbers

  How to represent data?
  - approach 1: 2D matrix
  - approach 2: seq of lines, with the context of prev and next line
  - approach 3: maps of numbers with their positions
  Approach 2 chosen, parsing & checking for adjacent symbols seems easiest here

  Excluded:
  - tests (relied on comments and REPL only)
  - final refactoring

  Learnings before checking other solutions:
  - In Part1, data was structured around numbers and their context.
    Part2, however, required structure around a particular symbol, which
    needed extra modelling
  - I've chosen an approach to always-be-adding to existing maps, which in this
    case made it easier to build the logic.
  - I keep forgetting about fn signatures, maybe it's time for Anki

  Learnings after checking other solutions:
  - Looking at solution at Clojurian Slack, the chosen representation was not optimal,
    and I have ended up with quite a bloated code
  - There's (dec x) and (inc x) functions
  - (for) allows :let and :when, that would be useful https://gitlab.com/maximoburrito/advent2023/-/blob/main/src/day03/main.clj?ref_type=heads#L139
  - tools missing in my arsenal: reduce-kv, mapcat, loop https://elken.github.io/aoc/src/solutions/2023/day03/
  "
  (:require [clojure.string :as str]))


(defn parse-schematic [lines]
  (let [with-ctx (map vector
                      (into [nil] lines)
                      lines
                      (conj (vec (rest lines)) nil))]
    (map-indexed
      (fn [idx m] (assoc m :curr-line-no idx))
      (for [[prev curr next] with-ctx]
        {:prev prev
         :curr curr
         :next next}))))

(comment (->> example parse-schematic))

(defn re-seq-pos
  "Like re-seq, but includes :start (incl) and :end (excl) of a :match
  Taken from https://stackoverflow.com/questions/21191045/get-string-indices-from-the-result-of-re-seq "
  [pattern string]
  (let [m (re-matcher pattern string)]
    ((fn step []
       (when (. m find)
         (cons {:start (. m start) :end (. m end) :group (. m group)}
           (lazy-seq (step))))))))

(defn extract-numbers [schematic]
  (for [{:keys [curr] :as ctx} schematic
        num-with-pos (re-seq-pos #"[\d]+" curr)]
    (merge num-with-pos ctx)))

(comment (->> example parse-schematic extract-numbers))

(defn get-or-nil
  "Like get on vector, but return nil if out-of-bound"
  [v idx]
  (cond
    (< idx 0) nil
    (>= idx (count v)) nil
    :else (get v idx)))

(comment (get-or-nil "abc" 3))

(defn with-adjacent-chars [{:keys [start end curr prev next curr-line-no] :as num}]
  (let [find-char (fn [line line-no pos] {:pos pos
                                          :line-no line-no
                                          :char (get-or-nil line pos)})
        char-left (find-char curr curr-line-no (- start 1))
        char-right (find-char curr curr-line-no end)
        chars-up-down (for [idx (range (- start 1) (+ end 1))]
                        [(find-char prev (- curr-line-no 1) idx)
                         (find-char next (+ curr-line-no 1) idx)])]
    (merge
      num
      {:adjacent-chars (->> [char-left char-right chars-up-down]
                           flatten
                           (filter some?))})))

(comment (->> example parse-schematic extract-numbers (map with-adjacent-chars)))

(defn symb? [ch] (boolean (re-matches #"[^.0-9]" (str ch))))
(comment (symb? \$))

(defn sum-part-numbers [lines]
  (->> lines
       parse-schematic
       extract-numbers
       (map with-adjacent-chars)
       (filter (fn [{:keys [adjacent-chars]}] (some (comp symb? :char) adjacent-chars)))
       (map (comp parse-long :group))
       (reduce + 0)))

(comment (sum-part-numbers example)) ; should be 4361


(defn with-stars [{:keys [adjacent-chars] :as num}]
  (let [stars (->> adjacent-chars
                  (filter #(= \* (:char %))))]
    (merge num {:stars stars})))

(defn stars
  "Get all stars, incl their positions"
  [nums-with-chars]
  (->> nums-with-chars
       (map with-stars)
       (map :stars)
       flatten
       set))

(comment (stars (->> example parse-schematic extract-numbers (map with-adjacent-chars))))

(defn sum-gear-ratios [lines]
  (let [nums (->> lines
                  parse-schematic
                  extract-numbers
                  (map with-adjacent-chars))
        part-numbers-around-stars (for [star (stars nums)]
                                      (->> nums
                                           (filter (fn [{:keys [adjacent-chars]}]
                                                     (some #(= % star) adjacent-chars)))
                                           (map (comp parse-long :group))))]
    (->> part-numbers-around-stars
         (filter #(= 2 (count %)))
         (map #(reduce * 1 %))
         (reduce + 0))))

(comment (sum-gear-ratios example)) ; should be 467835



(let [input (->> "input/day-03.txt" slurp str/split-lines)]
  {:part1 (sum-part-numbers input)
   :part2 (sum-gear-ratios input)})


(comment
  (def example
    ["467..114.."
     "...*......"
     "..35..633."
     "......#..."
     "617*......"
     ".....+.58."
     "..592....."
     "......755."
     "...$.*...."
     ".664.598.."]))

