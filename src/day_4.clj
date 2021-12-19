(ns day-4
  (:refer-clojure :exclude [any?])
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint :refer [print-table pprint]]
            [com.rpl.specter :as s]
            [clojure.tools.trace :as trace]
            [clojure.test :as test]))

(defn mapcatv
  "Like mapcat, but returns a vector... like mapv"
  [f coll]
  (let [results (map f coll)]
    (reduce into [] results)))

(defn parse-input
  [^String input]
  (->> input
       str/split-lines
       (partition-by (partial = ""))
       (filter (partial not= '("")))
       (#(hash-map :moves (-> % first first (str/split #",")) :boards (rest %)))
       ((fn [x]
          (assoc
           x :boards
           (mapv #(mapcatv
              ; We want to trim the strings, then split on spaces, then group into maps
                   (comp
                    (partial into [] (map (fn [y] (hash-map :value y :marked? false))))
                    (fn [y] (str/split y #"\s+"))
                    str/trim) %)
                (:boards x)))))))

(defn get-cols
  [board]
  (map (fn [i]
         (vals
          (select-keys board
                       (map #(+ i (* 5 %)) (range 5)))))
       (range 5)))

(defn get-rows
  [board]
  (map (fn [i]
         (subvec board (* 5 i) (+ 5 (* 5 i))))
       (range 5)))

(defn get-diagonals
  [board]
  [(vals (select-keys board [0 6 12 18 24]))
   (vals (select-keys board [4 8 12 16 20]))])

(defn play-move
  "Returns the new state. Takes the current state and a move."
  [state move]
  (->> (update state :moves rest)
       (s/transform [:boards s/ALL s/ALL]
                    #(if (= move (:value %))
                       (assoc % :marked? true)
                       %))))

(defn not-empty?
  "Checks if coll is empty."
  [coll]
  (not= (count coll) =))

(defn any?
  "Checks if a predicate is true across any members of a collection."
  [pred coll]
  (not-empty (filter pred coll)))

(defn all?
  "Checks if a predicate is true across all members of a collection."
  [pred coll]
  (= (count (filter pred coll)) (count coll)))

(defn solution-one
  "Solves the first puzzle."
  [input]
  (loop [state (parse-input (slurp input))
         move (first (:moves state))]
    (if move
      (let [new-state (play-move state move)
            cols (map get-cols (:boards new-state))
            rows (map get-rows (:boards new-state))]
        (if-let [winner (or (any? #(any? (partial all? :marked?) %) cols)
                            (any? #(any? (partial all? :marked?) %) rows))]
          (->> (first winner)
               (map #(filter (fn [x] (not (:marked? x))) %))
               flatten
               (map :value)
               (map #(Integer/parseInt %))
               (reduce +)
               (* (Integer/parseInt move)))
          (recur new-state (first (new-state :moves)))))
      (throw (Exception. "Could not find solution!")))))

(comment
  (trace/untrace-vars all?)
  (trace/trace-vars filter)

  (let [board [0  1  2  3  4
               5  6  7  8  9
               10 11 12 13 14
               15 16 17 18 19
               20 21 22 23 24]]
    (get-rows board)))

(test/deftest first-solution
  (test/is (= (solution-one "inputs/day_4_input.txt") 50008)))

(test/run-tests)