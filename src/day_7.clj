(ns day-7
  (:require [clojure.string :as str]
            [criterium.core :as criterium]))

(def sample "16,1,2,0,4,2,7,1,2,14")

(defn average
  "Calculates the average of the inputs."
  [& xs]
  (/ (reduce + xs) (count xs)))

(defn unchecked-growing-diff
  [start target]
  (binding [*unchecked-math* true]
    (loop [num start
           so-far 0
           steps 0]
      (cond
        (< num target) (recur (inc num) (+ so-far (inc steps)) (inc steps))
        (> num target) (recur (dec num) (+ so-far (inc steps)) (inc steps))
        (= num target) so-far))))

(let [input (slurp "inputs/day_7_input.txt")
      nums (map #(Integer/parseInt %) (str/split input #","))]
  (apply min
         (for [num (range 0 (apply max nums))]
           (reduce + (map #(unchecked-growing-diff % num) nums))))
  #_(reduce + (map #(Math/abs (- % target-fuel)) nums)))