(ns day-7
  (:require [clojure.string :as str]))

(def sample "16,1,2,0,4,2,7,1,2,14")

(let [input (slurp "inputs/day_7_input.txt")
      nums (map #(Integer/parseInt %) (str/split input #","))
      target-fuel (nth (sort nums) (/ (count nums) 2))]
  (reduce + (map #(Math/abs (- % target-fuel)) nums)))