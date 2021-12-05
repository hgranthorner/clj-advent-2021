(ns day-4
  (:require [clojure.string :as str]))

(def group-boards-xf
  "A transducer that takes an array of strings and returns a collection of collections:
   The first is the series of \"moves\" that will occur in the game, 
   the rest are seqs of strings of various player's boards."
  (comp
   (partition-by #(= "" %))
   (filter #(not= '("") %))))

(->> (slurp "inputs/day_4_sample.txt")
     str/split-lines
     (into [] group-boards-xf)
     (#(hash-map :moves (first %) :board-strings (rest %))))



