(ns day-4
  (:require [clojure.string :as str]))

(def group-boards-xf
  "A transducer that takes an array of strings and returns a collection of collections:
   The first is the series of \"moves\" that will occur in the game, 
   the rest are seqs of strings of various player's boards."
  (comp
   (partition-by #(= "" %))
   (filter #(not= '("") %))))

(defn board-string->maps
  "Transforms a board string into a map.
   `=> (board-string->maps \" 8  2 23  4 24\"`)
   ({:marked false, :value \"8\"} 
    {:marked false, :value \"2\"} 
    {:marked false, :value \"23\"} 
    {:marked false, :value \"4\"} 
    {:marked false, :value \"24\"})"
  [board-string]
  (->> (str/split board-string #"\s")
       (sequence
        (comp
         (filter #(not= "" %))
         (map #(hash-map :value % :marked false))))))

(defn parse-input
  "When given the puzzle input, returns the following shape:
   `{:moves [list of moves]
     :board-strings [list of lists of original board rows as strings]}`
     :boards [list of lists of maps {:marked boolean, :value string}]"
  [input]
  (->> input
       str/split-lines
       (into [] group-boards-xf)
       (#(hash-map :moves (first %) :board-strings (rest %)))
       ((fn [{:keys [board-strings] :as m}]
          (assoc m :boards
                 (map (partial mapcat board-string->maps) board-strings))))))

(comment
  
  ;; => ({:marked false, :value "8"} {:marked false, :value "2"} {:marked false, :value "23"} {:marked false, :value "4"} {:marked false, :value "24"})


  ;; => ["" "8" "" "2" "23" "" "4" "24"]

  )
;; => {:moves ["7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"], 
;;     :board-strings (["22 13 17 11  0"
;;                      " 8  2 23  4 24"
;;                      "21  9 14 16  7"
;;                      " 6 10  3 18  5"
;;                      " 1 12 20 15 19"]
;;                     [" 3 15  0  2 22"
;;                      " 9 18 13 17  5"
;;                      "19  8  7 25 23"
;;                      "20 11 10 24  4"
;;                      "14 21 16 12  6"] 
;;                     ["14 21 17 24  4"
;;                      "10 16 15  9 19"
;;                      "18  8 23 26 20"
;;                      "22 11 13  6  5"
;;                      " 2  0 12  3  7"])}




