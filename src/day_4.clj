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
       (#(hash-map :moves (str/split (get-in % [0 0]) #"\,") :board-strings (rest %)))
       ((fn [{:keys [board-strings] :as m}]
          (assoc m :boards
                 (mapv (partial into [] (mapcat board-string->maps)) board-strings))))))

(defn mark-board-from-move
  "When given a move (a string of an integer), marks the board's cells as marked if the value equals the move."
  [move board]
  (mapv #(if (= (:value %) move)
           (assoc % :marked true)
           %) board))

(defn get-rows
  "When given a set of 25 cells, creates rows."
  [board]
  (map
   #(subvec board (* (dec %) 5) (* % 5))
   (range 1 6)))

(defn get-columns
  "When given a set of 25 cells, creates columns."
  [board]
  (map #(vals (select-keys board (map (partial * %) [0 5 10 15 20]))) (range 1 6)))

(defn get-diagonals
  "When given a set of 25 cells, creates diagonals."
  [board]
  [(vals (select-keys board [5 9 13 17 21])) (vals (select-keys board [0 6 12 18 14] #_(map (partial * 6) [0 1 2 3 4])))])

(comment
  (let [data (parse-input (slurp "inputs/day_4_sample.txt"))
        boards (:boards data)
        move (first (:moves data))
        marked-boards (mapv (partial mark-board-from-move move) boards)]
    (def *marked-boards marked-boards))
  
  ; working on determining a winner
  (let [board (first *marked-boards)
        series (concat (get-rows board) (get-columns board) (get-diagonals board))]
    (->> series
         (map (partial map :marked))
         (filter (partial every? identity))))
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




