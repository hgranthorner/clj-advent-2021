(ns day-4
  (:require [clojure.string :as str]
            [clojure.pprint :as pprint]))

(defn inspect
  [x]
  (pprint/pprint x)
  x)

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
  (map #(vals (select-keys board (map (partial + %) [0 5 10 15 20]))) (range 5)))

(defn get-diagonals
  "When given a set of 25 cells, creates diagonals."
  [board]
  [(vals (select-keys board [5 9 13 17 21])) (vals (select-keys board [0 6 12 18 14]))])

(defn check-winner
  "Checks if the board has a winning serie. Otherwise returns empty seq."
  [board]
  (let [series (concat (get-rows board) (get-columns board) (get-diagonals board))]
    (def *data {:series series :rows (get-rows board) :columns (get-columns board) :diagonals (get-diagonals board)})
    (->> series
         (map (partial map :marked))
         (filter #(every? identity %)))))

(defn first-truthy-index
  "Returns the index of the first truthy element."
  [xs]
  (loop [index 0]
    (if (< index (count xs))
      (if (nth xs index)
        index
        (recur (inc index)))
      nil)))

(defn solution-one
  [input]
  (loop [state (parse-input input)
         move (first (:moves state))]
    (let [marked-boards (mapv (partial mark-board-from-move move) (:boards state))
          checked (map check-winner marked-boards)]
      (if (first (filter not-empty checked))
        (do
          (pprint/pprint move)
          (pprint/pprint (nth marked-boards (first-truthy-index (map not-empty checked))))
          (* (Integer/parseInt move)
             (reduce
              +
              (eduction (comp
                         (filter #(not (:marked %)))
                         (map :value)
                         (map #(Integer/parseInt %)))
                        (nth marked-boards (first-truthy-index (map not-empty checked)))))))
        (let [new-state (update state :moves rest)]
          (when (empty? (:moves new-state))
            (pprint/pprint (:boards new-state))
            (throw (Exception. "No more moves!")))
          (recur (assoc new-state :boards marked-boards) (-> new-state :moves first)))))))

(comment
  (solution-one (slurp "inputs/day_4_input.txt"))
  (first (filter not-empty [[] [1]]))
  (let [data (parse-input (slurp "inputs/day_4_sample.txt"))
        boards (:boards data)
        move (first (:moves data))
        marked-boards (mapv (partial mark-board-from-move move) boards)]
    (def *marked-boards marked-boards))

  (map count (:columns *data))
  ;; => {:series ([{:marked true, :value "14"} {:marked false, :value "21"} {:marked false, :value "17"} {:marked false, :value "24"} {:marked false, :value "4"}] [{:marked false, :value "10"} {:marked false, :value "16"} {:marked false, :value "15"} {:marked false, :value "9"} {:marked false, :value "19"}] [{:marked false, :value "18"} {:marked false, :value "8"} {:marked false, :value "23"} {:marked false, :value "26"} {:marked false, :value "20"}] [{:marked false, :value "22"} {:marked false, :value "11"} {:marked false, :value "13"} {:marked false, :value "6"} {:marked false, :value "5"}] [{:marked false, :value "2"} {:marked false, :value "0"} {:marked false, :value "12"} {:marked false, :value "3"} {:marked false, :value "7"}] ({:marked true, :value "14"} {:marked false, :value "10"} {:marked false, :value "18"} {:marked false, :value "22"} {:marked false, :value "2"}) ({:marked true, :value "14"} {:marked false, :value "18"} {:marked false, :value "2"}) ({:marked true, :value "14"} {:marked false, :value "22"}) ({:marked true, :value "14"} {:marked false, :value "2"}) ({:marked true, :value "14"}) ({:marked false, :value "10"} {:marked false, :value "19"} {:marked false, :value "26"} {:marked false, :value "13"} {:marked false, :value "0"}) ({:marked true, :value "14"} {:marked false, :value "16"} {:marked false, :value "23"} {:marked false, :value "6"} {:marked false, :value "20"})), :rows ([{:marked true, :value "14"} {:marked false, :value "21"} {:marked false, :value "17"} {:marked false, :value "24"} {:marked false, :value "4"}] [{:marked false, :value "10"} {:marked false, :value "16"} {:marked false, :value "15"} {:marked false, :value "9"} {:marked false, :value "19"}] [{:marked false, :value "18"} {:marked false, :value "8"} {:marked false, :value "23"} {:marked false, :value "26"} {:marked false, :value "20"}] [{:marked false, :value "22"} {:marked false, :value "11"} {:marked false, :value "13"} {:marked false, :value "6"} {:marked false, :value "5"}] [{:marked false, :value "2"} {:marked false, :value "0"} {:marked false, :value "12"} {:marked false, :value "3"} {:marked false, :value "7"}]), :columns (({:marked true, :value "14"} {:marked false, :value "10"} {:marked false, :value "18"} {:marked false, :value "22"} {:marked false, :value "2"}) ({:marked true, :value "14"} {:marked false, :value "18"} {:marked false, :value "2"}) ({:marked true, :value "14"} {:marked false, :value "22"}) ({:marked true, :value "14"} {:marked false, :value "2"}) ({:marked true, :value "14"})), :diagonals [({:marked false, :value "10"} {:marked false, :value "19"} {:marked false, :value "26"} {:marked false, :value "13"} {:marked false, :value "0"}) ({:marked true, :value "14"} {:marked false, :value "16"} {:marked false, :value "23"} {:marked false, :value "6"} {:marked false, :value "20"})]}


  (loop [state (parse-input (slurp "inputs/day_4_sample.txt"))
         move (first (:moves state))]
    (let [marked-boards (mapv (partial mark-board-from-move move) (:boards state))
          checked (map check-winner marked-boards)]
      (if (first (filter not-empty checked))
        (reduce
         +
         (eduction (comp
                    (filter #(not (:marked %)))
                    (map :value)
                    (map #(Integer/parseInt %)))
                   (nth marked-boards (first-truthy-index (map not-empty checked)))))
        (let [new-state (update state :moves rest)]
          (when (empty? (:moves new-state))
            (pprint/pprint (:boards new-state))
            (throw (Exception. "No more moves!")))
          (recur (assoc new-state :boards marked-boards) (-> new-state :moves first))))))

  (map check-winner [[{:marked false, :value "22"} {:marked false, :value "13"} {:marked false, :value "17"} {:marked false, :value "11"} {:marked false, :value "0"} {:marked false, :value "8"} {:marked false, :value "2"} {:marked false, :value "23"} {:marked false, :value "4"} {:marked false, :value "24"} {:marked false, :value "21"} {:marked false, :value "9"} {:marked false, :value "14"} {:marked false, :value "16"} {:marked true, :value "7"} {:marked false, :value "6"} {:marked false, :value "10"} {:marked false, :value "3"} {:marked false, :value "18"} {:marked false, :value "5"} {:marked false, :value "1"} {:marked false, :value "12"} {:marked false, :value "20"} {:marked false, :value "15"} {:marked false, :value "19"}] [{:marked false, :value "3"} {:marked false, :value "15"} {:marked false, :value "0"} {:marked false, :value "2"} {:marked false, :value "22"} {:marked false, :value "9"} {:marked false, :value "18"} {:marked false, :value "13"} {:marked false, :value "17"} {:marked false, :value "5"} {:marked false, :value "19"} {:marked false, :value "8"} {:marked true, :value "7"} {:marked false, :value "25"} {:marked false, :value "23"} {:marked false, :value "20"} {:marked false, :value "11"} {:marked false, :value "10"} {:marked false, :value "24"} {:marked false, :value "4"} {:marked false, :value "14"} {:marked false, :value "21"} {:marked false, :value "16"} {:marked false, :value "12"} {:marked false, :value "6"}] [{:marked false, :value "14"} {:marked false, :value "21"} {:marked false, :value "17"} {:marked false, :value "24"} {:marked false, :value "4"} {:marked false, :value "10"} {:marked false, :value "16"} {:marked false, :value "15"} {:marked false, :value "9"} {:marked false, :value "19"} {:marked false, :value "18"} {:marked false, :value "8"} {:marked false, :value "23"} {:marked false, :value "26"} {:marked false, :value "20"} {:marked false, :value "22"} {:marked false, :value "11"} {:marked false, :value "13"} {:marked false, :value "6"} {:marked false, :value "5"} {:marked false, :value "2"} {:marked false, :value "0"} {:marked false, :value "12"} {:marked false, :value "3"} {:marked true, :value "7"}]])

  (filter not-empty '((nil nil nil nil nil nil nil nil nil nil nil nil) (nil nil nil nil nil nil nil nil nil nil nil nil) (nil nil nil nil nil nil nil nil nil nil nil nil)))

  (map Integer/parseInt ["1"])

  ; working on determining a winner
  (map check-winner *marked-boards)
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

(get-diagonals [{:marked true, :value "89"}
                {:marked false, :value "61"}
                {:marked false, :value "97"}
                {:marked true, :value "14"}
                {:marked false, :value "56"}
                {:marked true, :value "32"}
                {:marked false, :value "90"}
                {:marked false, :value "98"}
                {:marked false, :value "69"}
                {:marked true, :value "4"}
                {:marked false, :value "88"}
                {:marked false, :value "58"}
                {:marked false, :value "51"}
                {:marked true, :value "76"}
                {:marked true, :value "66"}
                {:marked false, :value "15"}
                {:marked false, :value "62"}
                {:marked true, :value "35"}
                {:marked false, :value "7"}
                {:marked false, :value "29"}
                {:marked true, :value "95"}
                {:marked true, :value "8"}
                {:marked true, :value "33"}
                {:marked false, :value "73"}
                {:marked true, :value "22"}])
;; => [({:marked true, :value "32"} {:marked true, :value "4"} {:marked true, :value "76"} {:marked true, :value "35"} {:marked true, :value "8"}) ({:marked true, :value "89"} {:marked false, :value "90"} {:marked false, :value "51"} {:marked false, :value "7"} {:marked true, :value "66"})]

;; => (({:marked true, :value "89"} {:marked true, :value "32"} {:marked false, :value "88"} {:marked false, :value "15"} {:marked true, :value "95"}) ({:marked false, :value "61"} {:marked false, :value "90"} {:marked false, :value "58"} {:marked false, :value "62"} {:marked true, :value "8"}) ({:marked false, :value "97"} {:marked false, :value "98"} {:marked false, :value "51"} {:marked true, :value "35"} {:marked true, :value "33"}) ({:marked true, :value "14"} {:marked false, :value "69"} {:marked true, :value "76"} {:marked false, :value "7"} {:marked false, :value "73"}) ({:marked false, :value "56"} {:marked true, :value "4"} {:marked true, :value "66"} {:marked false, :value "29"} {:marked true, :value "22"}))

;; => ([{:marked true, :value "89"} {:marked false, :value "61"} {:marked false, :value "97"} {:marked true, :value "14"} {:marked false, :value "56"}] [{:marked true, :value "32"} {:marked false, :value "90"} {:marked false, :value "98"} {:marked false, :value "69"} {:marked true, :value "4"}] [{:marked false, :value "88"} {:marked false, :value "58"} {:marked false, :value "51"} {:marked true, :value "76"} {:marked true, :value "66"}] [{:marked false, :value "15"} {:marked false, :value "62"} {:marked true, :value "35"} {:marked false, :value "7"} {:marked false, :value "29"}] [{:marked true, :value "95"} {:marked true, :value "8"} {:marked true, :value "33"} {:marked false, :value "73"} {:marked true, :value "22"}])
