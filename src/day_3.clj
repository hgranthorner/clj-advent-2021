(ns day-3
  (:require [clojure.string :as str]))

(declare puzzle-input sample)
(defn most-common-bit
  "Determines the most common bit in a particular position in a sequence of strings of bits."
  [input n]
  (let [m (reduce (fn [acc x]
                    (update acc (Integer/parseInt (str (nth x n))) inc))
                  {0 0, 1 0}
                  input)]
    (if (> (m 0) (m 1))
      0
      1)))

(defn least-common-bit
  "Determines the least common bit in a particular position in a sequence of strings of bits."
  [input n]
  (let [m (reduce (fn [acc x]
                    (update acc (Integer/parseInt (str (nth x n))) inc))
                  {0 0, 1 0}
                  input)]
    (if (<= (m 0) (m 1))
      0
      1)))

(defn solution-one [input]
  (let [gamma (map (partial most-common-bit input) (range (count (first input))))
        epsilon (map (partial least-common-bit input) (range (count (first input))))
        binary-to-int (fn [x] (Integer/parseInt (str/join x) 2))]
    (* (binary-to-int gamma) (binary-to-int epsilon))))
(def answer-one (solution-one puzzle-input))

(defn oxygen-rating [input]
  (loop [xs input
         bit 0]
    (if (= (count xs) 1)
      (Integer/parseInt (first xs) 2)
      (recur
       (filterv
        #(= (most-common-bit xs bit) (Integer/parseInt (str (nth % bit))))
        xs)
       (inc bit)))))

(defn co2-scrubber-rating [input]
  (loop [xs input
         bit 0]
    (if (= (count xs) 1)
      (Integer/parseInt (first xs) 2)
      (recur
       (filterv
        #(= (least-common-bit xs bit) (Integer/parseInt (str (nth % bit))))
        xs)
       (inc bit)))))

(defn solution-two [input]
  (* (oxygen-rating input) (co2-scrubber-rating input)))


(comment
  (oxygen-rating sample)
  (co2-scrubber-rating sample)
  (solution-two puzzle-input)
  ;; => 6677951

  
  (most-common-bit sample 0) 

  (map (partial most-common-bit sample) (range 5))
  ;; => (1 0 1 1 0)
  (Integer/parseInt (str/join '(1 0 1 1 0)) 2)

  (map (partial least-common-bit sample) (range 5))
  ;; => (0 1 0 0 1)
  (Integer/parseInt (str/join '(0 1 0 0 1)) 2)
  )

(def puzzle-input (-> (slurp "inputs/day_3_input.txt")
                      str/split-lines))

(def sample
  (->> "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"
       str/split-lines))