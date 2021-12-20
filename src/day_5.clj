(ns day-5
  (:require [clojure.string :as str]
            [clojure.pprint :refer [print-table pprint]]
            [com.rpl.specter :as s]))

(defn parse-input
  "Parses the input string into maps."
  [input]
  (->> (slurp input)
       str/split-lines
       (sequence
        (comp
         (map #(re-matches #"^(\d+),(\d+) -> (\d+),(\d+)" %))
         (map #(hash-map :x1 (Integer/parseInt (get % 1))
                         :y1 (Integer/parseInt (get % 2))
                         :x2 (Integer/parseInt (get % 3))
                         :y2 (Integer/parseInt (get % 4))))))))

(defn build-diagram
  "Takes a series of coordinates and builds a blank diagram."
  [coordinates]
  (let [width (max (:x1 (apply max-key :x1 coordinates))
                   (:x2 (apply max-key :x2 coordinates)))
        height (max (:y1 (apply max-key :y1 coordinates))
                    (:y2 (apply max-key :y2 coordinates)))]
    {:coordinates coordinates
     :width width
     :height height
     :diagram (repeat (* (inc height) (inc height)) 0)}))

(defn inclusive-range
   [min max]
   (if (= min max)
     (list min)
     (range min (inc max))))
 
(defn coordinate-cross-product
   [{:keys [x1 x2 y1 y2]}]
   (for [x (inclusive-range (min x1 x2)
                            (max x1 x2))
         y (inclusive-range (min y1 y2)
                            (max y1 y2))]
     [x y]))


(let [xs [1 2 3]
      ys [0 0 0 0 0 0 0]]
  (map-indexed #(if ((set xs) %1) (inc %2) %2) ys))

 (defn mark-diagram
    "Returns a new diagram.
      Marks the diagram with the coordinates, and the space between the coordinates."
    [diagram width coordinate]
    (let [cp (coordinate-cross-product coordinate)
          points (->> cp (map (fn [[x y]] (+ x (* y (inc width))))) set)]
      (map-indexed #(if (points %1) (inc %2) %2) diagram)))

(defn solution-one
  [input]
  (let [diagram (->> (parse-input input)
                     (filter #(or (= (:y1 %) (:y2 %))
                                  (= (:x1 %) (:x2 %))))
                     build-diagram)]
    (loop [coordinate (first (:coordinates diagram))
           diagram (update diagram :coordinates rest)]
      (if coordinate
        (recur (first (:coordinates diagram))
               (-> diagram
                   (update :coordinates rest)
                   (update :diagram mark-diagram (:width diagram) coordinate)))
        (count (filter #(< 1 %) (:diagram diagram)))))))

   (comment
     (solution-one "inputs/day_5_sample.txt")
     (solution-one "inputs/day_5_input.txt")
     (def diagram (->> (parse-input "inputs/day_5_sample.txt")
                       (filter #(or (= (:y1 %) (:y2 %))
                                    (= (:x1 %) (:x2 %))))
                       build-diagram))

     (def coordinate {:y1 4, :x1 3, :y2 4, :x2 1})

     (coordinate-cross-product coordinate)


     (re-matches #"^(\d+),(\d+) -> (\d+),(\d+)" "5,5 -> 8,2")
     )