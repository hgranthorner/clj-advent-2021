(ns day-9
  (:require [clojure.string :as str]
            [com.rpl.specter :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer [deftest testing is run-tests]]
            [portal.api :as p]))

(def p (p/open {:launcher :vs-code}))
(add-tap #'p/submit)
(defn tap
  [x]
  (tap> x)
  x)

(defn inspect
  [x]
  (pprint x)
  x)

(defn parse-input
  [input]
  (->> input
       str/split-lines
       (mapv #(str/split % #""))
       (s/transform [s/ALL s/ALL] #(Integer/parseInt %))
       (s/transform [s/ALL] #(map-indexed (fn [x y] (sorted-map x {:height y :x x})) %))
       (map #(into (sorted-map) %))
       (map-indexed
        (fn [x y]
          [x (s/transform [s/MAP-VALS] #(assoc % :y x) y)]))
       (into (sorted-map))))

(pprint
 (parse-input "inputs/day_9_sample.txt"))

(defn lower?
  "Checks if the height at (x1, y1) is lower than the height at (x2, y2)"
  [x1 y1 x2 y2 height-map]
  (if-let [above (get-in height-map [y2 x2 :height])]
    (< (get-in height-map [y1 x1 :height]) above)
    true))

(defn lowest-point?
  "Checks if the point is a local minimum height."
  [{:keys [x y]} height-map]
  (and
   ; above
   (lower? x y x (dec y) height-map)
   ; right
   (lower? x y (inc x) y height-map)
   ; below
   (lower? x y x (inc y) height-map)
   ; left
   (lower? x y (dec x) y height-map)))

(defn solution-one
  [input]
  (let [height-map (parse-input input)
        lowest-points (s/select
                       [s/MAP-VALS (s/filterer #(lowest-point? (second %) height-map))]
                       height-map)]
    (reduce
     #(+ %1 (inc %2))
     0
     (s/select [(s/filterer not-empty) s/ALL s/ALL (s/filterer map?) s/ALL :height] lowest-points))))
(def answer-one (solution-one (slurp "inputs/day_9_input.txt")))



(deftest day-9
  (testing "Solution One"
    (is (= (solution-one (slurp "inputs/day_9_input.txt")) 462))))