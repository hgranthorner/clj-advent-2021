(ns day-11
  (:require [utils :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is run-tests]]
            [malli.core :as m]
            [malli.error :as me]
            [malli.registry :as mr]
            [malli.util :as mu]
            [malli.instrument :as mi]
            [com.rpl.specter :as sp]))

(str/split-lines (slurp "inputs/day_11_sample.txt"))

(defn parse-input
  "Parses input into a height map."
  [input]
  (->> input
    str/split-lines
    (mapv #(str/split % #""))
    (sp/transform [sp/ALL sp/ALL] #(Integer/parseInt %))
    (sp/transform [sp/ALL]
      #(map-indexed
         (fn [x y] (sorted-map x {::brightness y ::x x ::flashed? false})) %))
    (map #(into (sorted-map) %))
    (map-indexed
      (fn [x y]
        [x (sp/transform [sp/MAP-VALS] #(assoc % ::y x) y)]))
    (into (sorted-map))))

(defn inc-round
  "`inc` input - if the number is gt 9 then round down to 10."
  [x]
  (let [x (inc x)]
    (if (< 9 x) 0 x)))

(def adjacent-coordinates
  (remove #(= (first %) (second %) 0)
    (for [x (range -1 2)
          y (range -1 2)]
      [x y])))

(defn adjacent-octopi
  [grid {::keys [x y]}]
  (remove nil?
    (for [[xd yd] adjacent-coordinates]
      (get-in grid [(+ yd y) (+ xd x)]))))

(def grid (parse-input (slurp "inputs/day_11_sample.txt")))

(adjacent-octopi grid {::x 1 ::y 1})
