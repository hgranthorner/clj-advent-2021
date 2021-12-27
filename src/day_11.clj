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

(comment
  (inc-round 2)
  (inc-round 9)
  )

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

(defn mark-flashed
  "Marks an octopus as 'flashed' if its brightness is 0."
  [{::keys [brightness] :as o}]
  (if (zero? brightness)
    (assoc o ::flashed? true)
    o))

(defn grow-older
  [o]
  (-> o
      (update ::brightness inc-round)
      mark-flashed))

(defn pass-time
  "Increase all octopi's brightness and have them flash."
  [grid]
  (sp/transform [sp/MAP-VALS sp/MAP-VALS] grow-older grid))

(defn increase-surrounding-energy
  "Find the flashed octopi and increase the surrounding octopi's brightness."
  [grid]
  (let [flashed (sp/select [sp/MAP-VALS sp/MAP-VALS #(::flashed? %)] grid)
        to-increase (into #{}
                      (comp
                        (mapcat (partial adjacent-octopi grid))
                        (filter #(not (::flashed? %)))
                        (map #(select-keys % [::x ::y])))
                      flashed)]
    (sp/transform
      [sp/MAP-VALS sp/MAP-VALS #(to-increase (select-keys % [::x ::y]))]
      grow-older
      grid)))

(def grid (parse-input (slurp "inputs/day_11_sample.txt")))

(increase-surrounding-energy
  (-> grid pass-time pass-time))

(adjacent-octopi grid {::x 1 ::y 1})

; Two phases
; 1 - time passes. octopi brightness all goes up. If anyone rolls over they're marked as "flashed"
; 2 - find every octopus that flashed and increase the adjacent octopus's brightness. Remove their flashed flag.
;   - This step can be continuous, as one octopus flashing may make another octopus flash.
