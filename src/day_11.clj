(ns day-11
  (:require [utils :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is run-tests]]
            [malli.core :as m]
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
    (assoc o ::flashed? true
             ::just-flashed? true)
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

(defn determine-octopi-to-increase
  [grid flashed-octopi]
  (into #{}
        (comp
          (mapcat (partial adjacent-octopi grid))
          (filter #(not (::flashed? %)))
          (map #(select-keys % [::x ::y])))
        flashed-octopi))

(defn increase-surrounding-energy
  "Find the flashed octopi and increase the surrounding octopi's brightness."
  [grid]
  (let [grid grid
        just-flashed (sp/select [sp/MAP-VALS sp/MAP-VALS #(::just-flashed? %)] grid)
        ; Only age the octopi that haven't flashed yet
        to-increase (determine-octopi-to-increase grid just-flashed)]
    (sp/transform
      [sp/MAP-VALS sp/MAP-VALS #(to-increase (select-keys % [::x ::y]))]
      grow-older
      grid)))

(def grid (parse-input (slurp "inputs/day_11_sample.txt")))
(def mini-grid (parse-input "11111\n19991\n19191\n19991\n11111"))
(increase-surrounding-energy
  (-> grid pass-time pass-time))

(-> mini-grid
    pass-time
    increase-surrounding-energy)

(adjacent-octopi grid {::x 1 ::y 1})

; Two phases
; 1 - time passes. octopi brightness all goes up. If anyone rolls over they're marked as "flashed" to prevent from flashing again,
;     and "just-flashed" to indicate that the surrounding octopi need to grow-older.
; 2 - find every octopus that just-flashed and increase the adjacent octopus's brightness. Remove their just-flashed flag.
;   - This may cause other octopi to flash - continue to find the "just-flashed" octopi until there are none left
; 3 - Set all "flashed" octopi to 0 brightness and remove their flashed and just-flashed statuses.
