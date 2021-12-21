(ns day-6
  (:require [clojure.string :as str]
            [clojure.pprint :refer [print-table pprint]]
            [com.rpl.specter :as s]
            [criterium.core :as criterium]))

(def no-fish {0 0, 1 0, 2 0, 3 0, 4 0, 5 0, 6 0, 7 0, 8 0})

(defn progress-day
  [fishes]
  (reduce
   (fn [acc [day amt]]
     (if (= day 0)
       (-> acc
           (update 6 (partial + amt))
           (update 8 (partial + amt)))
       (update acc (dec day) (partial + amt))))
   no-fish
   fishes))

(let [input-strs (-> (slurp "inputs/day_6_input.txt")
                     (str/split #","))
      input-nums (map #(Integer/parseInt %) input-strs)
      starting-fish (reduce (fn [acc n] (update acc n inc))
                            no-fish
                            input-nums)]
  (reduce + (vals (reduce (fn [acc _] (progress-day acc)) starting-fish (range 256)))))