(ns day-8
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.data :as data]
            [com.rpl.specter :as s]
            [clojure.test :refer [run-tests testing is deftest]]))

(defn parse-input
  [input]
  (->> (slurp input)
       str/split-lines
       (map #(str/split % #" \| "))
       (map (fn [[pattern output]]
              {:pattern pattern :output output}))))

(comment
  (parse-input "inputs/day_8_sample.txt")
  )

(defn unique-length?
  "Checks if an output pattern is a unique length."
  [s]
  (let [x #(= (count s) %)]
    (or (x 2)
        (x 3)
        (x 4)
        (x 7))))

(defn solution-one
  [input]
  (->> (parse-input input)
       (map #(str/split (:output %) #" "))
       (s/select [s/ALL s/ALL unique-length?])
       count))

(defn find-one
  [pred coll]
  (first (filter pred coll)))

(defn find-count
  [num coll]
  (first (filter #(= (count %) num) coll)))

(defn find-top
  [strs]
  (let [f #(find-count % strs)
        one   (set (f 2))
        seven (set (f 3))]
    (first (second (data/diff one seven)))))

(defn find-bottom
  [strs]
  (let [f #(find-count % strs)
        top (find-top strs)
        four (f 4)
        four-with-top (set (str four top))
        diffs (map #(data/diff four-with-top (set %)) strs)]
    (first (second
            (find-one #(and (nil? (first %))
                            (= (count (second %)) 1)) diffs)))))

(comment
   (def sample-pattern (:pattern (first (parse-input "inputs/day_8_sample.txt"))))
   (as-> (str/split sample-pattern #" ") x
     (find-bottom x))
   #{:t :tr :tl :m :br :bl :b}
   "Values: 1, 4, 7, 8"
   "Counts: 2, 4, 3, 7"
   (run-tests)
   )

 (deftest day-8
   (testing "Solution One"
     (is (= (solution-one "inputs/day_8_input.txt") 409))))
