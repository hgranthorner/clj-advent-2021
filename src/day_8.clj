(ns day-8
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.data :as data]
            [clojure.set :as set]
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
  (let [top (find-top strs)
        four (find-count 4 strs)
        four-with-top (set (str four top))
        diffs (map #(data/diff four-with-top (set %)) strs)]
    (first
     (second
      (find-one #(and (nil? (first %))
                      (= (count (second %)) 1))
                diffs)))))

(defn find-bottom-left
  "Gets a four, then slaps a top and bottom on that guy (making a nine).
   Then we can see what's in 8 but not 9 to find the bottom left char."
  [strs]
  (let [top (find-top strs)
        bottom (find-bottom strs)
        four (find-count 4 strs)
        nine (set (str four top bottom))
        eight (find-count 7 strs)
        diff (data/diff nine (set eight))]
    (first (second diff))))

(defn find-top-right
  "Find the difference between 9 and 5 to find the top right."
  [strs]
  (let [top (find-top strs)
        bottom (find-bottom strs)
        bottom-left (find-bottom-left strs)
        one (set (find-count 2 strs))
        four (find-count 4 strs)
        diffs (map #(data/diff (set (str four top bottom bottom-left))
                               (set %)) strs)]
    (first
     (first
      (find-one #(and (nil? (second %))
                      (= (count (first %)) 1)
                      (get one (first (first %)))) diffs)))))

(defn find-bottom-right
  "Find the difference between the top right and 1."
  [strs]
  (let [one (set (find-count 2 strs))
        top-right (set (str (find-top-right strs)))]
    (-> (data/diff one top-right) first first)))

(defn find-middle
  "Find the difference between the top-tr-bl-b and 2."
  [strs]
  (let [thing (set [(find-top strs)
                    (find-top-right strs)
                    (find-bottom-left strs)
                    (find-bottom strs)])
        diffs (map #(data/diff thing (set %)) strs)]
    (-> (find-one #(and (= (count (second %)) 1)
                        (nil? (first %))) diffs) second first)))

(defn find-top-left
  "Find the remaining character."
  [strs]
  (let [known #{(find-top strs)
                (find-top-right strs)
                (find-bottom-right strs)
                (find-bottom strs)
                (find-bottom-left strs)
                (find-middle strs)}
        all #{\a \b \c \d \e \f \g}]
    (first (set/difference all known))))

(defn wires->segments
  [strs]
  {(find-top strs) :t
   (find-bottom strs) :b
   (find-middle strs) :m
   (find-top-left strs) :tl
   (find-top-right strs) :tr
   (find-bottom-left strs) :bl
   (find-bottom-right strs) :br})

(def segments->numbers
  {#{:tr :br} 1
   #{:t :tr :m :bl :b} 2
   #{:t :tr :m :br :b} 3
   #{:tl :tr :m :br} 4
   #{:t :tl :m :br :b} 5
   #{:t :tl :m :bl :br :b} 6
   #{:t :tr :br} 7
   #{:t :m :b :tl :tr :bl :br} 8
   #{:t :tl :tr :m :br :b} 9
   #{:t :tr :br :b :bl :tl} 0})

(defn solution-two
  [input]
  (reduce +
          (let [x (map (fn [x]
                         (let [m (wires->segments (str/split (:pattern x) #" "))
                               o (str/split (:output x) #" ")
                               segments (s/transform [s/ALL s/ALL] #(get m %) o)]
                           (->> segments
                                (sequence
                                 (comp (map set)
                                       (map #(segments->numbers %))
                                       (map str)))
                                (str/join "")
                                (Integer/parseInt))))
                       (parse-input input))]
            (pprint x)
            x)))
(solution-two "inputs/day_8_input.txt")
(comment
   (def sample-pattern (:pattern (first (parse-input "inputs/day_8_sample_2.txt"))))

   (as-> (str/split sample-pattern #" ") x
     (wires->segments x)
     (pprint x))
   (def sample (first (parse-input "inputs/day_8_sample_2.txt")))

   #{:t :tr :tl :m :br :bl :b}
   "Values: 1, 4, 7, 8"
   "Counts: 2, 4, 3, 7"
   (run-tests))

 (deftest day-8
   (testing "Solution One"
     (is (= (solution-one "inputs/day_8_input.txt") 409))))
