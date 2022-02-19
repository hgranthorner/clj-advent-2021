(ns day-11
  (:require [utils :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is run-tests]]
            [com.rpl.specter :as sp]))

(def flashes (atom 0))

(def starting "11111
19991
19191
19991
11111")

(def after-step-1 "34543
40004
50005
40004
34543")

(def after-step-2 "45654
51115
61116
51115
45654")

(defn parse-octopi
  "Returns a map of x/y tuples to octopi."
  [octopi-string]
  (let [len (-> octopi-string str/split-lines first count)]
  (into {}
   (comp
    (remove (partial = \newline))
    (map-indexed
     (fn [i n]
       [[(mod i len) (quot i len)]
        {:value (Integer/parseInt (str n))
         :flashed? false
         :already-flashed? false}])))
   octopi-string)))


(defn encode-octopi
  [octopi]
  (let [all-pos (keys octopi)
        max-x (first (apply (partial max-key first) all-pos))]
    (->> octopi
         (into (sorted-map-by (fn [pos1 pos2]
                                (compare (vec (reverse pos1)) (vec (reverse pos2))))))
         (map (fn [[_ {:keys [value]}]]
                (str value)))
         (partition (inc max-x))
         (map str/join)
         (str/join \newline))))

(defn increment-octopus-value
  [{:keys [value] :as m}]
  (let [new-val (inc value)]
    (assoc m
           :value new-val
           :flashed? (< 9 new-val))))

(defn increment-octopus
  [[pos m]]
  [pos (increment-octopus-value m)])

(defn increment-octopi
  "Increments the value for each octopi. Sets :flashed? to '(< 9 new-value)."
  [octopi]
  (into {}
        (map increment-octopus)
        octopi))

(defn surrounding-positions
  [octopi x y]
  (let [all-pos (keys octopi)
        max-x (first (apply (partial max-key first) all-pos))
        min-x (first (apply (partial min-key first) all-pos))
        max-y (second (apply (partial max-key second) all-pos))
        min-y (second (apply (partial min-key second) all-pos))
        xs (filter #(<= min-x % max-x) (range (dec x) (inc (inc x))))
        ys (filter #(<= min-y % max-y) (range (dec y) (inc (inc y))))]
    (for [x' xs
          y' ys]
      [x' y'])))

(defn update-surrounding
  "Updates the surrounding octopi after a flash."
  [octopi]
  (let [flashed-octopi (filter (fn [[_ {:keys [flashed? already-flashed?]}]]
                                 (and flashed? (not already-flashed?)))
                               octopi)
        _ (swap! flashes (partial + (count flashed-octopi)))]
    (loop [[[x y :as pos] m] (first flashed-octopi)
           os (rest flashed-octopi)
           octopi octopi]
      (if m
        (recur (first os)
               (rest os)
               (-> (reduce (fn [acc pos]
                             (update acc pos increment-octopus-value))
                           octopi
                           (surrounding-positions octopi x y))
                   (update [x y] #(assoc % :already-flashed? true))))
        octopi))))

(comment
  (let [octopi (parse-octopi starting)
        grid1 (increment-octopi octopi)
        grid2 (update-surrounding grid1)]
    (filter
     #(and (:flashed? %) (not (:already-flashed? %)))
     (vals grid2)))
  )

(defn reset-flashed-octopi
  [octopi]
  (into {}
        (map (fn [[pos {:keys [flashed?] :as m}]]
               [pos (if flashed?
                      (assoc m
                             :value 0
                             :flashed? false
                             :already-flashed? false)
                      m)]))
        octopi))

(defn update-octopi
  "Returns a grid of octopi after 1 step."
  [octopi]
  (loop [octopi (-> octopi increment-octopi update-surrounding)]
    (if (not-empty (filter
                    #(and (:flashed? %) (not (:already-flashed? %)))
                    (vals octopi)))
      (recur (update-surrounding octopi))
      (reset-flashed-octopi octopi))))

(defn verify
  [octopi example]
  (let [next (update-octopi octopi)]
    (assert (= example (encode-octopi next))
            (format "Octopis do not match. Example: \n%s\nSupplied:\n%s\n"
                    example
                    (encode-octopi next)))
    next))

(comment
  (def input (slurp "inputs/day_11_input.txt"))
  @flashes

  (loop [octopi (parse-octopi input)
         steps 0]
    (if (= 100 steps)
      @flashes
      (recur (update-octopi octopi) (inc steps))))

  (-> (parse-octopi starting)
      (verify after-step-1)
      (verify after-step-2))

  (= (encode-octopi (parse-octopi ex-starting)) ex-starting)
  (-> (parse-octopi ex-starting)
      (verify ex-step-1)
      (verify ex-step-2)
      (verify ex-step-3))
  )


(def ex-starting "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(def ex-step-1 "6594254334
3856965822
6375667284
7252447257
7468496589
5278635756
3287952832
7993992245
5957959665
6394862637")

(def ex-step-2 "8807476555
5089087054
8597889608
8485769600
8700908800
6600088989
6800005943
0000007456
9000000876
8700006848")

(def ex-step-3 "0050900866
8500800575
9900000039
9700000041
9935080063
7712300000
7911250009
2211130000
0421125000
0021119000")
