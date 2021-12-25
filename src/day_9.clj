(ns day-9
  (:require [clojure.repl :refer [doc]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is run-tests]]
            [com.rpl.specter :as s]))

;(def p (p/open))
;(add-tap #'p/submit)
;(defn tap
;  [x]
;  (tap> x)
;  x)

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
(comment
  (tap
    (parse-input (slurp "inputs/day_9_sample.txt")))
  )

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

(defn assoc-get-in
  "Assoc and then get the same path."
  [m ks v]
  (-> (assoc-in m ks v)
      (get-in ks)))

(defn update-get-in
  "Update and then get the same path."
  [m ks f]
  (-> (update-in m ks f)
      (get-in ks)))

(defn get-surrounding
  "Returns a map of the surrounding cells."
  [height-map {:keys [x y]}]
  (let [up (get-in height-map [(dec y) x])
        down (get-in height-map [(inc y) x])
        left (get-in height-map [y (dec x)])
        right (get-in height-map [y (inc x)])]
    {:up up :down down :left left :right right}))

(defn not-boundary?
  "Checks if a cell is boundary of a basin."
  [{:keys [height visited?]}]
  (and (not= height 9) (not visited?)))

(defn get-group
  "Returns a marked up height map and the group that the cell falls into."
  [height-map cell]
  (loop [height-map height-map
         group (if (not-boundary? cell) #{cell} #{})
         cells-to-check [cell]]
    (if-let [{:keys [x y] :as cell} (first cells-to-check)]
      (if (not-boundary? cell)
        (let [new-height-map (assoc-in height-map [y x :visited?] true)
              {:keys [up down left right]} (get-surrounding new-height-map cell)
              rest-cells (s/setval [s/FIRST] s/NONE cells-to-check)]
          (recur new-height-map
                 (conj group cell)
                 (apply conj rest-cells (remove nil? [up down left right]))))
        (recur height-map group (rest cells-to-check)))
      {:height-map height-map
       :group      group})))

(comment
  (def height-map (parse-input (slurp "inputs/day_9_sample.txt")))
  (def cell (get-in height-map [0 0]))

  (get-group height-map cell)
  (apply conj [1 2 3] '(4 5 6))
  (s/select [(s/filterer identity) s/ALL] [1 2 nil 3 nil])
  (update-get-in height-map (:y cell))
  (let [height-not-nine? (fn [{:keys [height]}]
                           (not= height 9))
        not-visited? (fn [{:keys [visited?]}]
                       (not visited?))
        pred #(and (height-not-nine? %) (not-visited? %))]
    (create-group height-map cell pred))
  )

(deftest day-9
  (testing "Solution One"
    (is (= (solution-one (slurp "inputs/day_9_input.txt")) 462))))

(run-tests)