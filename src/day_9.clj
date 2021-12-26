(ns day-9
  (:require [clojure.repl :refer [doc]]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is run-tests]]
            [malli.core :as m]
            [malli.error :as me]
            [malli.registry :as mr]
            [malli.util :as mu]
            [malli.instrument :as mi]
            [com.rpl.specter :as sp]))

(mr/set-default-registry!
  (merge
    (m/default-schemas)
    (mu/schemas)
    {::x        [:int {:min 0}]
     ::y        [:int {:min 0}]
     ::height   [:int {:min 0 :max 9}]
     ::visited? :boolean}))

(def Cell
  [:map ::x ::y ::height [::visited? {:optional true}]])

(def HeightMap
  [:map-of
   [:int {:min 0}]
   [:map-of
    [:int {:min 0}]
    Cell]])

(m/validate HeightMap {0 {1 {::x 1 ::y 2 ::height 4}}})

(defn parse-input
  "Parses input into a height map."
  [input]
  (->> input
    str/split-lines
    (mapv #(str/split % #""))
    (sp/transform [sp/ALL sp/ALL] #(Integer/parseInt %))
    (sp/transform [sp/ALL] #(map-indexed (fn [x y] (sorted-map x {::height y ::x x})) %))
    (map #(into (sorted-map) %))
    (map-indexed
      (fn [x y]
        [x (sp/transform [sp/MAP-VALS] #(assoc % ::y x) y)]))
    (into (sorted-map))))
(m/=> parse-input [:=> [:cat :string] HeightMap])


(comment
  (mi/instrument!)
  (parse-input (slurp "inputs/day_9_sample.txt"))
  )

(defn lower?
  "Checks if the height at (x1, y1) is lower than the height at (x2, y2)"
  [x1 y1 x2 y2 height-map]
  (if-let [above (get-in height-map [y2 x2 ::height])]
    (< (get-in height-map [y1 x1 ::height]) above)
    true))

(defn lowest-point?
  "Checks if the point is a local minimum height."
  [{::keys [x y]} height-map]
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
  (let [height-map    (parse-input input)
        lowest-points (sp/select
                        [sp/MAP-VALS (sp/filterer #(lowest-point? (second %) height-map))]
                        height-map)]
    (reduce
      #(+ %1 (inc %2))
      0
      (sp/select [(sp/filterer not-empty) sp/ALL sp/ALL (sp/filterer map?) sp/ALL ::height] lowest-points))))
(m/=> solution-one [:=> [:cat :string] :int])
(def answer-one (solution-one (slurp "inputs/day_9_input.txt")))

(defn update-get-in
  "Update and then get the same path."
  [m ks f]
  (-> (update-in m ks f)
    (get-in ks)))

(defn get-surrounding
  "Returns a map of the surrounding cells."
  [height-map {::keys [x y]}]
  (let [up    (get-in height-map [(dec y) x])
        down  (get-in height-map [(inc y) x])
        left  (get-in height-map [y (dec x)])
        right (get-in height-map [y (inc x)])]
    {:up up :down down :left left :right right}))

(defn not-boundary?
  "Checks if a cell is boundary of a basin."
  [{::keys [height visited?]}]
  (and (not= height 9) (not visited?)))
(m/=> not-boundary? [:=> [:cat
                          [:map ::height [::visited? {:optional true}]]]
                     :boolean])

(not-boundary? {::height 3})

(defn get-group
  "Returns a marked up height map and the group that the cell falls into."
  [height-map cell]
  (loop [height-map     height-map
         group          (if (not-boundary? cell) #{cell} #{})
         cells-to-check [cell]]
    (if-let [{::keys [x y] :as cell} (first cells-to-check)]
      (if (not-boundary? cell)
        (let [new-height-map (assoc-in height-map [y x ::visited?] true)
              {:keys [up down left right]} (get-surrounding new-height-map cell)
              rest-cells     (sp/setval [sp/FIRST] sp/NONE cells-to-check)]
          (recur new-height-map
            (conj group cell)
            (apply conj rest-cells (remove nil? [up down left right]))))
        (recur height-map group (rest cells-to-check)))
      {:height-map height-map
       :group      group})))
(m/=> get-group [:=> [:cat HeightMap Cell] [:map
                                            [:height-map HeightMap]
                                            [:group [:set Cell]]]])

(defn solution-two
  [input]
  (let [height-map  (parse-input (slurp input))
        valid-cells (map (partial into {})
                      (sp/select [sp/MAP-VALS (sp/filterer #(not-boundary? (second %))) sp/ALL sp/ALL map?]
                        height-map))
        groups      (reduce (fn [{:keys [height-map groups] :as acc} cell]
                              (let [{new-map :height-map new-group :group} (get-group height-map cell)]
                                {:height-map new-map :groups (conj groups new-group)}))
                      {:height-map height-map, :groups []}
                      valid-cells)]
    (apply * (take 3 (reverse (sort (map count (:groups groups))))))))
(def answer-two (solution-two (slurp "inputs/day_9_input.txt")))



(comment
  (mi/instrument!)
  (mi/unstrument!)

  (def height-map (parse-input (slurp "inputs/day_9_sample.txt")))
  (def cell (get-in height-map [0 0]))
  (get-group height-map cell)
  (def valid-cells
    (map (partial into {})
      (sp/select [sp/MAP-VALS (sp/filterer #(not-boundary? (second %))) sp/ALL sp/ALL map?]
        height-map)))

  (reverse (sort (map count (:groups groups))))
  (def groups
    (reduce (fn [{:keys [height-map groups] :as acc} cell]
              (let [{new-map :height-map new-group :group} (get-group height-map cell)]
                {:height-map new-map :groups (conj groups new-group)}))
      {:height-map height-map, :groups []}
      valid-cells))

  (reduce #(apply + (map ::height %)) groups)

  (get-group height-map cell)
  (apply conj [1 2 3] '(4 5 6))
  (sp/select [(sp/filterer identity) sp/ALL] [1 2 nil 3 nil])
  (update-get-in height-map (::y cell))
  (let [height-not-nine? (fn [{::keys [height]}]
                           (not= height 9))
        not-visited?     (fn [{::keys [visited?]}]
                           (not visited?))
        pred             #(and (height-not-nine? %) (not-visited? %))]
    (create-group height-map cell pred))
  )

(deftest day-9
  (testing "Solution One"
    (is (= (solution-one (slurp "inputs/day_9_input.txt")) 462)))
  (testing "Solution Two"
    (is (= (solution-two (slurp "inputs/day_9_input.txt")) 1397760))))

(run-tests)