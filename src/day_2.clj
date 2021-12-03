(ns day-2
  (:require [clojure.string :as str]))

(declare puzzle-input)
(def directions #{"forward" "down" "up"})

(defn solution-one [input]
  (let [{x :x y :y}
        (reduce (fn [{:keys [x y]} [dir magnitude-str]]
                  (let [magnitude (Integer/parseInt magnitude-str)]
                    (condp = dir
                      "forward" {:x (+ x magnitude) :y y}
                      "down" {:x x :y (+ y magnitude)}
                      "up" {:x x :y (- y magnitude)})))
                {:x 0 :y 0}
                input)]
    (println x y)
    (* x y)))
(def answer-one (solution-one puzzle-input))

(def answer-two
  (->> (reduce
        (fn [{:keys [aim x y] :as acc} [dir magnitude-str]]
          (let [magnitude (Integer/parseInt magnitude-str)]
            (condp = dir
              "forward" {:aim aim :x (+ x magnitude) :y (+ y (* magnitude aim))}
              "down" (assoc acc :aim (+ aim magnitude))
              "up" (assoc acc :aim (- aim magnitude)))))
        {:aim 0 :x 0 :y 0}
        puzzle-input)
       ((fn [{:keys [x y]}] (* x y)))))

(comment
  (let [puzzle-input (map #(str/split % #"\s") ["forward 5"
                                                "down 5"
                                                "forward 8"
                                                "up 3"
                                                "down 8"
                                                "forward 2"])]
    (reduce
     (fn [{:keys [aim x y] :as acc} [dir magnitude-str]]
       (let [magnitude (Integer/parseInt magnitude-str)]
         (condp = dir
           "forward" {:aim aim :x (+ x magnitude) :y (+ y (* magnitude aim))}
           "down" (assoc acc :aim (+ aim magnitude))
           "up" (assoc acc :aim (- aim magnitude)))))
     {:aim 0 :x 0 :y 0}
     puzzle-input))
  ;; => {:aim 10, :x 15, :y 60}
  (* 15 60)
  )

(def puzzle-input (map #(str/split % #"\s") (str/split-lines (slurp "inputs/day_2_input.txt"))))