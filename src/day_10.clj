(ns day-10
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

(def closing-pairs {\{ \}
                    \( \)
                    \< \>
                    \[ \]})

(def scoring-pairs {\) 3
                    \] 57
                    \} 1197
                    \> 25137})

(def second-scoring-pairs {\( 1
                           \[ 2
                           \{ 3
                           \< 4})

(defn find-first-incorrect-character
  [s]
  (reduce
    (fn [{:keys [current stack] :as acc} char]
      #_(prn char)
      #_(prn acc)
      #_(println "==========")
      (cond
        ; Nothing left on the stack
        (or (nil? current)) {:current char :stack '()}
        ; Current char is an opener
        (get closing-pairs char) {:current char :stack (cons current stack)}
        ; Current char closes the last opener
        (= char (get closing-pairs current)) {:current (first stack) :stack (rest stack)}
        ; If the current char doesn't close the last opener, we're done!
        (not= char (get closing-pairs current)) (do
                                                  #_(prn "DONE!")
                                                  #_(prn acc)
                                                  #_(prn char)
                                                  (reduced char))))
    {:current (first s) :stack '()}
    (rest s)))

(find-first-incorrect-character "[({(<(())[]>[[{[]{<()<>>")

(defn solution-one
  [input]
  (reduce
    +
    (sequence
      (comp
        (map
          find-first-incorrect-character)
        (remove map?)
        (map (partial get scoring-pairs)))
      (str/split-lines input))))
(def answer-one (solution-one (slurp "inputs/day_10_input.txt")))

(defn complete-sequence
  [{:keys [current stack]}]
  (let [new-stack (cons current stack)]
    (reduce
      (fn [acc c]
        (+
          (get second-scoring-pairs c)
          (* acc 5)))
      0
      new-stack)))

(let [nums (->> (str/split-lines (slurp "inputs/day_10_input.txt"))
             (map #(vector % (find-first-incorrect-character %)))
             (remove #(-> % second char?))
             (map second)
             (map complete-sequence)
             (sort >)
             vec)]
  (get nums (int (/ (count nums) 2))))

(int (/ 5 2))

(deftest day-10
  (testing "Solution one"
    (is (= (solution-one (slurp "inputs/day_10_input.txt")) 374061))))

(run-tests)