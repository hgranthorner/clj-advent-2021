(ns day_12
  (:require [clojure.string :as str]))

(def sample "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(def decode-line-regex #"(\w+)-(\w+)")

(re-find decode-line-regex "start-A")

(defn read-input
  [s]
  (->> (str/split-lines sample)
       (map (fn [s]
              (let [[_ start end] (re-find decode-line-regex s)]
                [start end])))
       (group-by first)
       (into {}
             (map
              (fn [[k vs]]
                [k (map second vs)])))))


(comment
  (read-input sample)
  )
