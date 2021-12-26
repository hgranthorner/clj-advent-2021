(ns utils
  (:require [clojure.pprint :refer [pprint]]))

(defn inspect
  [x]
  (pprint x)
  x)

(defn select-in-keys
  [])