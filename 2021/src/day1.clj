(ns day1
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :refer [split-lines]]))

(def input
  (map #(Integer/parseInt %)
       (split-lines (slurp (resource "day1-1.txt")))))

;; part-1
(->> input
     (partition 2 1)
     (map (fn [[x y]]
            [y (if (> y x)
                 :inc
                 :dec)]))
     (map second)
     frequencies)

;;part-2 
(->> input
     (partition 3 1)
     (map #(apply + %))
     (partition 2 1)
     (map (fn [[x y]]
            [y (cond
                 (= y x) :=
                 (> y x) :inc
                 :else   :dec)
             ]))
     (map second)
     frequencies)
