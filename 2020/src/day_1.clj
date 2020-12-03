(ns day-1
  (:require [clojure.test :refer [is deftest]]
            [clojure.math.combinatorics :as combi]))

(def input-1 (->> (slurp "resources/input-day-1.txt")
                  clojure.string/split-lines
                  (map #(Integer/parseInt %))))

(defn find-entries-summing-to [input sum entries]
  (let [perm (combi/permuted-combinations input entries)]
    (apply *
           (some (fn [ints] (when (= sum (apply + ints))
                              ints)) perm))))
(deftest part-1-example
  (let [input [1721 979 366 299 675 1456]]
    (is (= 514579 (find-entries-summing-to input 2020 2)))))

(deftest part-2-example
  (let [input [1721 979 366 299 675 1456]]
    (is (= 241861950 (find-entries-summing-to input 2020 3)))))
