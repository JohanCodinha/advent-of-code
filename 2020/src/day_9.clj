(ns day-9
  (:require [clojure.string :as st]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combi]))

(def input (slurp (io/resource "input-day-9.txt")))

(defn parse-input [s] (->> (st/split-lines s)
                           (map #(Long/parseLong %))))

(defn preamble [index numbers size]
  (->> (take index numbers)
       (take-last size )))

(defn valid-possible-sum [xs]
  (->> (combi/combinations xs 2)
       (filter #(apply not= %))
       (map #(vector (apply + %) %))
       (into {})))

(defn part-1 [numbers]
  (some (fn check [ns]
          (let [preamble (butlast ns)
                n (last ns)
                sums (valid-possible-sum preamble)]
            (when (not (sums n)) [n (sums n)])))
        (partition 26 1 numbers)))

(def invalid-num  (first (part-1 (parse-input input))))
;;Part-1
;; => 14360655

(defn part-2 [window invalid-num nums]
  (let [poss-set (->> (partition window 1 nums)
                      (map #(vector (apply + %) %) )
                      (into {}))
        weakness (poss-set invalid-num)]
    (if weakness
      (+ (apply min weakness) (apply max weakness))
      (part-2 (inc window) invalid-num nums))))

(part-2 2 invalid-num (parse-input input))
;; => 1962331

