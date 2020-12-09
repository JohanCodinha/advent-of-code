(ns day-9
  (:require [clojure.string :as st]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combi]))

(def input (slurp (io/resource "input-day-9.txt")))

(def exampl-input "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(defn parse-input [s] (->> (st/split-lines s)
                           (map #(Long/parseLong %))))

(parse-input exampl-input)
(parse-input input)

(defn preamble [index numbers size]
  (->> (take index numbers)
       (take-last size )))

(preamble 5 (parse-input exampl-input) 5)

(defn valid-possible-sum [xs]
  (->> (combi/combinations xs 2)
       (filter #(apply not= %))
       (map #(vector (apply + %) %))
       (into {})))

(def d (atom nil))

(defn part-1 [numbers]
  (some (fn check [ns]
          (let [preamble (butlast ns)
                n (last ns)
                sums (valid-possible-sum preamble)]
            (when (not (sums n)) (reset! d preamble) (prn n sums (sums n)))
            (when (not (sums n)) [n (sums n)])))
        (partition 26 1 numbers)))

(def invalid-num  (first (part-1 (parse-input input))))  ;; => 14360655

(def invalid-num-example  (first (part-1 (parse-input exampl-input))))

(defn part-2 [n invalid-num nums]
  (let [poss-set (into {} (map #(vector (apply + %) %) (partition n 1 nums)))
        weakness (poss-set invalid-num)]
    (if (or weakness (> n (count nums)))
      (+ (apply min weakness) (apply max weakness))
      (part-2 (inc n) invalid-num nums))))

(part-2 2 invalid-num (parse-input input)) 
;; => 1962331

