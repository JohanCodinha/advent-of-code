(ns day-6
  (:require [clojure.string :as st]))

(def example-input
  "abc

a
b
c

ab
ac

a
a
a
a

b")

(def input (-> (slurp "resources/input-day-6.txt")
               (st/split #"\R\R")))

(defn process-part-1 [group-answers]
  (let [frequencies (->> group-answers
                         (map frequencies)
                         (apply (partial merge-with +)))
        unique (keys frequencies)]
    (count unique)))

(defn process-part-2 [group-answers]
  (let [person (count group-answers)
        frequencies (->> group-answers
                         (map frequencies)
                         (apply (partial merge-with +)))]
    (count (filter (fn [[_ v]] (= person v)) frequencies))))

(->> input
     (map st/split-lines)
     (map process-part-1)
     (apply +))

(->> input
     (map st/split-lines)
     (map process-part-2)
     (apply +))

(process-part-2 ["ymw" "w" "wm" "vsw" "wm"])
