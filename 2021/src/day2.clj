(ns day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            ))

(def input (slurp (io/resource "day2.txt")))
(def demo-input
  "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(->> (str/split-lines input)
     (map (fn [s] (let [[dir unit] (str/split s #" ")]
                    [(keyword dir) (Integer/parseInt unit)])))
     (reduce (fn [acc [dir unit]]
               (case dir
                 :forward (update acc :horizontal + unit)
                 :down (update acc :depth + unit)
                 :up (update acc :depth - unit)))
             {:horizontal 0
              :depth 0})
     vals
     (apply *))
;; 1804520

(->> (str/split-lines input)
     (map (fn [s] (let [[dir unit] (str/split s #" ")]
                    [(keyword dir) (Integer/parseInt unit)])))
     (reduce (fn [acc [dir unit]]
               (case dir
                 :forward (-> acc
                              (update :horizontal + unit)
                              (update :depth + (* (:aim acc) unit)))
                 :down (-> acc
                           (update :aim + unit))
                 :up (-> acc
                         (update :aim - unit))))
             {:horizontal 0
              :depth 0
              :aim 0})
     (filter #(#{:horizontal :depth} (first %)))
     (map second)
     (apply *))

;; 1971095320
