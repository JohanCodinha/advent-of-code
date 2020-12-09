(ns day-8
  (:require [clojure.java.io :as io]
            [clojure.string :as st]))

(def input (slurp (io/resource "input-day-8.txt")))

(def example-input
  "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(defn parse-line [s]
  (let [[_ op arg ] (re-find #"(\w{3}) (.*)" s)]
    [op (Integer/parseInt arg)]))

(def instructions (mapv parse-line (st/split-lines input)))

(defn execute [instructions] 
  (loop [pos [0]
         acc 0]
    (let [current-pos (last pos)
          [op arg] (nth instructions current-pos nil)
          infinite-loop (not (apply distinct? pos))
          terminated (>= current-pos (count instructions))]
      
      (if (and (not infinite-loop) (not terminated))
        (case op
          "acc" (recur (conj pos (inc current-pos))
                       (+ acc arg))
          "jmp" (recur (conj pos (+ current-pos arg))
                       acc)
          "nop" (recur (conj pos (inc current-pos))
                       acc))
        [(when terminated :terminated) acc]))))


(def all-possible-program
  (->> instructions
       (map-indexed
        (fn [index inst]
          (when (#{"jmp" "nop"} (first inst))
            (update-in instructions [index 0] {"jmp" "nop" "nop" "jmp"}))))
       (remove nil?)))

;part-2
(some #(and (= :terminated (first %)) %) (map execute all-possible-program))
