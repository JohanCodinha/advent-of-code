(ns day-11
  (:require [clojure.string :as st]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combi]))

(def demo-input (slurp (io/resource "input-day-11-demo.txt")))
(def input (slurp (io/resource "input-day-11.txt")))

(defn parse-input [s]
  (->> s
       (map {\L :e \. :f})
       (partition-by nil?)
       (take-nth 2)))

(defn vincinity [pos]
  (let [[x y] pos
        xs (range (dec x) (inc (inc x)))
        ys (range (dec y) (inc (inc y)))]
    (remove #{pos} (combi/cartesian-product xs ys))))

(def memo-vincinity (memoize vincinity))


(defn step [layout]
  (time 
   (reduce (fn [acc pos]
             (let [seat (layout pos)
                   vincinity' (keep layout (memo-vincinity pos))]
               (cond
                 (and (= :e seat) (not (some #(= :o %) vincinity')))
                 (assoc acc pos :o)
                 (and (= :o seat) (<= 4 (count (filter #(= :o %) vincinity'))))
                 (assoc acc pos :e)
                 :else
                 acc
                 ))) layout (keys layout))))
"
    If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
    If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
    Otherwise, the seat's state does not change.
"
(let [parsed-input (parse-input input) 
           map-layout (->> (map-indexed
                            (fn [x rows]
                              (map-indexed (fn [y v] [[x y] v]) rows))
                            parsed-input)
                           (apply concat)
                           (filter #(not= :f (second %)))
                           (into {}))]
       (->> (iterate step map-layout)
            (partition 2 2)
            (some (fn [[a b]] (when (= a b) b)))
            (map second)
            (frequencies)
            :o))

(defn neg-range [start]
  (iterate dec start))

(defn pos-range [start]
  (iterate inc start))

(defn vincinity-pt2 [layout pos]
  (let [[x y] pos
        right (map vector (pos-range (inc x)) (repeat y))
        left (map vector (neg-range (dec x)) (repeat y))
        top (map vector (repeat x) (pos-range (inc y)))
        down (map vector (repeat x) (neg-range (dec y)))
        top-left (map vector (neg-range (dec x)) (pos-range (inc y)))
        top-right (map vector (pos-range (inc x)) (pos-range (inc y)))
        bottom-left (map vector (neg-range (dec x)) (neg-range (dec y)))
        bottom-right (map vector (pos-range (inc x)) (neg-range (dec y)))]
    (reduce (fn [acc item]
              (conj acc (some {nil :out
                               :e :e
                               :o :o}
                              (map layout item))))
            [] [right left top down top-right top-left bottom-right bottom-left])))

(defn step-pt2 [layout]
  (reduce (fn [acc pos]
            (let [seat (layout pos)
                  vincinity' (vincinity-pt2 layout pos)]
              (cond
                (and (= :e seat) (not (some #(= :o %) vincinity')))
                (assoc acc pos :o)
                (and (= :o seat) (<= 5 (count (filter #(= :o %) vincinity'))))
                (assoc acc pos :e)
                :else
                acc
                ))) layout (keys layout)))
;; part-2
(time (let [parsed-input (parse-input input) 
            map-layout (->> (map-indexed
                             (fn [x rows]
                               (map-indexed (fn [y v] [[x y] v]) rows))
                             parsed-input)
                            (apply concat)
                            (into {}))]

        (->> (iterate step-pt2 map-layout)
             (partition 2 2)
             (some (fn [[a b]] (when (= a b) b)))
             (map second)
             (frequencies)
             :o)))
