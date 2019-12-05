(ns day3
  (:require [clojure.test :refer [is run-tests deftest]]))

(def input (->> (slurp "resources/day3.txt")
               clojure.string/split-lines
               (map #(clojure.string/split % #","))))

(def test-input0
  [["R8","U5","L5","D3"] 
   ["U7","R6","D4","L4"]])

(def test-input
  [["R8","U5"] 
   ["U7","R6"]])

(def test-input1
  [["R75","D30","R83","U83","L12","D49","R71","U7","L72"] 
   ["U62","R66","U55","R34","D71","R55","D58","R83"]])

(def test-input2
  [["R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"] 
  ["U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"]])

(defn manhattan-distance [[ax ay] [bx by]]
  (+
   (Math/abs (- ax bx))
   (Math/abs (- ay by))))

(defn draw [wire]
  (reduce
    (fn [acc wire-section-edge]
      (let [[last-x last-y] (last acc)
            dir (first wire-section-edge)
            dist (->> (rest wire-section-edge)
                      (apply str)
                      Integer/parseInt)
            section
            (case dir
              \U (map #(vector last-x %) (range (inc last-y) (+ last-y (inc dist))))
              \D (map #(vector last-x %) (reverse (range (- last-y dist) last-y)))
              \R (map #(vector % last-y) (range (inc last-x) (+ last-x (inc dist))))
              \L (map #(vector % last-y) (reverse (range (- last-x dist) last-x))))]
        (apply conj acc section)))
    [[0 0]] wire))

(deftest draw-test
  (is (= [[0 0] [0 1] [0 2] [0 3] [0 4] [0 5]] (draw ["U5"])))
  (is (= [[0 0] [0 -1] [0 -2] [0 -3] [0 -4] [0 -5]] (draw ["D5"])))
  (is (= [[0 0] [-1 0] [-2 0] [-3 0] [-4 0] [-5 0]] (draw ["L5"])))
  (is (= [[0 0] [1 0] [2 0] [3 0] [4 0] [5 0]] (draw ["R5"]))))

(defn crosses [wires]
  (let [first-wire (into #{} (draw (first wires)))]
  (rest
    (keep first-wire (draw (second wires))))))

(defn part1-distance [crosses]
  (map (fn [o c]
         {:distance (manhattan-distance o c)
          :cross c})
       (repeat [0 0])
       crosses))
 
(defn part1 [input]
  (->> (crosses input)
       part1-distance
       (sort-by :distance)
       first
       :distance))

(deftest part1-examples
  (is (= 159 (part1 test-input1)))
  (is  (= 135 (part1 test-input2)))
  (is  (= 1195 (part1 input))))

(defn calc-dis [inter wire]
  (let [indexed-wire
        (->> (subvec wire 0 (inc (.indexOf wire inter)))
             (map-indexed #(vector % %2)))
        steps
        (reduce
          (fn reducer [acc [i section]]
            (if-let [visited (acc section)]
              acc
              (merge acc {section i})))
          {} 
          indexed-wire)]
    (steps inter)))

(defn part2 [input]
  (let [[wire1 wire2] (map draw input)
        intersections
        (remove #{[0 0]}
                (clojure.set/intersection (set wire1) (set wire2)))]
    (->> (map (fn [intercetion]
                ( + (calc-dis intercetion wire1)
                    (calc-dis intercetion wire2))) intersections)
         (apply min))))

(deftest part2-tests
  (is (= 610 (part2 test-input1)))
  (is (= 410 (part2 test-input2)))
  (is (= 91518 (part2 input))))
