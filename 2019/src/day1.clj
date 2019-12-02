(ns day1
  (:require [clojure.test :refer [is deftest run-tests]]))



; part 1 examples:
; 
; For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2.
; For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2.
; For a mass of 1969, the fuel required is 654.
; For a mass of 100756, the fuel required is 33583.
;
; part 2 examples:
; The fuel required by a module of mass 100756 and its fuel is:
; 33583 + 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2 = 50346.

(def input (->> (slurp "resources/day1-1.xml")
                clojure.string/split-lines
                (map #(Integer/parseInt %))))

(defn required-fuel [mass]
  (-> mass
      (/ 3)
      Math/floor
      (- 2)
      int))

(deftest part1
  (is (= 2 (required-fuel 12)))
  (is (= 2 (required-fuel 14)))
  (is (= 654 (required-fuel 1969)))
  (is (= 33583 (required-fuel 100756))))

(def part1 (->> (map required-fuel input)
                (reduce +)))
; 3305301

(defn total-required-fuel [mass]
  (reduce + (rest (take-while pos? (iterate required-fuel mass)))))

(deftest part2
  (is (= 50346 (total-required-fuel 100756))))

(def part2 (reduce + (map total-required-fuel input)))
; 4955106
