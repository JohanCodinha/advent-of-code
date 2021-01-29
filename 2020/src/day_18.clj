(ns day-18
  (:require [clojure.java.io :as io]
            [clojure.string :as st]
            [clojure.walk :refer [postwalk]]))

(def input (slurp (io/resource "input-day-18.txt")))

(def examples [["1 + (2 * 3) + (4 * (5 + 6))" 51 51]
               ["2 * 3 + (4 * 5)" 26 46]
               ["5 + (8 * 3 + 9 + 3 * 4 * 3)" 437 1445]
               ["5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))" 12240 669060]
               ["((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2" 13632 23340]])

(defn simple-expression [tks]
  (reduce (fn [acc c]
            (cond
              (= "(" c)             []
              (nil? (#{"(" ")"} c)) (conj acc c)
              (= ")" c)             (reduced acc)))
          [] tks))

(defn calc [tks]
  (first
   (reduce (fn [acc t]
             (cond
               (= "*" (last acc)) [(* (first acc) t)]
               (= "+" (last acc)) [(+ (first acc) t)]
               :else (conj acc t))) [] tks)))

(defn simple-expression [tks]
  (reduce (fn [acc [i c]]
            (cond
              (= "(" c)             []
              (nil? (#{"(" ")"} c)) (conj acc [i c])
              (= ")" c)             (reduced acc)))
          [] tks))

(defn step [exp]
  (let [exp' (map vector (range) exp)
        nxt (simple-expression exp')
        [h t] (split-at (first (first nxt)) exp)]
    (concat (butlast h)
            [(map second nxt)]
            (rest (drop (count nxt) t)))))

(defn parser [s]
  (loop [exps (->> s
                   (re-seq #"\(|\)|\d+|\+|\*")
                   (map (fn [s] (if (every? #(Character/isDigit %) s)
                                  (Integer/parseInt s)
                                  s))))]
    (if (some #{"(" ")"} exps)
      (recur (step exps))
      exps)))

(defn solve [s]
  (let [parsed (parser s)]
    (postwalk (fn [x]
                (cond
                  (and (seq? x)
                       (not (some seq? x))) (calc x)
                  :else x)) parsed)))
;;part-1
(map (fn [[e a1]] (= (solve e) a1))
     examples)

(reduce + (map solve (st/split-lines input)))
;; => 12956356593940

(defn calc-+ [tks]
  (reduce (fn [acc t]
            (cond
              (= "+" (last acc)) (vec (conj (vec (butlast (butlast acc)))
                                            (+ (last (butlast acc))
                                               t)))
              :else (conj acc t))) [] tks))

;;part-2
(defn solve-2 [s]
  (let [parsed (parser s)]
    (postwalk (fn [x]
                (cond
                  (and (seq? x)
                       (not (some seq? x))) (calc (calc-+ x))
                  :else x)) parsed)))

(map (fn [[e _a1 a2]] (= (solve-2 e) a2))
     examples)

(reduce + (map solve-2 (st/split-lines input)))

;; => 94240043727614
