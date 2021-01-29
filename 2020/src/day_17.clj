(ns day-17
  (:require [clojure.string :as st]))

(def demo-input ".#.
..#
###
")

(def input "#####..#
#..###.#
###.....
.#.#.#..
##.#..#.
######..
.##..###
###.####
")

(defn parse [s]
  (->> (st/split-lines s)
       (map-indexed (fn [i l]
                      (keep-indexed #(when (= \# %2) [i %1 0 0] ) l)))
       (apply concat)))

(defn neighbors [[x y z w]]
  (for [xs (range (dec x) (+ 2 x))
        ys (range (dec y) (+ 2 y))
        zs (range (dec z) (+ 2 z))
        ws (range (dec w) (+ 2 w))
        :let [pos [xs ys zs ws]]
        :when (not= pos [x y z w])]
    pos))

(defn cube-step [state pos]
  (let [active? (state pos)
        active-neigbhors (count (keep state (neighbors pos)))]
    (cond
      (and active? (<= 2 active-neigbhors 3))
      pos
      (and (not active?) (= active-neigbhors 3))
      pos
      :else
      nil)
    ))

(defn generation [state]
  (let [state-set (set state)
        all-neighbors (->> state
                           (map neighbors)
                           (apply concat)
                           distinct)]
    (keep (fn [pos] (cube-step state-set pos)) all-neighbors)))

(let [s input
      state (parse s)]
  (->> state
       (iterate generation)
       (take 7)
       last
       count))
;; Part-2
;; => 2620

;; Part-1
;; => 336
