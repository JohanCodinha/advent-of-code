(ns day-24
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :refer [union difference]]))

(def demo
  "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew")

(def inputs (slurp (io/resource "input-day-24.txt")))

(def orientation->coord {"e"  [1 0]
                         "w"  [-1 0]
                         "ne" [1 -1]
                         "nw" [0 -1]
                         "sw" [-1 1]
                         "se" [0 1]})
;;Part-1
(let [floor (map #(re-seq #"e|se|sw|w|nw|ne" %)
                 (string/split-lines demo #_inputs))]
  (->> (map (fn [tile] (map orientation->coord tile)) floor)
       (map (fn [tile]
              (reduce (fn [[q r] [q' r']] [(+ q q') (+ r r')]) [0 0] tile)))
       frequencies
       (filter (fn [[_ freq]] (= 1 freq)))
       count))

(defn adjacent [[q' r']]
  (map (fn [[_ [q r]]]
         [(+ q q') (+ r r')])
       orientation->coord))

(def black-flipped (comp #(some identity %)
                         (juxt #(= 0 %) #(< 2 %))))
(defn day [blacks-set]
  (let [adjacent-white (into #{} (mapcat adjacent blacks-set))
        black->white (keep (fn [tile] (when (black-flipped (count (filter blacks-set (adjacent tile))))
                                              tile))
                                 blacks-set)
        white->black (keep (fn [tile] (when (= 2 (count (filter blacks-set (adjacent tile))))
                                              tile) ) adjacent-white) ]
    (union (difference blacks-set (set black->white))
                (set white->black))))
;;Part-2
(time
 (let [floor (map #(re-seq #"e|se|sw|w|nw|ne" %)
                  (string/split-lines inputs))
       day-0 (->> (map (fn [tile] (map orientation->coord tile)) floor)
                  (map (fn [tile]
                         (reduce (fn [[q r] [q' r']] [(+ q q') (+ r r')]) [0 0] tile)))
                  frequencies
                  (filter (fn [[_ freq]] (= 1 freq)))
                  (map first)
                  (into #{}))]
   (count (last (take 101 (iterate day day-0))))))


