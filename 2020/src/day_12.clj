(ns day-12
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))

(def demo-input "F10
N3
F7
R90
F11")

(def input (slurp (io/resource "input-day-12.txt")))

(defn parse [s]
  (->> (vec (re-seq #"(\w)(\d*)" s))
       (map (comp vec rest))
       (map #(update % 1 (fn [i] (Integer/parseInt i))))))

(parse demo-input)
(let [[a & b] "he12"] [a b])
(update `("1" "2") 1 (constantly "nop"))
(frequencies (map second (filter #(#{"L" "R"} (first %)) (parse input))))
(count (parse input))

(defn direction [deg]
  (prn deg)
  (case (-> deg
            (quot 90)
            (rem 4)
            )
    0 :north
    1 :east
    2 :south
    3 :west
    -1 :west
    -2 :south
    -3 :east))

(let [final 
      (reduce (fn [ctx [action v]]
                (case action
                  "N" (update ctx :x #(+ % v))
                  "S" (update ctx :x #(- % v))
                  "E" (update ctx :y #(+ % v))
                  "W" (update ctx :y #(- % v))
                  "L" (update ctx :dir #(- % v))
                  "R" (update ctx :dir #(+ % v))
                  "F" (case (direction (:dir ctx))
                        :north (update ctx :x #(+ % v))
                        :east (update ctx :y #(+ % v))
                        :south (update ctx :x #(- % v))
                        :west (update ctx :y #(- % v))))
                )
              {:x 0 :y 0 :dir 90}
              (parse input))
      {:keys [x y]} final]
  (+ (Math/abs x) (Math/abs y)))

;; part-2
(defn rotate [dir x y]
  (case dir
    :east [(* -1 y) x]
    :west [y (* -1 x)]
    :south [(* -1 x) (* -1 y)]))
(let [final 
      (reduce (fn [{:keys [wx wy x y ] :as ctx} [action v]]
                (let [r (case action
                          "N" (update ctx :wy #(+ % v))
                          "S" (update ctx :wy #(- % v))
                          "E" (update ctx :wx #(+ % v))
                          "W" (update ctx :wx #(- % v))
                          "L" (let [[x y] (rotate (direction  v) wx wy)] (-> ctx
                                                                                   (assoc :wx x)
                                                                                   (assoc :wy y)))
                          "R" (let [[x y] (rotate (direction (* -1 v)) wx wy)] (-> ctx
                                                                            (assoc :wx x)
                                                                            (assoc :wy y)))
                          "F" (-> ctx
                                  (assoc :x (+ x (* v wx)))
                                  (assoc :y (+ y (* v wy)))))]
                  (prn r action v)
                  r))
              {:wx 10 :wy 1 :x 0 :y 0}
              (parse input))
      {:keys [x y]} final]
  (+ (Math/abs x) (Math/abs y)))

(rotate (direction -90) 10 4)
(rotate (direction 90) [10 4])
