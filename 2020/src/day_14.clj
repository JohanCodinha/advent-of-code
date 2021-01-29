(ns day-14
  (:require [clojure.string :as st]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(def demo-input "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")
(re-find #"mask" "msk = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
(def input (slurp (io/resource "input-day-14.txt")))

(defn parse [s]
  (->> (st/split-lines s)
       (partition-by #(re-find #"mask" %))
       (partition 2)
       (map flatten)
       (map (fn [[mask & mems]]
              [(second (re-find #"mask = (.*)$" mask))
               (map #(vec (map (fn [i] (Integer/parseInt i)) (rest (re-find #"mem\[(\d+)\] = (\d+)" %))))
                    mems)]))))

(take 2 (parse demo-input))

(defn apply-mask [mask value]
  (reduce (fn [acc [v i]]
            (case v
              \X acc
              \1 (bit-set acc i)
              \0 (bit-clear acc i)))
          value
          (map vector (st/reverse mask) (range))))

(->> input
     parse
     (map (fn [[mask mems]]
            (into {} (map #(update % 1 (partial apply-mask mask)) mems))))
     (reduce merge)
     (vals)
     (reduce +))
;; => 15919415426101

(defn replace-at [s idx replacement]
  (str (subs s 0 idx) replacement (subs s (inc idx))))

(defn apply-mask-pt2 [mask value]
  (reduce (fn [acc [v i]]
            (case v
              \X acc
              \1 (bit-set acc i)
              \0 acc))
          value
          (map vector (st/reverse mask) (range))))

(defn generate-add [mask add]
  (let [masked (apply-mask-pt2 mask add)
        combis (apply combo/cartesian-product 
                      (keep-indexed (fn [i c] (when (= \X c) [[i 0] [i 1]])) (st/reverse mask)))]
    (prn :masked masked :combis combis)
    (map (fn [combi]
           
           (reduce (fn [masked' [id b]]
                    
                     (let [r (if (zero? b) (bit-set masked' id) (bit-clear masked' id))]
                       
                       r)
                     
                     ) masked combi))
         combis)))


(->> demo-input
     parse
     (map (fn [[mask mems]]
            (->> mems
                 (map
                  (fn [[add mem]]
                    (prn add)
                    (generate-add mask add)))
                 #_(into {}))))
     #_(reduce merge)
     #_(vals)
     #_(reduce +))

(format "%04s" "12")
(format "%032d" 11010)
(Integer/toString 12 2)
(Integer/toString (bit-set 12 9) 2)
(format "%032d" 2r12 #_(Integer/toString 12 2))
(count "000000000000000000000000000000011010")
(Integer/toString (apply-mask-pt2  12) 2)
;; => ("000000000000000000000000000000011010"
;;     "000000000000000000000000000000011011"monitor
;;     "000000000000000000000000000000111010"
;;     "000000000000000000000000000000111011")
;; => ("000000000000000000000000000000001010"
;;     "000000000000000000000000000000001011"
;;     "000000000000000000000000000000101010"
;;     "000000000000000000000000000000101011")
(Integer/toString (Integer/parseInt "0111" 2) 10)
