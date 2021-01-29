(ns day-13
  (:require [clojure.string :as cs]
            [clojure.java.io :as io]))

(def demo-input "939
7,13,x,x,59,x,31,19")

(def input (slurp (io/resource "input-day-12.txt")))

(defn pos-range [n] (iterate inc n))
;part-1
(let [[first-line second-line] (cs/split-lines input)
      arrive-at (Integer/parseInt first-line)
      bus-ids (->> (cs/split second-line #",")
                   (filter #(not= "x" %))
                   (map #(Integer/parseInt %)))
      res (some
           (fn [ts] (some #(when (= 0 (rem ts %)) {:ts ts :id %}) bus-ids))
           (pos-range arrive-at))]
  [(* (:id res) (- (:ts res) arrive-at)) res])

;;part-2
(defn isDeparting? [t id]
  (if (= :x id)
    true
    (= 0 (rem t id))))

(defn departures [id]
  (iterate #(+ id %) 0))

(defn distance [list id]
  (let [id-pos (.indexOf (vec list) id)]
    (map-indexed (fn [i b] [b (- i id-pos)]) list)))


(defn is-next-minute [id ts]
  (if (= :x id)
    true
    (zero? (rem (inc ts) id))))

(time
 (let [[_ second-line] (cs/split-lines input)
       bus-ids (->>(cs/split second-line #",")
                   (map #(if (= "x" %)
                           :x
                           (Integer/parseInt %))))
       max-bus-id (->> bus-ids
                       (filter #(not= :x %))
                       (apply max))
       
       distances (filter #(not= :x (first %)) (distance bus-ids max-bus-id ))]
   ;;Brute force
   #_(some (fn [ts]
           (let [should (map #(update % 1 (partial + ts)) distances)
                 should' (map #(isDeparting? (second %) (first %)) should)]
             (when (= 0 (rem ts 1000000)) (prn ts))
             (when (apply = true should') [(-> should first second) ts should' should])))
           (iterate #(+ max-bus-id %) 0))
   ;;Smart
   (loop [])
   
   ))


;; => ([7 -4] [13 -3] [59 0] [31 2] [19 3])
(take 10 (departures 59))
(reduce )
;; => (7 13 :x :x 59 :x 31 19)
(quot 350 7)
(quot 351 13)
(rem (+ (- 15 356)  (* 7 13 59 31)) 31)

(rem (inc (* 7 11)) 13 )

(defn magic [[ts id-product] [index id]]
  (loop [ts ts]
    (if (zero? (mod (+ ts index) id))
      [ts (* id-product id)]
      (recur (+ ts id-product)))))

(let [[_ second-line] (cs/split-lines input)
      bus-ids (->>(cs/split second-line #",")
                  (map #(if (= "x" %)
                          :x
                          (Integer/parseInt %)))
                  (interleave (range))
                  (partition 2)
                  (filter #(not= :x (second %))))]
  (reduce magic bus-ids))
;; => [775230782877242 1398323334468437]
;; https://www.youtube.com/watch?v=vZWZ5VxbCTs&t=186s
