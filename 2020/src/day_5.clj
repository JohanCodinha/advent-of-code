(ns day-5)

(def input (-> (slurp "resources/input-day-5.txt")
               clojure.string/split-lines))
(defn lower-half [[start stop]]
  [start (- stop (quot (inc (- stop start)) 2))])

(defn upper-half [[start stop]]
  [(+ start (quot (inc (- stop start)) 2)) stop])

(defn parse-input [s]
  (reduce (fn [acc char]
            (case char
              \F (update acc :row lower-half)
              \B (update acc :row upper-half)
              \L (update acc :col lower-half)
              \R (update acc :col upper-half)
              )) {:row [0 127] :col [0 7]} s))

(defn seat-id [{:keys [row col] :as pass}]
  (when (or (apply not= col) (apply not= row)) (prn row col))
  (merge pass {:seat-id (+ (first col) (* (first row) 8))}))

(defn missing-number [xs]
  (let [first (first xs)
        last (last xs)
        full (set (range first (inc last)))]
    (clojure.set/difference full (set xs))))
;; part-1
(->> input
     (map parse-input)
     (map seat-id)
     (map :seat-id)
     (sort)
     last)
;; => 922

(->> input
     (map parse-input)
     (map seat-id)
     (map :seat-id)
     (sort)
     missing-number);; => #{747}
