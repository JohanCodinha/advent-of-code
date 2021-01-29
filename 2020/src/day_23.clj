(ns day-23 
  (:require
   [clojure.string :as string]))

(set! *warn-on-reflection* true)

(def demo "389125467")
(def input "418976235")

(defn parse [s]
  (map (fn [c] (Integer/parseInt c)) (string/split s #"")))

(defn cups->circle ^longs [cups]
  (let [circle (long-array  (inc (count cups)))]
    (doseq [[a b] (partition 2 1 cups)]
      (aset-long circle a b))
    (aset-long circle (last cups) (first cups))
    circle))

;; C X Y Z D A 4 6 7
;; C D X Y Z A 4 6 7

;;  3 (2) 8  9  1  5  4  6  [7]
;;         \ \ \
;;          \ \ \
;;           \ \ \
;;            \ \ \
;;  3  2 (5) 4  6  7  8  9  1

(time
 (let [inputs (parse input)
       init-circle (concat inputs
                      (range (inc (apply max inputs))
                             (inc 1000000)))
       min-circle (reduce min init-circle)
       ^longs game
       (loop [circle (cups->circle init-circle)
              step 10000000
              current (first init-circle)]
         (if (zero? step)
           circle
           (let [x (aget circle current)
                 y (aget circle x)
                 z (aget circle y)
                 
                 destination (loop [label (dec current)]
                               (cond
                                 (#{x y z} label)
                                 (recur (dec label))
                                 (< label min-circle)
                                 (reduce max (remove #{x y z} circle))
                                 :else label))
                 a (aget circle z)]
             (doto circle
               (aset-long current a)
               (aset-long z (aget circle destination))
               (aset-long destination x))
             (recur circle (dec step) (aget circle current)))))      
       first-after-one (aget game 1)
       second-after-one (aget game first-after-one)]
   {:first first-after-one
    :second second-after-one
    :product (* first-after-one second-after-one)}))

(defn turn [{:keys [cups]}]
    (let [current (first cups)
          crab-pick (rest (take 4 cups))
          low (first (sort cups))
          high (last (sort cups))
          leftover (concat [current] (drop 4 cups))
          destination (loop [label (dec current)]
                        (cond
                          (< label (first (sort leftover)))
                          (last (sort leftover))
                          (contains? (set crab-pick) label)
                          (recur (dec label))
                          :else label))
          end (flatten (map (fn [c] (if (= c destination)
                                      [c crab-pick]
                                      c)) leftover))]
      {:crab-pick crab-pick
       :current current
       :destination destination
       :leftover leftover
       :en (map (fn [c] (if (= c destination)
                          [c crab-pick]
                          c)) leftover)
       :end end
       :cups (concat (rest end) [(first end)])})
  )

(defn rotate-to [coll i]
  (concat
   (drop-while #(not= i %) coll)
   (take-while #(not= i %) coll)))

(time
 (let [cups (parse input)
       part-1 (rest
               (rotate-to
                (:cups (->> {:cups cups}
                            (iterate turn)
                            (take 1001)
                            last))
                1))
       ]
   
   part-1))
