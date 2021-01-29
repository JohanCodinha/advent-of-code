(ns day-20
  (:require [clojure.java.io :as io]
            [clojure.string :as st]))

(def input (slurp (io/resource "input-day-20.txt")))

(def sea-monster-raw (slurp (io/resource "sea-monster-input-day-20.txt")))

(def demo-input (slurp (io/resource "demo-input-day-20.txt")))

(defn parse-sea-monster [s]
  (remove #(not= \# (second %))
          (->>  (st/split-lines s)
                (map-indexed
                 (fn [yr r]
                   (map-indexed
                    (fn [xr v] [[xr yr] v])
                    r)))
                (apply concat))))

(defn offset-monster [monster image-pos]
  (map (fn [[pos part]] [(mapv + pos image-pos) part]) monster))

(defn borders [tiles]
  (let [top (first tiles)
        right (apply str (map last tiles))
        down (last tiles)
        left (apply str (map first tiles))]

    {:top top :right right :down down :left left}))

(defn remove-borders [payload]
  (->> payload
       rest
       butlast
       (map (comp #(apply str %) rest butlast))))

(defn parsing [s]
  (let [tiles (st/split s #"\n\n")]
    (map
     (fn [tile-string]
       (let [[_ tile-id] (re-find #"Tile (\d+):" tile-string)
             payload (rest (st/split-lines tile-string))]
         {:id (Integer/parseInt tile-id)
          :payload payload}))
     tiles)))

(defn rotate-tile-right [tile]
  (map #(apply str %)
       (apply map vector
              (reverse tile))))
(defn flip-tile-right [tile]
  (map #(apply str (reverse %)) tile))

(defn orientations [tile]
  (concat
   (take 4 (iterate rotate-tile-right tile))
   (take 4 (iterate rotate-tile-right (flip-tile-right tile)))))

(defn border-match [{[x y] :pos o-border :borders} tile-pool]
  (keep
   (fn [{o'-border :borders :as o'}]
     (cond
       (= (:top o-border) (:down o'-border))    (assoc o' :pos [x (inc y)])
       (= (:left o-border) (:right o'-border))  (assoc o' :pos [(dec x) y])
       (= (:down o-border) (:top o'-border))    (assoc o' :pos [x (dec y)])
       (= (:right o-border) (:left o'-border))  (assoc o' :pos [(inc x) y])
       :else nil))
   tile-pool))

(defn merge-tile [tiles]
  (let [tiles-map(into {} (map #(vector (:pos %) %) tiles))
        ys (dedupe (sort (map (comp second first) tiles-map)))
        xs (dedupe (sort (map (comp first first) tiles-map)))
        rows-pos (map (fn [y] (map (fn [x] [x y]) xs)) ys)
        rows  (map (fn [rp] (map #(tiles-map %) rp)) rows-pos)]
    (reverse
     (map
      (fn [row]
        (->> row
             (sort-by :pos)
             (map (comp :payload))
             (reduce
              (fn [a i]
                (map (fn [x y] (concat x y)) a i)))
             (map #(apply str %))))
      rows))))

(defn part-1 [squares]
  (let [pos  (map :pos squares)
         p-map (into {} (map #(vector (:pos %) (:id %)) squares))
         _ (prn pos)
         xs (map first pos)
         ys (map second pos)
         tl (get p-map [(apply min xs) (apply max ys)])
         tr (get p-map [(apply max xs) (apply max ys)])
         dl (get p-map [(apply min xs) (apply min ys)])
         dr (get p-map [(apply max xs) (apply min ys)])]
    (apply * [tl tr dl dr])))

;;part-2
(time
 (let [tiles (parsing input)
       tiles-orientations (mapcat (fn [{:keys [payload] :as tile}]
                              (map (fn [orientation]
                                     (assoc tile
                                            :payload (remove-borders orientation)
                                            :borders (borders orientation)))
                                   (orientations payload)))
                            tiles)
       rest-of-tiles (fn [tiles id-set] (remove #(id-set (:id %)) tiles))
       pick (first tiles-orientations)
       ids (into #{} (map :id tiles))
       squares (loop [state [(assoc pick :pos [0 0])]]
           (if (= ids (into #{} (map :id state)))
             state
             (recur (concat state
                            (mapcat
                             #(border-match % (rest-of-tiles tiles-orientations (into #{} (map :id state))))
                             (into #{} state))))))
       image (->> squares
                  merge-tile
                  (apply concat))
       image-pos (into {} (apply concat (map-indexed
                                          (fn [yr r]
                                            (map-indexed
                                             (fn [xr v] [[xr yr] v])
                                             r))
                                          image)))
         sea-monster (parse-sea-monster sea-monster-raw)
         monster-part (count (filter (fn [[_ v]] (= \# v))image-pos))
         sea-monster-part (count sea-monster)
         image-orientations (->> image
                                 orientations
                                 (map (fn [i] (into {} (apply concat (map-indexed
                                                                      (fn [yr r]
                                                                        (map-indexed
                                                                         (fn [xr v] [[xr yr] v])
                                                                         r))
                                                                      i))))))
        monster-count (some #(when (not= 0 %) %) (map #(count
                                                           (filter
                                                            (fn [[i-pos _]]
                                                              (every? (fn [[pos v]] (= v (% pos))) (offset-monster sea-monster i-pos)))
                                                            %))
                                                         image-orientations))]
   (- monster-part (* sea-monster-part monster-count))))
