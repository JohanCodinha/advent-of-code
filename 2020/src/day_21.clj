(ns day-21
  (:require [clojure.java.io :refer [resource]]
            [clojure.string :as str]
            [clojure.set :refer [intersection difference union]]))

(def demo-input "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)")

(def input (slurp (resource "input-day-21.txt")))

(defn parse
  "[[ingredients ...] [alergens ...] ]"
  [s]
  (->> (str/split-lines s)
       (map (fn [l]
              (let [[_ ingredients-str alergens-str] (re-matches #"(.*)\(contains (.*)\)" l)
                    ingredients (str/split ingredients-str #" ")
                    alergens (str/split alergens-str #", ")]
                [(set ingredients) (set alergens)])))))
(defn shared-ingredients [foods]
  (apply intersection (map first foods)))

(let [food-list (parse input)
      all-alergen (reduce into #{} (map second food-list))
      all-ingredients (apply concat (map first food-list))
      food-by-alergen (fn [alergen] (filter (fn [[ingredients alergens]] (alergens alergen)) food-list))
      step (fn [alergens]
             (->> all-alergen
                  (map
                   (fn [alergen] [alergen (shared-ingredients (food-by-alergen alergen))]))
                  (map (fn [[a i]] [a (difference i (apply union (map second alergens)))]))
                  (filter (fn [[alergen ingredients]] (= 1 (count ingredients))))
                  (into {})
                  (merge alergens)))
      alergens (->> {}
                    (iterate step)
                    (partition 2)
                    (take-while #(apply not= %))
                    last
                    last)
      part-1 (count (remove (->> alergens
                                 (map second)
                                 (apply union))
                            all-ingredients))
      part-2 (->> alergens
                  (sort-by first)
                  (map second)
                  (apply concat)
                  (interleave (repeat ","))
                  rest
                  (apply str))]
  {:part-1 part-1
   :part-2 part-2})
;; => {:part-1 2324, :part-2 "bxjvzk,hqgqj,sp,spl,hsksz,qzzzf,fmpgn,tpnnkc"}
