(ns day-16
  (:require [clojure.string :as st]
            [clojure.java.io :as io]
            [clojure.set :refer [intersection difference union]]))

(def demo-input "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12")

(def input (slurp (io/resource "input-day-16.txt")))
(defn parseInt [s] (Integer/parseInt s))

(defn parse [s]
  (let [[rules-st your-ticket-st nearby-tickets-st] (st/split s #"\n\n")
        rules (->> (st/split-lines rules-st)
                   (map #(re-find #"(.+): (\d+)-(\d+) or (\d+)-(\d+)" %))
                   (map (fn [[_ rule r1-min r1-max r2-min r2-max]]
                          {:rule rule
                           :ranges [[(parseInt r1-min) (parseInt r1-max)]
                                    [(parseInt r2-min) (parseInt r2-max)]]})))
        your-ticket (->> (st/split-lines your-ticket-st)
                         rest
                         (map #(map parseInt (st/split % #",")))
                         first)
        nearby-tickets (->> (st/split-lines nearby-tickets-st)
                         rest
                         (map #(map parseInt (st/split % #",")))
                         )]
    {:rules rules
     :ticket your-ticket
     :nearby-tickets nearby-tickets}))

(parse demo-input)
;; => {:rules
;;     ({:rule "class", :ranges [[1 3] [5 7]]}
;;      {:rule "row", :ranges [[6 11] [33 44]]}
;;      {:rule "seat", :ranges [[13 40] [45 50]]}),
;;     :ticket (7 1 14),
;;     :nearby-tickets ((7 3 47) (40 4 50) (55 2 20) (38 6 12))}

(let [{:keys [rules ticket nearby-tickets]} (parse input)
      rules-pred (reduce (fn [acc {:keys [ranges rule]}]
                           (conj acc
                                 [rule (map (fn [[r-min r-max]]
                                              #(<= r-min % r-max))
                                            ranges)]))
                         [] rules)
      check-part1 (apply some-fn (flatten (map second rules-pred)))
      part-1 (->> nearby-tickets
                  flatten
                  (filter #(when-not (check-part1 %) %))
                  (reduce +))
      valid-tickets (->> nearby-tickets
                         (filter #(apply = true (map check-part1 %))))
      
      possible-fields  (fn [i]
                         (set (keep (fn [[rule preds]]
                                      (when ((apply some-fn preds) i) rule))
                                    rules-pred)))
      
      grid (mapv #(mapv possible-fields %) valid-tickets)
      possible-fields-set (map (fn [i]
                                 (apply intersection (map #(get % i) grid)))
                               (range (count (first grid))))]

  (->> (range)
       (map vector possible-fields-set)
       (sort-by (comp count first))
       (reduce (fn [acc [rules id]]
                 (let [[unique] (remove acc rules)]
                   (prn (remove acc rules))
                   (assoc acc unique id)))
               {})
       (filter (fn [[k _v]]
                 (st/starts-with? k "departure")))
       (map second)
       ((fn [idx] (keep-indexed (fn [i v] (when ((set idx)  i) v)) ticket)))
       (apply *))  
    )
;; => 1289178686687

