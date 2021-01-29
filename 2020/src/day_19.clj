(ns day-19
  (:require [clojure.java.io :as io]
            [clojure.string :as st]
            [clojure.zip :refer [root next node edit end? zipper]]))

(def input (slurp (io/resource "input-day-19.txt")))

(def demo-input-2
  "0: 4 1 5
1: 6 3 | 3 2
3: 4 5 | 5 4
2: 4 2 | 5 5
4: \"a\"
5: \"b\"
6: 5 5 5 4 4 4 5 5 4

ababbb
bababa
abbbab
aaabbb
aaaabbb")

(def demo-input
  "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"

ababbb
bababa
bbbbbbaaab
aaaaaaaa
abbbab
aaabbb
aaaabbb")

(def part-2
  "8: 42 | 42 8
11: 42 31 | 42 11 31")

(defn parseInteger [s] (Integer/parseInt s))

(defn parse-integer-string [s] (re-seq #"\d+" s))

(defn parse-rule [s]
  (let [[_ n sub-rule-1 sub-rule-2 letter rule]
        (re-find #"(\d+): (?:(.*) \| (.*)|\"([a-z])\"|([ \d]+))" s)]
    (conj [n]
          (or (first letter)
              (when rule (parse-integer-string rule))
              (mapv parse-integer-string [sub-rule-1 sub-rule-2])))))

(defn replace-first [rules rule ]
  (loop [z (zipper seq? seq (fn [_ node] node) rule)]
    (let [d  (-> z next)]
      (cond
        (end? d) (root d)
        (string? (node d)) (-> d
                               (edit #(rules %))
                               root)
        :else (recur d)))))

(defn extend-first [rule]
  (loop [z (zipper seq? seq (fn [_ node] node) rule)]
    (let [d  (-> z next)]
      (cond
        (end? d) (conj (empty seq) (root d))
        (vector? (node d)) 
        (map (fn [o]
               (flatten
                (-> d
                    (clojure.zip/replace o)
                    root)))
             (node d))
        :else (recur d))))
  )
(defn redd  [rules]
  (reduce (fn [acc item]
            (cond
              (or (char? item) (string? item)) (map #(concat % [item]) acc)
              (vector? item) (mapcat (fn [i] (map #(concat % i) acc)) item)
              (seq? item) (map #(concat % item) acc)))
          [[]]
          rules))

(def res
  (time (let [input input
              [input-rules input-messages] (st/split input #"\n\n")
              part-2 (->> part-2
                          st/split-lines
                          (map parse-rule)
                          (into {}))
              rules (into (->> input-rules
                               st/split-lines
                               (map parse-rule)
                               (into {}))
                          part-2)
              messages (->> input-messages
                            st/split-lines
                            (map (fn [m] (map identity m))))
              step #(replace-first rules %)
              flow (fn [rules]
                     (mapcat #(->> %
                                   step
                                   extend-first
                                   (map flatten))
                             rules))
              
              map-f 
              (fn match-rule [rules messages]
                (loop [m messages
                       valid []
                       r rules]
                  (let [non-reduced (remove #(every? char? %) r)
                        relevant (filter
                                  #(some
                                    (fn [m']
                                      (let [match
                                            (map = m' (take-while char? %))]
                                        (every? identity match)))
                                    m)
                                  r)
                        {match true
                         not-matched nil} 
                        (group-by
                         (fn [m]
                           (some #(and (= (count %) (count m))
                                       (every? identity (map = % m)))
                                 relevant))
                         m)
                        relevant-m (filter (fn [m]
                                             (let [r (map #(take-while char? %) relevant)]
                                               (some (fn [r']
                                                       (when (<= (count r') (count m))
                                                         (every? identity (map = m r'))))
                                                     r)))
                                           not-matched)
                        ]
                    (comment
                      (prn :not (count not-matched))
                      (prn :match (count match))
                      (prn :valid (count valid))
                      (prn :rules (count r) (frequencies (map count r)))
                      (prn :messages (count m) (count relevant-m) (frequencies (map count relevant-m))))
                    (prn (count valid) (count r))
                    (if (or (empty? m) (empty? non-reduced))
                      (apply conj valid match)
                      (recur (remove (set match) relevant-m)
                             (concat valid match)
                             (flow relevant))))))]
       
          (map-f [(rules "0")] (sort-by count messages)))))

;;match-til

(take-while char? `(\a "1" "3"))
(remove (set `()) `(1 2 3))
