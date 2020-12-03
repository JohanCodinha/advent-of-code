(ns day4)

(def input "123257-647015")

; Facts
; 6 digit number
; withon input range
; left to right never decrease

(defn digits [n]
  (->> n
       str
       (map (comp read-string str))))

(defn same-adjacent-digits [digits]
  (not (coll? (reduce (fn [acc i]
                      (if (= (last acc) i)
                        (reduced i)
                        (conj acc i)))
                    []
                    digits))))

(def part1
  (let [input-range (map read-string (clojure.string/split input #"-"))]
    (->> (range (first input-range) (inc (second input-range)))
         (filter (fn [password]
                   (let [digits (digits password)]
                     (and (apply <= digits)
                          (same-adjacent-digits digits)))))
         count
         )))

(defn adjacent-digits-of-max-size [digits size]
  (first (filter #(= size (count %)) (vals (group-by identity digits)))))

(def part2
  (let [input-range (map read-string (clojure.string/split input #"-"))]
    (->> (range (first input-range) (inc (second input-range)))
         (filter (fn [password]
                   (let [digits (digits password)]
                     (and (apply <= digits)
                          (adjacent-digits-of-max-size digits 2)                          
                          ))))
         count
         )))
(defn ok [x] x)
(+ 1 "a
