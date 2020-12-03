(ns day2
 (:require [clojure.test :refer [is deftest run-tests]]))

(def input
  (-> (slurp "resources/day5.txt") 
      clojure.string/split-lines 
      first
      (clojure.string/split #",")
      (as-> strings (mapv #(Integer/parseInt %) strings))))

; 1 add pos 1 2, store to 3
; 2 mul pos 1 2, store to 3
; 99 program finished

(defn instruction-params-modes
  [instruction]
  (->> (clojure.string/split (str instruction) #"")
       (split-at (- (count (str instruction)) 2))
       ((juxt
          #(map (fn [s] (Integer/parseInt s)) (first %))
          #(Integer/parseInt (apply str (last %))))) 
       ))
(instruction-params-modes 1002)
(defn extract-params [instruction program]
  )

(def op-code->instruction
  {1 +
   2 *
   3 (fn [position program]
       (assoc program
              position
              (-> (read-line)
                  Integer/parseInt)))
   4 (fn [position program]
       (println (get program position))
       program)})

(defn execute [{:keys [program step]}]
  (let [[op-code input-a-pos input-b-pos res-pos]
        (take 4 (drop (* 4 step) program))
        input-a (get program input-a-pos)
        input-b (get program input-b-pos)]
   (when-let [instruction (op-code->instruction op-code)]
     {:program (assoc program res-pos (instruction input-a input-b))
      :step (inc step)})))

(defn compute [program]
  (-> (take-while (complement nil?)
                  (iterate execute {:program program
                                    :step 0}))
       last
       :program))

(deftest part1-examples
    (is (= [2,0,0,0,99] (compute [1,0,0,0,99]))) 
    (is (= [2,3,0,6,99] (compute [2,3,0,3,99]))) 
    (is (= [2,4,4,5,99,9801] (compute [2,4,4,5,99,0]))) 
    (is (= [30,1,1,4,2,5,6,0,99] (compute [1,1,1,4,99,5,6,0,99]))))

(def part1
  (-> (assoc input
             1 12
             2 2)
      compute
      first))
; 5098658

(def part2
  (let [{:keys [noun verb]}
         (some
           #(and (= 19690720 (:output %)) %)
           (for [noun (range 100)
                 verb (range 100)
                 :let [output
                       (-> (assoc input
                                  1 noun
                                  2 verb)
                           compute
                           first)]]
             {:output output
              :noun noun
              :verb verb}))]
    (+ verb (* 100 noun))))
; 5064

