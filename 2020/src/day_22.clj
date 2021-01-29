(ns day-22 
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

(def demo-input "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10")

(def demo-2 "Player 1:
43
19

Player 2:
2
29
14
")

(def input (slurp (io/resource "input-day-22.txt")))

(defn parse [s]
  (let [
      players (str/split s #"\n\n")
      [p1 p2] (->> players
                   (map (comp rest str/split-lines))
                   (map (fn [p] (mapv (fn [i] (Integer/parseInt i)) p))))]
    [p1 p2]))

(defn turn [{:keys [p1 p2]}]
  (let [p1c (first p1)
        p2c (first p2)]
    (if (or (nil? p1c) (nil? p2c))
      {:winner (if (empty? p1) :p2 :p1)
       :p1 p1
       :p2 p2}
      (cond
        (< p1c p2c)
        {:p1 (rest p1)
         :p2 (concat (rest p2) [p2c p1c])}
        (< p2c p1c)
        {:p1 (concat (rest p1) [p1c p2c])
         :p2 (rest p2)}))))


(defn play [player-1 player-2]
  (loop [state {:p1 player-1 :p2 player-2}
                    p1-decks []
                    p2-decks []
                    round 1]
               (let [{:keys [winner] :as played-round} (turn state)
                     p1r (rest (state :p1))
                     p2r (rest (state :p2))
                     p1c (first (state :p1))
                     p2c (first (state :p2))
                     play-sub (and p1c p2c (<= p1c (count p1r)) (<= p2c (count p2r)))
                     sub-winner (:winner (when play-sub (play (take p1c p1r) (take p2c p2r))))]
                 (cond
                   (or (contains? (set p1-decks) (state :p1))
                       (contains? (set p2-decks) (state :p2))) (assoc state :winner :p1)  
                   
                   sub-winner (recur (if (= :p1 sub-winner)
                                       {:p1 (concat p1r [p1c p2c])
                                        :p2 p2r}
                                       {:p1 p1r
                                        :p2 (concat p2r [p2c p1c])})
                                     (conj p1-decks (state :p1))
                                     (conj p2-decks (state :p2))
                                     (inc round))
                   winner played-round
                   :else (recur played-round
                                (conj p1-decks (state :p1))
                                (conj p2-decks (state :p2))
                                (inc round))))))

(let [game (apply play (parse input))]
  [(apply + (map * (reverse (game (game :winner))) (rest (range))))
   game])

