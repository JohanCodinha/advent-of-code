(ns day-3)

(def input-1 (->> (slurp "resources/input-day-3.txt")
                  clojure.string/split-lines))

(def ex-input
  ["..##......."
   "#...#...#.."
   ".#....#..#."
   "..#.#...#.#"
   ".#...##..#."
   "..#.##....."
   ".#.#.#....#"
   ".#........#"
   "#.##...#..."
   "#...##....#"
   ".#..#...#.#"])

(defn str->infinite-str [str]
  (->> str
     (map identity)
     repeat
     flatten))

(def infinite-grid
  (map str->infinite-str input-1))

(defn get-value-at-pos [grid x y]
  (-> grid
      (nth x)
      (nth y)))

(defn infinite-slope [right down]
  (iterate
   (fn [[x y]] [(+ x down) (+ y right)])
   [0 0]))

(defn count-tree-encounter [[right down]]
  (let [slope (infinite-slope right down)
        grid-height (count infinite-grid)
        encounters (->> (map (fn [[x y]]
                              (get-value-at-pos infinite-grid x y)) slope)
                       (take grid-height)
                       rest
                       frequencies)]
    (get encounters \#)))

;; part 1
(count-tree-encounter [3 1])

;;part 2
(apply * (map count-tree-encounter [[1 1] [3 1] [5 1] [7 1] [1 2]]))
