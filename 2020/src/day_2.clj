(ns day-2)

(def input-1 (->> (slurp "resources/input-day-2.txt")
                  clojure.string/split-lines))

(defn parse-input [s]
  (let [[_ low high letter pwd] (re-find #"(\d+)-(\d+) (\w): (\w+)" s)]
    {:low (Integer/parseInt low) :high (Integer/parseInt  high) :letter letter :pwd pwd}))

(defn is-valid-pwd-part1 [{:keys [low high letter pwd]}]
  (let [occurence (count (re-seq (re-pattern letter) pwd))]
    (<= low occurence high)))

(defn is-valid-pwd-part2 [{:keys [low high letter pwd]}]
  (let [first-pos (str (get pwd (dec low)))
        second-pos (str (get pwd (dec high)))]
    (not= (= letter first-pos) (= letter second-pos))))

(->> input-1
     (map parse-input)
     (map is-valid-pwd-part1)
     (filter identity)
     count)

(->> input-1
     (map parse-input)
     (map is-valid-pwd-part2)
     (filter identity)
     count)
 
