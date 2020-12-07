(ns day-4
  (:require [clojure.string :as cs]))

(def example-input
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(def input (slurp "resources/input-day-4.txt"))

(defn empty-line?
  [s]
  (= "" s))

(defn parse-kv-string [s]
  (let [[_ k v] (re-matches #"(.*):(.*)" s)]
    [k v]))

(defn validate-byr [v]
  (when (re-find #"\d{4}" v)
    (<= 1920 (Long/parseLong v) 2002)))

(defn validate-iyr [v]
  (when (re-find #"\d{4}" v)
    (<= 2010 (Long/parseLong v) 2020)))

(defn validate-eyr [v]
  (when 
    (<= 2020 (Long/parseLong v) 2030)))

(defn validate-hgt [v]
  (let [[_ height unit] (re-matches #"(\d*)(cm|in)" v)
        height (when height (Long/parseLong height))]
    (case unit
      "cm" (<= 150 height 193)
      "in" (<= 59 height 76)
      false)))

(defn validate-hcl [v]
  (re-find #"#[a-f0-9]{6}$" v))

(defn validate-ecl [v]
  (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} v))

(defn validate-pid [v]
  (re-find #"^\d{9}$" v))

(def fields
  {:required #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"}
   :optional #{"cid"}
   :validation {
                "byr" validate-byr
                "iyr" validate-iyr
                "eyr" validate-eyr
                "hgt" validate-hgt
                "hcl" validate-hcl
                "ecl" validate-ecl
                "pid" validate-pid}})

(defn validate-paper [p]
  (let [present-keys (into #{} (map first p))]
    (map #(and (contains? present-keys %)
               (((:validation fields) %) (second (first (filter (fn [[k v]] (= k %)) p)))))
         (:required fields))))

 (->> input
    cs/split-lines
    (partition-by empty-line?)
    (remove #(and (= 1 (count %)) (= "" (first %))))
    (map (fn process-papper [p] (->> (flatten (map #(cs/split %  #" ") p))
                                     (filter (complement empty-line?))
                                     (map parse-kv-string))))
    (map validate-paper)
    (map #(every? identity %))
    (filter identity)
    count)
