(ns day-25)

(def inputs
  "19774466
7290641")

(def magic-num 20201227)

(defn loop-size [public-key]
  (let [sub-num 7]
    (loop [value 1
           loop-size 0]
      #_(prn value loop-size (rem (* value sub-num) magic-num))
      (if (= public-key value)
        loop-size
        (recur (rem (* value sub-num) magic-num) (inc loop-size)))
      )))

(defn transform [sub-num loop-size]
  (loop [value 1
         loop-size loop-size]

    (if (= 0 loop-size)
        value
        (recur (rem (* value sub-num) magic-num) (dec loop-size)))))

(loop-size 5764801)
(loop-size 17807724)

(time (transform 19774466 (loop-size 7290641) ))

