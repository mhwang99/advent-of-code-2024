(ns aoc24.d01
  (:require [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))


(def ex
  (get-line-num
    "3 4
    4 3
    2 5
    1 3
    3 9
    3 3"))

(def in (get-line-num))

(defn q1 [l]
  (let [ll (sort (mapv first l))
        rl (sort (mapv second l))]
    (->> (map (fn [a b]
           (if (> a b) (- a b) (- b a)))
         ll rl)
         (apply +))))

#_(q1 in)

(defn q2 [l]
  (let [ll (mapv first l)
        rl (mapv second l)
        rlm (frequencies rl)]
    (reduce (fn [acc i]
              (+ acc (* i (get rlm i 0))))
            0 ll)))

#_(q2 in)
