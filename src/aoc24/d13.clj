(ns aoc24.d13
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))

(defn parse-input [s]
  (->> (get-line-num s)
       (keep seq)
       (partition-all 3)))

(def ex (parse-input
"Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"))
(def in (parse-input (get-res)))

;; axA + bxB = tx
;; ayA + byB = ty
(defn get-value
  [[[ax ay] [bx by] [tx ty]]]
  (let [d (- (* ax by) (* ay bx))]
    (when-not (zero? d)
      (let [p (- (* by tx) (* bx ty))
            q (- (* ax ty) (* ay tx))]
        (when (= (mod p d) (mod q d) 0)
          [(quot p d) (quot q d)])))))

(defn get-tokens [[an bn]] (+ (* 3 an) bn))

(defn q1
  [games]
  (->> games
       (keep get-value)
       (map get-tokens)
       (apply +)))

#_(q1 in)

(defn q2
  [games]
  (->> games
       (map (fn [[a b t]] [a b (map + (repeat 10000000000000) t)]))
       q1))

#_(q2 in)

