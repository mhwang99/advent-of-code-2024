(ns aoc24.d03
  (:require [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))

(def ex "xmul(2,4)%&mul[3,7344]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(def in (get-res))

(defn q1 [s]
  (->> (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" s)
       (map #(* (atoi (nth % 1)) (atoi (nth % 2))))
       (apply +)))

#_(q1 ex)
#_(q1 in)

(def ex2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
(defn q2 [s]
  (->> (re-seq #"(?:mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\))" s)
       (reduce (fn [[do? tot] [op v1 v2]]
                 (cond
                   (= op "do()") [true tot]
                   (= op "don't()") [false tot]
                   :else (if do?
                           [true (+ tot (* (atoi v1) (atoi v2)))]
                           [false tot])))
               [true 0])
       second))

#_(q2 ex2)
#_(q2 in)
