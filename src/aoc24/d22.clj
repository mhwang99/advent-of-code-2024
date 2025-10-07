(ns aoc24.d22
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))

(def in (map first (get-line-num)))

(defn mix [a b] (bit-xor a b))
(defn prune [a] (mod a 16777216))
(defn s1 [x] (-> (* x 64) (mix x) prune))
(defn s2 [x] (-> (/ x 32) int (mix x) prune))
(defn s3 [x] (-> (* x 2048) (mix x) prune))
(defn gen-secret [x] (-> x s1 s2 s3))

(defn q1
  [l]
  (->> l
       (map #(nth (iterate gen-secret %) 2000))
       (reduce +)))

#_(q1 in)

(defn get-changes-map
  [n x]
  (->> (iterate gen-secret x)
       (take n)
       (map #(mod % 10))
       (partition 5 1)
       (map (fn [[p1 p2 p3 p4 p5]]
              [[(- p2 p1)
                (- p3 p2)
                (- p4 p3)
                (- p5 p4)]
               p5]))
       reverse
       (into {})))

(defn q2
  [l]
  (->> (map #(get-changes-map 2000 %) l)
       (apply merge-with +)
       vals
       (reduce max)))

#_(q2 in)
