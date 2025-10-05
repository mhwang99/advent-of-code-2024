(ns aoc24.d19
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))

(defn parse-input
  [s]
  (let [[ptns _ & dgs] (get-lines s)]
    [(mapv s/trim (s/split ptns #","))
     (mapv s/trim dgs)]))

(def valid-count
  (memoize
    (fn [ptns dg]
      (->> (reduce (fn [dgs ptn]
                     (if (s/starts-with? dg ptn)
                       (conj dgs (subs dg (count ptn)))
                       dgs))
                   [] ptns)
           (map #(if (= "" %) 1 (valid-count ptns %)))
           (reduce +)))))

(defn q1
  [[ptns dgs]]
  (->> dgs
       (map #(valid-count ptns %))
       (filter pos?)
       count))

#_(q1 in)

(defn q2
  [[ptns dgs]]
  (->> dgs
       (map #(valid-count ptns %))
       (reduce +)))

#_(q2 in)
