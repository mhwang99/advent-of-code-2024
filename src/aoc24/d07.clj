(ns aoc24.d07
  (:require [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))


(def ex
  (get-line-num
"190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20"))
(def in (get-line-num))


(defn q1
  [ll]
  (->> ll
       (filter (fn [[expect n1 & l]]
                 (loop [prevs #{n1}
                        l l]
                   (if-not (seq l)
                     (get prevs expect)
                     (let [v (first l)]
                       (recur (set
                                (mapcat (fn [prev]
                                          [(+ prev v)
                                           (* prev v)])
                                        prevs))
                              (next l)))))))
       (map first)
       (apply +)))

#_(q1 in)

(defn q2
  [ll]
  (->> ll
       (filter (fn [[expect n1 & l]]
                 (loop [prevs #{n1}
                        l l]
                   (if-not (seq l)
                     (get prevs expect)
                     (let [v (first l)]
                       (recur (set
                                (mapcat (fn [prev]
                                          [(+ prev v)
                                           (* prev v)
                                           (atoi (str prev v))])
                                        prevs))
                              (next l)))))))
       (map first)
       (apply +)))

#_(q2 in)
