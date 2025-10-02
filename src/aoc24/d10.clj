(ns aoc24.d10
  (:require [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))

(def ex
  (get-line-ichars
"89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732"))

(def in (get-line-ichars))

(defn get-all-pts
  [board]
  (let [rows (count board)
        cols (count (first board))]
    (reduce
      (fn [ret row]
        (reduce
          (fn [ret col]
            (let [v (get-in board [row col])]
              (update ret v (fnil conj #{}) [row col])))
          ret (range cols)))
      {} (range rows))))

(defn get-next-pts
  [pts pred]
  (->> pts
       (mapcat (fn [pt]
                 (map #(mapv + pt %)
                      [[1 0] [-1 0] [0 1] [0 -1]])))
       (filter pred)
       set))

(defn trailhead-point
  [m pt]
  (loop [v 0
         pts #{pt}]
    (if (seq pts)
      (if (= v 9)
        (count pts)
        (recur (inc v)
               (get-next-pts pts (get m (inc v)))))
      0)))


(defn q1
  [board]
  (let [m (get-all-pts board)]
    (->> (get m 0)
         (map #(trailhead-point m %))
         (apply +))))

#_(q1 in)

(defn get-next-routes
  [routes pred]
  (->> routes
       (mapcat (fn [route]
                 (mapcat #(let [pt (mapv + (last route) %)]
                            [(conj route pt)])
                         [[1 0] [-1 0] [0 1] [0 -1]])))
       (filter (fn [route]
                 (pred (last route))))
       set))

(defn trailhead-point2
  [m pt]
  (loop [v 0
         routes #{[pt]}]
    (if (seq routes)
      (if (= v 9)
        (count routes)
        (recur (inc v)
               (get-next-routes routes (get m (inc v)))))
      0)))


(defn q2
  [board]
  (let [m (get-all-pts board)]
    (->> (get m 0)
         (map #(trailhead-point2 m %))
         (apply +))))

#_(q2 in)
