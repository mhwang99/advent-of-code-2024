(ns aoc24.d08
  (:require [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))


(def ex
  (get-line-chars
"............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............"))
(def in (get-line-chars))

(defn get-all-antenna
  [board]
  (let [rows (count board)
        cols (count (first board))]
    (reduce
      (fn [ret row]
        (reduce
          (fn [ret col]
            (let [v (get-in board [row col])]
              (if (not= \. v)
                (update ret v (fnil conj #{}) [row col])
                ret)))
          ret (range cols)))
      {} (range rows))))

(defn get-antinodes
  [board [pt1 pt2]]
  (let [an1 (mapv + pt2 (map - pt2 pt1))
        an2 (mapv + pt1 (map - pt1 pt2))]
    (reduce (fn [acc an]
              (if (get-in board an)
                (conj acc an) acc))
            #{} [an1 an2])))

(defn q1
  [board]
  (->> (vals (get-all-antenna board))
       (mapcat #(get-permutation % 2))
       (reduce (fn [acc comb]
                 (into acc (get-antinodes board comb)))
               #{})
       count))

#_(q1 in)

(defn get-next-pts
  [board pt1 pt2]
  (let [d (map - pt2 pt1)]
    (loop [pt pt2
           pts #{pt1 pt2}]
      (let [npt (map + pt d)]
        (if (nil? (get-in board npt))
          pts
          (recur npt (conj pts npt)))))))

(defn get-antinodes2
  [board [pt1 pt2]]
  (into (get-next-pts board pt1 pt2)
        (get-next-pts board pt2 pt1)))

(defn q2
  [board]
  (->> (vals (get-all-antenna board))
       (mapcat #(get-permutation % 2))
       (reduce (fn [acc comb]
                 (into acc (get-antinodes2 board comb)))
               #{})
       count))

#_(q2 in)


