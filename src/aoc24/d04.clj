(ns aoc24.d04
  (:require [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc24.utils :refer :all]))


(def ex
  (get-line-word
    "MMMSXXMASM
    MSAMXMSMSA
    AMXSXMAAMM
    MSAMASMSMX
    XMASAMXAMM
    XXAMMXXAMA
    SMSMSASXSS
    SAXAMASAAA
    MAMMMXMMMM
    MXMXAXMASX" "\\w"))
(def in (get-line-word "\\w"))

(def dirs (for [x [-1 0 1]
                y [-1 0 1]
                :when (not (= 0 x y))]
            [x y]))

(defn get-direction-word
  [board pt n dir]
  (loop [n n
         pt pt
         ret ""]
    (if (< n 1)
      ret
      (let [v (get-in board pt)]
        (if (nil? v)
          nil
          (recur (dec n)
                 (map + pt dir)
                 (str ret v)))))))

(defn get-directions-word
  [board pt n directions pred]
  (->> directions
       (keep #(let [r (get-direction-word board pt n %)]
                (when (pred r) r)))))

(defn q1 [board]
  (let [pred #(= "XMAS" %)]
    (->> (get-all-points board)
         (mapcat #(get-directions-word board % 4 dirs pred))
         count)))

#_(q1 in)

(def xpts [[[-1 -1] [0 0] [1 1]]
           [[1 1] [0 0] [-1 -1]]
           [[1 -1] [0 0] [-1 1]]
           [[-1 1] [0 0] [1 -1]]])

(defn check-x-word
  [board pt]
  (->> (keep #(get-point-word board pt %) xpts)
       (filter #(= "MAS" %))
       count (= 2)))


(defn q2 [board]
  (let [rows (count board)
        cols (count (first board))]
    (->> (mapcat #(mapv vector (repeat %1) %2)
                 (range rows)
                 (repeat (range cols)))
         (filter #(check-x-word board %))
         count)))

#_(q2 in)
