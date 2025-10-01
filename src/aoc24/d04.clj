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

(def dirs (vec (for [x (range -1 2)
                     y (range -1 2)
                     :when (not (= x y 0))] [x y])))

(defn get-word
  ([board pt dir n] (get-word board pt dir n ""))
  ([board pt dir n ret]
   (cond
     (< n 1) ret
     (nil? (get-in board pt)) nil
     :else
     (get-word board
               (mapv + pt dir)
               dir
               (dec n)
               (str ret (get-in board pt))))))


(defn q1 [board]
  (->> (for [x (range (count board))
             y (range (count (first board)))]
         [x y])
       (mapcat (fn [pt]
                 (keep #(get-word board pt % 4 "") dirs)))
       (filter #(= "XMAS" %))
       count))

(def xpts [[[-1 -1] [0 0] [1 1]]
            [[1 1] [0 0] [-1 -1]]
            [[1 -1] [0 0] [-1 1]]
            [[-1 1] [0 0] [1 -1]]])

(defn get-pt-word
  [board pt map-pt]
  (let [cs (mapv #(get-in board (mapv + pt %)) map-pt)]
    (if (some nil? cs)
      nil
      (apply str cs))))

(defn check-x-word
  [board pt]
  (->> (keep #(get-pt-word board pt %) xpts)
       (filter #(= "MAS" %))
       count (= 2)))


(defn q2 [board]
  (let [pts (for [x (range (count board))
                  y (range (count (first board)))]
              [x y])]
    (count (filter #(check-x-word board %) pts))))

#_(q2 in)
